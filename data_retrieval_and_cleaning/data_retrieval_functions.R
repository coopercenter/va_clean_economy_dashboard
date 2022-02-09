

fetch_fred_series <- function(fred_series_id,start_date="1990-01-01") {
  #  start_date="1990-01-01"
  #fred_series_id = "VANGSP" 
  library(lubridate);
  library(fredr);library(data.table)
  #
  # Download the series 
  fredr_set_key("d7e23bfbe22e3a24b163719b064385f0")
  fred_data = data.table(fredr_series_observations(series_id = fred_series_id,
                                                   observation_start = as.Date(start_date))) 
  fred_data[,`:=`(series_id=NULL,realtime_start=NULL,realtime_end=NULL)]
  setkey(fred_data,"date")
  return(fred_data)
}

retrieve_statewide_sales <- function() {
  library(eia);library(data.table)
  # Set up base EIA api address with api_key
  eia_series_names=c("ELEC.SALES.VA-ALL.M",
                     "ELEC.SALES.VA-RES.M","ELEC.SALES.VA-COM.M",
                     "ELEC.SALES.VA-IND.M","ELEC.SALES.VA-TRA.M")
  local_names=c("date",
                "elec_sales_all_gwh",
                "elec_sales_res_gwh",
                "elec_sales_com_gwh",
                "elec_sales_ind_gwh",
                "elec_sales_tra_gwh")
  response = fetch_eia_series_group(eia_series_names)
  rd2 = data.table(response$data)
  setnames(rd2,local_names)
  
  # Process date field so that it can be used
  rd2$year = year(rd2$date)
  setkey(rd2,'date')
  return(rd2)
}

read_EIA_emission_data = function(fileName="raw_data/eia_emission_annual.xls") {
  local_file = here("raw_data/eia_emission_annual.xls")
  url = "https://www.eia.gov/electricity/data/state/emission_annual.xls"
  download.file(url,local_file,method="curl")
  nms <- names(read_excel(local_file, n_max = 0,skip=0))
  # Refashion column names into good variable names
  good_names = gsub("[\n]","_",nms)
  good_names = gsub("[?())]","",good_names)
  good_names = gsub("[[:punct:][:blank:]/]","_",good_names)
  column_types = c("numeric","text","text","text","numeric","numeric","numeric")
  # Read the data into a data.table
  thisData = data.table(read_excel(local_file,col_types=column_types,sheet=1))
  setnames(thisData,good_names)
  va_emission_data = thisData[State=='VA'][,State:=NULL]
  rm(thisData)
  return(va_emission_data)
}

# 
updateEIA826Data <- function(database="postgres") {
  library(readxl);library(lubridate); library(data.table)
  library(RMySQL);library(stringr); library(arrow)
  library(RPostgres)
  database="postgres"; table="eia_f826_data"
  # db_driver = dbDriver("MySQL")
  # CCPS_DATABASE_PWD = 'manx(0)Rose'
  # CCPS_DATABASE_ACCT = 'wms5f'
  # db_host = '52.44.235.34'
  # database = "energy"
  
  # Find the dates with data labeled as "Preliminary"
  this_year = 2021 #year(now())
  start_year = this_year - 2
  update_years <- seq(start_year,this_year)
  db_driver = dbDriver("PostgreSQL")
  source(here::here("my_postgres_credentials.R"))
  db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname=database, host=db_host)
    script  <- paste0("select year, month from ",table," where year>=",start_year,";")
    all_f826_dates = data.table(dbGetQuery(db, script))
  dbDisconnect(db)
  # update_years = unique(all_f826_data$year)
  print('Update EIA826/861 data')
  print(update_years)
  all_f826_dates[,date:=as.Date(paste(year,month,"01",sep="-"))]
  last_f826_date = all_f826_dates[,max(date)]
  print(paste0('last 861 date:  ',last_f826_date))
  rm(all_f826_dates)
  #
  #length(update_years)
  for (year in update_years) {
    # In 2017, EIA changed the way the file are stored
    year = 2020
    if(year>=2017) {
      if(year == this_year) {
        path = "https://www.eia.gov/electricity/data/eia861m/xls/"
      } else {
        path = "https://www.eia.gov/electricity/data/eia861m/archive/xls/"
      }
        file_name = paste0("retail_sales_",year,".xlsx")  
    # } else {
    #   path = "https://www.eia.gov/electricity/data/eia861m/archive/xls/"
    #   year <= 2016
    #   file_name = paste0("f826",year,".xls")   
    }
    url = paste0(path,file_name) #
    local_file = paste0(file_name)   #local_directory,
    cat(local_file)
    download.file(url,local_file,method="curl")
    #      cat(file.size(local_file))
    if(file.size(local_file)!=0) {
      skip_rows <- 3
      column_classes = c("numeric","numeric","numeric","text","text","text","text",
                         "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                         "numeric","numeric","numeric","numeric","numeric","numeric","numeric")
      names = c("year","month","utilityid","utility_name","state","ownership","data_status",
                "res_rev","res_sales_mwh","res_cust","com_rev","com_sales_mwh","com_cust",
                "ind_rev","ind_sales_mwh","ind_cust","oth_rev","oth_sales_mwh","oth_cust",
                "tot_rev","tot_sales_mwh","tot_cust")
      this_data = data.table(read_excel(local_file,1,skip = 3,col_names=names,col_types = column_classes))
      # Update database with EIA data for all states
      this_data = this_data[!is.na(year) & state=="VA"][!is.nan(year)]
      cat("Updating EIA 826 data.\n")
      db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
        script  <- paste0("delete from ",table," where year=",year,";")
        temp = data.table(dbGetQuery(db, script))
        dbWriteTable(db,"virginia_eia_f826",this_data,append=TRUE,row.names=F)
      dbDisconnect(db)
    } 
  }
  #
  # Process data from EIA form 826. This will not have the same start and end dates as the other sales data
  # This code needs frequent maintenance due to changes in the underlying EIA data
  # Especially check utility_name. Names change quite frequently
  #
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
  # Sales data from EIA survey. Note: does not include sales for resale!!
  script = paste0("select * from eia_f826_data where state = 'VA';")  
  virginia_utility_sales = data.table(dbGetQuery(db, script))
dbDisconnect(db)
  virginia_utility_sales[,date := as.Date(paste(year,month,'01',sep='-'))]
  setorder(virginia_utility_sales,date)
  setnames(virginia_utility_sales,"utilityid","utility_id")
  virginia_utility_sales[,utility_name := gsub(" ","_",utility_name)]
  virginia_utility_sales = virginia_utility_sales[utility_name!="Total_EPM"]
  virginia_utility_sales[,utility_name := gsub("Craig-Botetourt","Craig_Botetourt",utility_name)]
  virginia_utility_sales[,utility_name := gsub("A_&_N","A&N",utility_name)]
  virginia_utility_sales[str_detect(utility_name, 'Adjustment'),utility_name:="Other"]
  virginia_utility_sales[str_detect(utility_name, 'Appalachian'),utility_name:="APCO"]
  virginia_utility_sales[str_detect(utility_name, 'Kentucky'),utility_name:="Kentucky_Utilities"]
  virginia_utility_sales[str_detect(utility_name, 'Potomac'),utility_name:="Potomac_Edison"]
  virginia_utility_sales[str_detect(utility_name, 'Conectiv'),utility_name:="A&N_Electric_Coop"]
  virginia_utility_sales[str_detect(utility_name, 'Virginia_Electric'),utility_name:="Dominion"]
 
  virginia_utility_sales[,utility_name_orig:=utility_name]
  virginia_utility_sales[str_detect(utility_name, 'APCO'),utility_name:="apco"]
  virginia_utility_sales[str_detect(utility_name, 'Dominion'),utility_name:="dominion"]
  virginia_utility_sales[utility_name!="dominion" & utility_name!="apco",utility_name:="rest_of_state"]
  monthly_utility_sales = virginia_utility_sales[,.(total_sales_mwh = sum(tot_sales_mwh),
                                            residential_sales_mwh = sum(res_sales_mwh),residential_cust=sum(as.integer(res_cust),na.rm = TRUE),
                                            commercial_sales_mwh = sum(com_sales_mwh),commercial_cust=sum(as.integer(com_cust),na.rm = TRUE),
                                            industrial_sales_mwh = sum(ind_sales_mwh),industrial_cust=sum(as.integer(ind_cust),na.rm = TRUE),
                                            other_sales_mwh = sum(oth_sales_mwh),other_cust=sum(as.integer(oth_cust),na.rm = TRUE)     
  ),by=list(date, utility_name)]
  #monthly_utility_sales[,avg_daily_total_sales_mwh := total_sales_mwh/days_in_month]
  db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
    dbRemoveTable(db,"virginia_utility_sales_long")
    dbWriteTable(db,"virginia_utility_sales_long",monthly_utility_sales,append=F,row.names=F)
  dbDisconnect(db)

  # DAO_data_long = virginia_utility_sales[,.(total_sales_mwh = sum(tot_sales_mwh) , days_in_month = days_in_month(date), month = month(date),
  #                                  residential_sales_mwh = sum(res_sales_mwh),residential_cust=sum(as.integer(res_cust),na.rm = TRUE),
  #                                  commercial_sales_mwh = sum(com_sales_mwh),commercial_cust=sum(as.integer(com_cust),na.rm = TRUE),
  #                                  industrial_sales_mwh = sum(ind_sales_mwh),industrial_cust=sum(as.integer(ind_cust),na.rm = TRUE),
  #                                  other_sales_mwh = sum(oth_sales_mwh),other_cust=sum(as.integer(oth_cust),na.rm = TRUE)
  # ),by=list(date, utility_name)]
  DAO_data_monthly = reshape(monthly_utility_sales, idvar='date', timevar='utility_name', direction='wide')
  DAO_data_monthly[,Year := year(date)]
  db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
    dbRemoveTable(db,"virginia_monthly_utility_sales")
    dbWriteTable(db,"virginia_monthly_utility_sales",DAO_data_monthly,append=F,row.names=F)
  dbDisconnect(db)
  
#  arrow::write_feather(virginia_utility_sales,paste0("virginia_utility_sales_long.ftr"))
#  arrow::write_feather(DAO_data_monthly,paste0("virginia_monthly_utility_sales.ftr"))

} # End function updateEIA826Data


last_eia_data_date <- function() {
  vv_base = "http://api.eia.gov/series/?api_key=0E59CFF12754E0513DEB30FB4850B0FA&series_id="
  # construct full EIA api address
  seriesName = "ELEC.SALES.VA-ALL.M"
  vv = paste0(vv_base,seriesName)
  # This line reads the JSON object from the EIA site and parses it into a list
  rd <- fromJSON(readLines(vv, warn = "F"))
  return(rd$series$end)
}

fetch_eia_series_group = function(series_id_list) {
  library(eia);library(data.table)
  # Request one or multiple series in one call to eia::eia_series
  # eiaKey <- '7ee3cdbf1ded6bcfb9de1e50d722ebd4'
  # eia_set_key(eiaKey)
  return_list = eia::eia_series(series_id_list)
  # Loop through the responses
  name_list = list()
  for (i in 1:length(return_list$series_id)) {
    cat(return_list$series_id[i]," : ",return_list$name[i],"\n")
    # Extract the data into a data.table
    temp1 = data.table(return_list$data[[i]])
    temp1 = temp1[,.(date,value)]
    setnames(temp1,"value",gsub( "[.-]", "_",return_list$series_id[i]))
    # For the very first series, create a new data.table instance.
    #   For all others, merge by date into the existing table
    if (i==1) {
      temp2 = temp1
    } else {
      temp2 = merge(temp2,temp1, by="date", all=TRUE )
    }
  }
  # Return data and metadata in a list
  return_list["data"] <- NULL
  return(list(data=temp2,metaData = return_list))
}

readEIA860sheet <- function(year,local_directory,
                            xlsx_file="3_1_Generator") {
  # Downoads EIA zip file, unzips it to local_directory,
  #     then extracts the named sheet. Column labels are cleaned up
  #     into valid variable names
  lbry<-c("data.table", "RPostgreSQL",  "tidyr", "dplyr","arrow",
          "tools","lubridate", "Hmisc", "here", "readxl","stringr")
  test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
  rm(test,lbry)
#  year = 2020
#  local_directory = here("raw_data")
  EIA_path = "https://www.eia.gov/electricity/data/eia860/xls/"
  zip_file_name = paste0("eia860",year,".zip")
  
  url = paste0(EIA_path,zip_file_name) 
  local_file = paste0(local_directory,zip_file_name)
  cat(paste0(local_file,"\n"))
  download.file(url,local_file,method="curl")
  unzip(local_file,exdir=local_directory)
#  xlsx_file="3_1_Generator"
  sheet="Operable"
  file_name = paste0(local_directory,"/",xlsx_file,"_Y",year,".xlsx")
  nms <- names(read_excel(file_name, sheet, n_max = 0,skip=1))
  good_names = gsub("[?())]","",nms)
  good_names = gsub("[[:punct:][:blank:]/]","_",good_names)
  allStatesData = data.table(read_excel(file_name, sheet,skip=2,col_names=good_names))
  va_gen_capacity = allStatesData[State=="VA",][,State:=NULL,]
  va_gen_capacity[,RTO_ISO_Location_Designation:=RTO_ISO_Location_Designation_for_Reporting_Wholesale_Sales_Data_to_FERC]
  va_gen_capacity[,id:=paste0(Plant_Code,"_",Generator_ID)]
  setkey(id)
  return(va_gen_capacity)
}

read_SEDS_emission_data <- function(local_file = "eia_emission_by_fuel.xlsx") {
  ### Divide short tons by 1.102 to get metric tons.
  local_file = here("raw_data/eia_emission_by_fuel.xlsx")
  url = "https://www.eia.gov/electricity/state/Virginia/xls/va.xlsx"
  download.file(url,local_file,method="curl")
  raw_table = data.table(t(read_excel(local_file, skip=1,sheet="7. Emissions")))
  nms <- raw_table[1,1:ncol(raw_table)]
  raw_table = raw_table[-1]
  good_names = gsub("[\n]","_",nms)
  good_names = gsub("[?())]","",good_names)
  good_names = gsub("[[:punct:][:blank:]/]","_",good_names)
  good_names[1] = "Year"
  # SO2 vars are in 3:7, NOx in 9:13, CO2 in 15:19, rate data in 21:23
  good_names[3:7] = paste0("SO2_",good_names[3:7])
  good_names[9:13] = paste0("NOx_",good_names[9:13])
  good_names[15:19] = paste0("CO2_",good_names[15:19])
  good_names[21:23] = paste0(good_names[21:23],"_rate")
  setnames(raw_table,good_names)
  raw_table[,20:=NULL];raw_table[,14:=NULL];raw_table[,8:=NULL];raw_table[,2:=NULL]
  last_col_to_keep = length(raw_table)-1
  emissions_table <- raw_table[,1:last_col_to_keep]
  emissions_table[,Year := gsub("[\n\r]","",Year)]
  emissions_table[,Year := as.integer(str_replace_all(Year,'Year',''))]
  last_col = ncol(emissions_table)
  cols = 2:last_col
  # Convert from character to number
  emissions_table[,(cols) := lapply(.SD, as.numeric),.SDcols=cols]
  # Convert from short tons to metric tons
  # CO2 is already in thousands of metric tons
  convert <- function(obs) {obs/1.102}
  cols = c(2:11)   # Only SO2 and NOx are in short tons
  emissions_table[,(cols) := lapply(.SD, convert),.SDcols=cols]
  setkey(emissions_table,"Year")
  return(emissions_table)
}
