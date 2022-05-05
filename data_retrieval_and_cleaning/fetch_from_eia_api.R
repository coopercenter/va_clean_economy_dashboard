#Load necessary packages----------------------------------------------------------------------------------
lbry<-c("dplyr","stringr", "here",'httr','readr', "RPostgreSQL","tidyr",
         "readxl","data.table",'eia')
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)

#Load the database credentials----------------------------------------------------------------------------
#db_driver = dbDriver("PostgreSQL")
#source(here('my_postgres_credentials.R'))

# Fetching a large number of datasets
#all_data_series <-lapply(series_id_list,eia_series)

# loops through the all data series and stores the series in to a data.table
#
#Function takes a list of EIA time-series names and returns:
#   data: a data.table with a date column and a column for each item in the list
#   metaData: the non-data part of the EIA response: vectors of series info
#      including name, f(requency), units, description, etc. <- currently not used

#Set the key for the EIA API------------------------------------------------------------------------------
eiaKey <- '7ee3cdbf1ded6bcfb9de1e50d722ebd4'
eia_set_key(eiaKey)

#Create a function that will accept a list of series names and fetch them from the API--------------------
eiaSeriesGroup = function(series_id_list) {
  # Request one or multiple series in one call to eia::eia_series
  return_list = eia_series(series_id_list)
  # Loop through the responses
  name_list = list()
  for (i in 1:length(return_list$series_id)) {
    cat(return_list$series_id[i]," : ",return_list$name[i],"\n")
    # Extract the data into a data.table
    temp1 = data.table(return_list$data[[i]])
    temp1 = temp1[,.(date,value)]
    setnames(temp1,"value",str_replace_all(return_list$series_id[i], "[.-]", "_"))
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

# Return both monthly and annual versions of each monthly series
# The annual data is simply the sum of the monthly series by year

#Read in the list of series names we want to fetch from the API
series_id_vec <- read_file(here("data_retrieval_and_cleaning","series_ids.txt"))

#Transform the content to a list that we can later feed into the fetch function
series_id_list <- unlist(strsplit(series_id_vec,'\r\n')) #had to change from "\n" to "\r\n" on my system, reasons unclear

#Get separate lists for monthly and annual series
series_id_list.a  <-  series_id_list[grepl(".A$",series_id_list)]
series_id_list.m <-  series_id_list[grepl(".M$",series_id_list)]

# Fetch the monthly data---------------------------------------------------
response <- eiaSeriesGroup(series_id_list.m)
#Separate out the data from the API request results
eia_monthly_data <- response$data
#Separate out the metadata from the API request results
eia_monthly_metadata <- data.table(response$metaData)
setkey(eia_monthly_data,date)

#one time: store the series ids and names in an excel spreadsheet
# in the future, read this table either from a spreadsheet file or from the database
#eia_series_names <- response$metaData$name
#eiaSeriesInfo <- data.table(series_ids = monthly_list,
                           #series_names = eia_series_names)
# store series names
#library(xlsx)
#writexl::write_xlsx(eiaSeriesInfo, here("eiaSeriesInfo.xlsx"))


#Sum monthly data to annual data-----------------------------------------------------------------
# Annual data must not include incomplete years.

#grabbing the series names that made it to column names (not all monthly series names from the request list made it into the data table apparently. Why is that?)
cols <- names(eia_monthly_data)[names(eia_monthly_data)!= "date"]

#create a new column that just has the year, pulled from the 'date' column, used for summing the values by year
eia_monthly_data[,year := year(date)]

#Select the latest full year of data
#all the years with December values
latest_year <- eia_monthly_data %>% filter(month(date) == 12)
#latest year with a December value, the latest full year
latest_year <- as.numeric(max(latest_year$year))

#summarize the monthly data by year
annual_data.fromMonthly <- eia_monthly_data[year<=latest_year, lapply(.SD,sum), .SDcols = cols,by=year] %>% #groups all the columns of the monthly data by year, with the max year the latest year calculated in the previous step
  #rebuild the date column 
  mutate(date=as.Date(paste0(year,"-01-01"))) %>% 
  #remove the year column as we are done with it
  select(-year) %>% 
  #replace the M suffix for 'Monthly' with an A suffix for 'Annual'
  setnames(cols, str_replace_all(cols, "_M$", "_A"))

setkey(annual_data.fromMonthly,date)

#Tidy up the extra column now that we're done using it
eia_monthly_data <- eia_monthly_data %>% select(-year)

# Retrieve the annual series that are not available by month
response <- eiaSeriesGroup(series_id_list.a)
eia_annual_data <- response$data
eia_annual_metadata <- data.table(response$metaData)
setkey(eia_annual_data,date)

#  Merge all annual series
eia_annual_data <- merge(eia_annual_data,annual_data.fromMonthly, by="date",all=TRUE)
setkey(eia_annual_data,date)

# store both data tables in the database
#db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
# My postgres instance will only accept lower case column names
#   The tolower function can be removed if not needed in production
#
# annual
#
#names(eia_annual_data) = tolower(names(eia_annual_data))
dbWriteTable(db, "eia_annual_data", value = eia_annual_data, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbExecute(db, "alter table eia_annual_data add primary key (date);")
#
# monthly
#
#names(eia_monthly_data) = tolower(names(eia_monthly_data))
dbWriteTable(db, "eia_monthly_data", value = eia_monthly_data, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbExecute(db, "alter table eia_monthly_data add primary key (date);")

#test_output = dbGetQuery(db, "SELECT ELEC_SALES_VA_ALL_M from eia_monthly_data")
# Close connection
#dbDisconnect(db)
#dbUnloadDriver(db_driver)

#
#The following code is a start at extracting and storing metadata
#
#cols = names(annual_data)[names(annual_data)!= "date"]

#dbExecute(db, "COMMENT ON COLUMN annual_data.ELEC_SALES_VA_ALL_A IS 'All electricity sales';")

# comments.annual = list()
# for (i in 1:length(cols)) {
#   comments.annual[cols[i]] = paste0(series_id_list.a[i]," : ",response$metaData$name[i]," : ",response$metaData$units[i])
# }
# comments.annual = c(comments.annual,comments.annual.fromMonthly)
# for (i in 1:length(comment_names)) {
#   seriesName = comment_names[i]
#   comment = comments.annual[[i]]
#   cat(comment,"\n")
#   sql_stmt = paste0("COMMENT ON COLUMN annual_data.",seriesName," IS '",comment,"';")
#   dbExecute(db, sql_stmt)
#   
# }
# comments.monthly = list()
# for (i in 1:length(cols)) {
#   comments.monthly[cols[i]] = paste0(series_id_list.m[i]," : ",response$metaData$name[i]," : ",response$metaData$units[i])
# }
# comments.annual.fromMonthly = comments.monthly
# comment_names = names(comments.annual)
# comment_names = names(comments.monthly)
# for (i in 1:length(comment_names)) {
#   seriesName = comment_names[i]
#   comment = comments.monthly[[i]]
#   cat(i," : ", seriesName," : ",comment,  "\n")
#   sql_stmt = paste0("COMMENT ON COLUMN monthly_data.",seriesName," IS '",comment,"';")
#   print(sql_stmt)
#   dbExecute(db, sql_stmt)
#   
# }
# library(eia)
# eiaKey <- '7ee3cdbf1ded6bcfb9de1e50d722ebd4'
# eia_set_key(eiaKey)
# eia::eia_series_metadata("ELEC.GEN.COW-VA-99.M", cache = TRUE)$name
