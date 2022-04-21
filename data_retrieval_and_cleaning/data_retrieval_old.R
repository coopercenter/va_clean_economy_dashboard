#things I want to update about this document:
#Too long, hard to find the right data to update. Contains many different functions at once, it makes them hard to track. 
#Things are partially covered in other scripts, and the rest are covered in this script. An incompleteness and inconsistency to it.
#Not enough documentation on what data sources are used for what visualizations
#Plan: Have separate files for the code for updating each data source
#turn the data retrieval process into a terminal for sourcing the different code files as the new way to update the code


#data sources loaded in this file and sourced files: 
#EnergyCAP
#EIA (EIA, EIA F826, EIA F860, SEDS)
#FRED
#VE mandate data
#COVA Facility Tracker spreadsheet

#Load necessary packages----------------------------------------------------------------------------------
lbry<-c("data.table", "RPostgreSQL",  "tidyr", "dplyr","arrow","stringr",
        "tools","lubridate", "Hmisc", "here", "readxl","read_xlsx",'httr','jsonlite')
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)

#Connect to the database--------------------------------------------------------------------------------
db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

#Source data retrieval functions-------------------------------------------------------------------------
#A lot of data retrieval functionality is in these data retrieval functions
source(here("data_retrieval_and_cleaning","data_retrieval_functions.R"))

#Source the main EIA data via a separate file------------------------------------------------------------
# Update EIA time series; saving to the db is done inside the sourced code
source(here("data_retrieval_and_cleaning","fetch_from_eia_api.R"))

# Retrieve Virginia economic and demographic data from FRED--------------------------------------------
#source('FRED_series_retrieval.R')
#    These could be stored in one table, but we'll leave it this way for now
va_pop = fetch_fred_series("VAPOP","1960-01-01") 
setnames(va_pop,2,"va_pop")

#This bit of commented code is obsolete and can probably be deleted now since its solution is right below
# The dashboard currently uses nominal GDP. This is incorrect, it should use real GDP
# va_nom_gsp = fetch_fred_series("VANGSP","1990-01-01") 
# setnames(va_nom_gsp,2,"va_ngsp")

# Here is the real GDP data
va_real_gsp = fetch_fred_series("VARGSP","1960-01-01") 
setnames(va_real_gsp,2,"va_rgsp")
va_state_info = merge(va_pop,va_real_gsp,by="date",all=TRUE)
dbRemoveTable(db,"va_state_info")
dbWriteTable(db,"va_state_info",va_state_info,append=F,row.names=F)
# Do not remove this data table yet. It is used to calculate intensities.

#Update EIA f826 data----------------------------------------------------------------------------------
#source('EIAF826_data_retrieval.R')

# This function pulls EIA data and updates the postgres data tables:
#   virginia_eia_f826 with the raw data for Virginia and
#   va_monthly_utility_sales with clean data for dominion, apco and ros (rest of state)
# This data is not currently used in the dashboard
#updateEIA826Data(database="postgres")
# Now sum to annual data
#script = paste0("select * from virginia_monthly_utility_sales;")  
#virginia_utility_sales = data.table(dbGetQuery(db, script))
#setkey(virginia_utility_sales,date)
#
#virginia_utility_sales[,`:=`(total_sales_mwh = total_sales_mwh.apco + total_sales_mwh.dominion + total_sales_mwh.rest_of_state)]
# Create variables in gwh units (note we combine dom.com and dom.other)
#monthly_utility_data = virginia_utility_sales[,.(date,year=year(date), month=month(date),
#  va_total_sales_gwh = total_sales_mwh/1000,
#  dom_com_gwh = (commercial_sales_mwh.dominion+other_sales_mwh.dominion)/1000,
#  dom_res_gwh = residential_sales_mwh.dominion/1000,
#  dom_ind_gwh = industrial_sales_mwh.dominion/1000,
#  dom_total_gwh = total_sales_mwh.dominion/1000,
#  ros_total_gwh = total_sales_mwh.rest_of_state/1000,
#  apco_total_gwh = total_sales_mwh.apco/1000)]
# Find last full year
#recent = monthly_utility_data[order(year,month),.(last_year=last(year),last_month=last(month))]
#if (recent$last_month != 12) {
#  latest.year = recent$last_year - 1
#} else {
#  latest.year = recent$last_year
#} 
# Aggregate from monthly to annual data
#annual_va_utility_data = monthly_utility_data[year<=latest.year,.(
#  va_total_sales_gwh = sum(va_total_sales_gwh,na.rm=TRUE),
#  apco_total_gwh = sum(apco_total_gwh,na.rm=TRUE),
#  ros_total_gwh = sum(ros_total_gwh,na.rm=TRUE),
#  dom_total_gwh = sum(dom_total_gwh,na.rm=TRUE),
#  dom_com_gwh = sum(dom_com_gwh,na.rm=TRUE),
#  dom_res_gwh = sum(dom_res_gwh,na.rm=TRUE),
#  dom_ind_gwh = sum(dom_ind_gwh,na.rm=TRUE)
#),by=year]
#dbRemoveTable(db,"va_annual_utility_sales")
#dbWriteTable(db,"va_annual_utility_sales",annual_va_utility_data,append=F,row.names=F)
#rm(annual_va_utility_data)

#Calculate energy and emission intensity--------------------------------------------------------------
# Energy and emission intensity 
# Beware: energy consumption versus electricity consumption. 
### Work out the proper scaling for appropriate units.
# Currently (2021) the dashboard displays energy per capita
# maybe this should be changed
#Energy intensity
#
# Map local names to EIA data series (copied from dashboard_calculations.R because the local names are referenced here but weren't previously set except by running the calculations first)
#
eia_name=c("ELEC_GEN_COW_VA_99_A",
           "ELEC_GEN_PEL_VA_99_A",
           "ELEC_GEN_NG_VA_99_A",
           "ELEC_GEN_NUC_VA_99_A",
           "ELEC_GEN_SUN_VA_99_A",
           "ELEC_GEN_DPV_VA_99_A",
           "ELEC_GEN_HYC_VA_99_A",
           "ELEC_GEN_HPS_VA_99_A",
           "ELEC_GEN_WND_VA_99_A",
           "ELEC_GEN_WWW_VA_99_A",
           "ELEC_GEN_WAS_VA_99_A",
           "ELEC_GEN_ALL_VA_99_A",
           "SEDS_TETCB_VA_A",
           "SEDS_TERCB_VA_A",
           "SEDS_TECCB_VA_A",
           "SEDS_TEICB_VA_A",
           "SEDS_TEACB_VA_A",
           "SEDS_ELISP_VA_A",
           "EMISS_CO2_TOTV_EC_TO_VA_A",
           "EMISS_CO2_TOTV_TT_TO_VA_A")
local_name=c("Coal",
             "Oil",
             "Gas",
             "Nuclear",
             "Solar_utility", 
             "Solar_distributed",
             "Hydropower",
             "Pumped_storage",
             "Wind",
             "Wood",
             "Other_biomass",
             "Total_gen",
             "Total_energy_cons",
             "Residential",
             "Commercial",
             "Industrial",
             "Transportation",
             "Imported_electricity",
             "Electric_sector_CO2_emissions",
             "Total_CO2_emissions")

setnames(eia_annual_data,
         eia_name, local_name)


intensity_data = merge(eia_annual_data[Total_energy_cons!=0 & Total_CO2_emissions != 0,
                            .(date,Total_energy_cons,Total_CO2_emissions)],
                       va_state_info,by="date",all=TRUE)
intensity_data[,energy_consumption_per_capita := Total_energy_cons/va_pop]

#### Need to work out the units to report
intensity_data[!is.na(va_rgsp),
               energy_consumption_per_gdp := Total_energy_cons*1000/va_rgsp]
# Emission intensity 
intensity_data[,co2_per_capita := Total_CO2_emissions/va_pop*1000]
intensity_data[,co2_per_gdp := Total_CO2_emissions*1000000/va_rgsp]
intensity_data = intensity_data[!is.na(Total_energy_cons)] 
dbRemoveTable(db,"intensity_data")
dbWriteTable(db,"intensity_data",intensity_data,append=F,row.names=F)
rm(intensity_data)


#Read in VCEA provisions from Excel sheet-------------------------------------------------------------------------
VCEA <- data.table(read_excel(here('raw_data','VCEA_goals.xlsx')))
# Multiply the two decimal percent columns by 100
cols = c("apco_energy_efficiency_as_share_of_2019_sales",
         "dominion_energy_efficiency_as_share_of_2019_sales")
VCEA[,(cols) := lapply(.SD,"*",100),.SDcols=cols]
#
dbRemoveTable(db,"vcea_provisions")
dbWriteTable(db,"vcea_provisions",VCEA,append=F,row.names=F)
rm(VCEA)

#Retrieve EIA860 survey data for the latest year-----------------------------------------------------------------
#    EIA posts this data in around June of the subsequent year
# Then filter for solar (PV) and wind (WT)  and storage (BA)
#    Virginia doesn't have wind or storage up to 2020
#
local_directory = here("raw_data")
# Bypass this test, if you are sure that new data is available
if (month(now())<7) {
  year = year(now())-2
} else {
  year = year(now())-1
}
# This function call returns all Virginia active generation units
va_gen_plants = readEIA860sheet(year,local_directory,xlsx_file="3_1_Generator")
dbRemoveTable(db,"eia_plant_capacities")
dbWriteTable(db,"eia_plant_capacities",va_gen_plants,append=F,row.names=F)

# You can filter for solar, wind or storage (why is this done here, but not saved to the DB? Just gets lost.)
va_solar = va_gen_plants[Prime_Mover=="PV"]
va_solar[,id:=paste0(Plant_Code,"_",Generator_ID)]
setkey(va_solar,id)



#Read in current estimates on offshore wind capacity and capacity factor from spreadsheet-------------------------------------------
### The source of this data is not documented.
## This needs to be fixed.

offshore_wind <- data.table(read_xlsx(here('raw_data','offshore_wind_data.xlsx'),
                            sheet = "Offshore_wind_data",skip=1))
total_mw_offshore_wind = offshore_wind[,.(Year,CVOW_Pilot,CVOW_Stage_I,
                                          CVOW_Stage_II,CVOW_Stage_III,Total_mw)]
net_capacity_factor_offshore_wind <- offshore_wind[,.(Year,Pilot_cf,Stage_1_cf,
                                          Stage_2_cf,Stage_3_cf)]
total_production_forecast_offshore_wind <- offshore_wind[,.(Year,Stage_1_gen,
                                          Stage_2_gen,Stage_3_gen,Total_gen)]
setnames(total_production_forecast_offshore_wind,'Total_gen','Total_Production')

dbRemoveTable(db,"offshore_wind")
dbWriteTable(db,"offshore_wind",offshore_wind,append=F,row.names=F)
rm(offshore_wind)

#Retrieve latest EIA annual emission data for Virginia downloaded from SEDS----------------------------------------------------------------------

#Why is this read from an Excel sheet THEN written to the database? Should just write it directly to the database on sourcing. 
#Where is this spreadsheet orginally sourced from? What function?

# I'm pretty sure that this data is redundant. The same data, but with more detail
#                        is in electricity_emissions_by_fuel
# va_emission_data = read_EIA_emission_data(fileName="raw_data/eia_emission_annual.xls")
# # Limit observations to the relevant total amounts
# #    Producer_Type=="Total Electric Power Industry" & Energy_Source=="All Sources"
# va_emission_data = va_emission_data[Producer_Type=="Total Electric Power Industry" &
#                                       Energy_Source=="All Sources"][,
#                                     `:=`(Producer_Type=NULL,Energy_Source=NULL)]
# dbRemoveTable(db,"va_annual_emissions")
# dbWriteTable(db,"va_annual_emissions",va_emission_data,append=F,row.names=F)

# Now for SEDS data on electricity sector emissions by fuel source
# This data.table has SO2,NOx and CO2 emissions and emission rates by pollutant
electricity_emissions_by_fuel = read_SEDS_emission_data(local_file = here("raw_data/eia_emission_by_fuel.xlsx"))

dbRemoveTable(db,"va_electricity_emissions_by_fuel")
dbWriteTable(db,"va_electricity_emissions_by_fuel",electricity_emissions_by_fuel,append=F,row.names=F)
rm(electricity_emissions_by_fuel)
#------------------------------------------------------#
# Energy efficiency

### This data is incomplete and unhelpful
### The efficiency material needs to be completely overhauled
### The origins of this spreadsheet are not documented and the data is 
###    pertty worthless
#read in dataset
#file_name = here('raw_data','energy_efficiency_programs.xlsx')
#current_EE_programs <- data.table(read_excel(file_name, col_names = TRUE))
#rm(file_name)
# This next line does not appear to be used
# dominion_current_EE_data_through_2018 <- current_EE_programs[c(2,4:8),]

#upload to db
#dbWriteTable(db, 'current_ee_programs', current_EE_programs, row.names=FALSE, overwrite = TRUE)
#rm(current_EE_programs)

### Once again, the source of this data is undocumented. Its quality is unknown and suspect
### This part of the dashboard needs to be completely restructured.
#read in dataset
#file_name = here('raw_data','virginia_annual_savings_through_2020.xlsx')
#virginia_annual_savings_through_2020 <- data.table(read_excel(file_name, col_names = FALSE))
#file_name = here('raw_data','virginia_annual_savings_through_2022.xlsx')
#virginia_annual_savings_through_2022 <- data.table(read_excel(file_name, col_names = FALSE))
#rm(file_name)
#replacing row names 
#setnames(virginia_annual_savings_through_2020,c("Company Name", "MWh"))
#setnames(virginia_annual_savings_through_2022,c("Company Name", "MWh"))

#dbWriteTable(db, 'virginia_annual_savings_through_2020', virginia_annual_savings_through_2020, row.names=FALSE, overwrite = TRUE)
#dbWriteTable(db, 'virginia_annual_savings_through_2022', virginia_annual_savings_through_2022, row.names=FALSE, overwrite = TRUE)
#rm(virginia_annual_savings_through_2020,virginia_annual_savings_through_2022)


#Retrieving the EnergyCAP data for the Energy Efficiency page---------------------------------------------------------------------

#get the key
key <- source(here('data_retrieval_and_cleaning/EnergyCAP_API_key.R'))

#make the API requests and convert them into dataframes

#get the first page of the place data so we can use the headers to tell us how many more pages we need
energyCAP_building_data <- GET("https://app.energycap.com/api/v3/place",
                               add_headers(.headers=c('ECI-ApiKey'=energycap_key)))

#store the number of pages before flattening to a JSON
pages <- as.numeric(energyCAP_building_data$headers$totalpages)

#flatten the request results
energyCAP_building_data <- energyCAP_building_data %>% 
  content("text") %>% 
  fromJSON(flatten=TRUE) %>%
  as.data.frame() %>% 
  unnest(cols=c(meters))

#use a for-loop to go through the places page by page to capture all the building data
for(i in (2:pages)){
  next_page <- GET(paste("https://app.energycap.com/api/v3/place?pageNumber=",i,sep=""), 
                   add_headers(.headers=c('ECI-ApiKey'=energycap_key))) %>% 
    content("text") %>% 
    fromJSON(flatten=TRUE) %>% 
    as.data.frame() %>% 
    unnest(cols=c(meters))
  energyCAP_building_data <- rbind(energyCAP_building_data,next_page)
  
}

#create a function to process the API results for a given query
fetch_from_energyCAP <- function(query, unnest_col){
  GET(query,add_headers(.headers=c('ECI-ApiKey'=energycap_key))) %>% 
    content("text") %>% 
    fromJSON(flatten=TRUE) %>% 
    as.data.frame() %>% 
    unnest(cols=all_of(unnest_col))
}

#get the meter data which has use and cost, convert to dataframe then unnest the deeper columns
meter_query <- "https://app.energycap.com/api/v3/meter/digest/actual/yearly"
energyCAP_meter_data <- fetch_from_energyCAP(meter_query,'results')

#get the savings data
savings_query <- "https://app.energycap.com/api/v3/meter/digest/savings/yearly"
energyCAP_savings_data <- fetch_from_energyCAP(savings_query,'results')

#energyCAP_meter_data <- GET("https://app.energycap.com/api/v3/meter/digest/actual/yearly", 
 #                           add_headers(.headers=c('ECI-ApiKey'=energycap_key)))  %>% 
  #content("text") %>% 
  #fromJSON(flatten=TRUE) %>%
  #as.data.frame() %>% 
  #unnest(cols=results)

#energyCAP_savings_data <- GET("https://app.energycap.com/api/v3/meter/digest/savings/yearly", 
 #                             add_headers(.headers=c('ECI-ApiKey'=energycap_key))) %>% 
  #content("text") %>% 
  #fromJSON(flatten=TRUE) %>%
  #as.data.frame() %>% 
  #unnest(cols=results)

#remove the excess variables
rm(energycap_key,key)

#join the meter dataframe with the building dataframe on meterId
energyCAP_data <- full_join(energyCAP_building_data,energyCAP_meter_data,by='meterId')

#join the meter and building data with the savings data on meterId and year
energyCAP_data <- full_join(energyCAP_data,energyCAP_savings_data,by=c('meterId','year'))

#pare the data down to just the important parts to explore for visualizations
columns_to_keep <- c('placeId','parent.placeInfo','placeInfo',
                     'primaryUse.primaryUseInfo',
                     'size.value','year','totalCost.x','commonUse.x','savingsCommonUse')

#filter by electric meter
energyCAP_data <- energyCAP_data %>% filter(commodity.commodityCode=='ELECTRIC') %>% 
  #filter by size
  filter(as.numeric(size.value) >= 5000) %>%
  #pare down to just the columns we want
  select(all_of(columns_to_keep)) %>%
  #clear up any duplicates that may have come from merging
  distinct() %>%
  #rename a couple of columns
  rename(commonUse=commonUse.x) %>% 
  rename(totalCost=totalCost.x)

rm(energyCAP_building_data,energyCAP_meter_data,energyCAP_savings_data,columns_to_keep)


#write to the database
dbRemoveTable(db,"energycap_place_meter_and_savings_data")
dbWriteTable(db,"energycap_place_meter_and_savings_data",energyCAP_data)

#Loading the new energy efficiency spending mandates into the database from their CSV files------------------------------------------
#this will be updated once their structure is finalized and we have a more formalized data sharing setup with Virginia Energy

ee_resource_standard_projections <- read.csv(here('raw_data/energy_efficiency_resource_standard_projections.csv'))
ee_spending_progress <- read.csv(here('raw_data/energy_efficiency_spending_progress.csv'))
ee_spending_requirements <- read.csv(here('raw_data/energy_efficiency_spending_requirements.csv')) 

dbWriteTable(db,'energy_efficiency_resource_standard_projections',ee_resource_standard_projections)
dbWriteTable(db,'energy_efficiency_spending_progress', ee_spending_progress)
dbWriteTable(db,'energy_efficiency_spending_requirements',ee_spending_requirements)

#Writing the COVA facility tracker sheet from VE into the database----------------------------------------------------------------
#hopefully a more streamlined process soon
agency_facility_tracking <- read.csv(here('raw_data/COVA_Facility_Tracker_Simplified.csv'))

dbWriteTable(db,'agency_facility_tracking',agency_facility_tracking)

#Disconnect from the database and finish----------------------------------------------------------------------------------------
dbDisconnect(db)
