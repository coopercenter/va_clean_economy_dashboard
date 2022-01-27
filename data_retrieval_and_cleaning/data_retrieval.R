lbry<-c("data.table", "RPostgreSQL",  "tidyr", "dplyr","arrow","stringr",
        "tools","lubridate", "Hmisc", "here", "readxl","read_xlsx")
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)
source(here("data_retrieval_and_cleaning","data_retrieval_functions.R"))
#
# Update EIA time series; saving to the db is done inside the sourced code
source(here("data_retrieval_and_cleaning","fetch_from_eia_api.R"))

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

# First, retrieve Virginia economic and demographic data from FRED
#    These could be stored in one table, but we'll leave it this way for now
va_pop = fetch_fred_series("VAPOP","1960-01-01") 
setnames(va_pop,2,"va_pop")
# The dashboard currently uses nominal GDP. This is incorrect, it should use real GDP
# va_nom_gsp = fetch_fred_series("VANGSP","1990-01-01") 
# setnames(va_nom_gsp,2,"va_ngsp")
# Here is the real GDP data
va_real_gsp = fetch_fred_series("VARGSP","1960-01-01") 
setnames(va_real_gsp,2,"va_rgsp")
va_state_info = merge(va_pop,va_real_gsp,by="date",all=TRUE)
dbRemoveTable(db,"va_state_info")
dbWriteTable(db,"va_state_info",va_state_info,append=F,row.names=F)
# Do not remove this dqta.table yet. It is used to calculate intensities.
#------------------------------------------------------#
#Next, update EIA_f826 data
# This function pulls EIA data and updates the postgres data tables:
#   virginia_eia_f826 with the raw data for Virginia and
#   va_monthly_utility_sales with clean data for dominion, apco and ros (rest of state)
updateEIA826Data(database="postgres")
# Now sum to annual data
  script = paste0("select * from virginia_monthly_utility_sales;")  
  virginia_utility_sales = data.table(dbGetQuery(db, script))
setkey(virginia_utility_sales,date)
#
virginia_utility_sales[,`:=`(total_sales_mwh = total_sales_mwh.apco + total_sales_mwh.dominion + total_sales_mwh.rest_of_state)]
# Create variables in gwh units (note we combine dom.com and dom.other)
monthly_utility_data = virginia_utility_sales[,.(date,year=year(date), month=month(date),
  va_total_sales_gwh = total_sales_mwh/1000,
  dom_com_gwh = (commercial_sales_mwh.dominion+other_sales_mwh.dominion)/1000,
  dom_res_gwh = residential_sales_mwh.dominion/1000,
  dom_ind_gwh = industrial_sales_mwh.dominion/1000,
  dom_total_gwh = total_sales_mwh.dominion/1000,
  ros_total_gwh = total_sales_mwh.rest_of_state/1000,
  apco_total_gwh = total_sales_mwh.apco/1000)]
# Find last full year
recent = monthly_utility_data[order(year,month),.(last_year=last(year),last_month=last(month))]
if (recent$last_month != 12) {
  latest.year = recent$last_year - 1
} else {
  latest.year = recent$last_year
} 
# Aggregate from monthly to annual data
annual_va_utility_data = monthly_utility_data[year<=latest.year,.(
  va_total_sales_gwh = sum(va_total_sales_gwh,na.rm=TRUE),
  apco_total_gwh = sum(apco_total_gwh,na.rm=TRUE),
  ros_total_gwh = sum(ros_total_gwh,na.rm=TRUE),
  dom_total_gwh = sum(dom_total_gwh,na.rm=TRUE),
  dom_com_gwh = sum(dom_com_gwh,na.rm=TRUE),
  dom_res_gwh = sum(dom_res_gwh,na.rm=TRUE),
  dom_ind_gwh = sum(dom_ind_gwh,na.rm=TRUE)
),by=year]
dbRemoveTable(db,"va_annual_utility_sales")
dbWriteTable(db,"va_annual_utility_sales",annual_va_utility_data,append=F,row.names=F)
rm(annual_va_utility_data)
#------------------------------------------------------#
# Energy and emission intensity 
# Beware: energy consumption versus electricity consumption. 
### Work out the proper scaling for appropriate units.
# Currently (2021) the dashboard displays energy per capita
# maybe this should be changed
#Energy intensity
intensity_data = merge(eia_annual_data[,.(date,Total_energy_cons,Total_CO2_emissions)],
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
#------------------------------------------------------#
#VCEA provisions
VCEA <- data.table(read_excel(here('raw_data','VCEA_goals.xlsx')))
# Multiply the two decimal percent columns by 100
cols = c("apco_energy_efficiency_as_share_of_2019_sales",
         "dominion_energy_efficiency_as_share_of_2019_sales")
VCEA[,(cols) := lapply(.SD,"*",100),.SDcols=cols]
#
dbRemoveTable(db,"vcea_provisions")
dbWriteTable(db,"vcea_provisions",VCEA,append=F,row.names=F)
rm(VCEA)
#------------------------------------------------------#
# Retrieve EIA860 survey data for the latest year
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

# You can filter for solar, wind or storage
va_solar = va_gen_plants[Prime_Mover=="PV"]
va_solar[,id:=paste0(Plant_Code,"_",Generator_ID)]
setkey(va_solar,id)


#------------------------------------------------------#
# Read in current estimates on offshore wind capacity and capacity factor
### The source of this data is not documented.
## This needs to be fixed.

offshore_wind <- data.table(read_xlsx(here('raw_data','offshore_wind_data.xlsx'),
                            sheet = "Offshore_wind_data",skip=1))
total_mw_offshore_wind = offshore_wind[,.(Year,Pilot_mw,Stage_1_mw,
                                          Stage_2_mw,Stage_3_mw,Total_mw)]
net_capacity_factor_offshore_wind <- offshore_wind[,.(Year,Pilot_cf,Stage_1_cf,
                                          Stage_2_cf,Stage_3_cf)]
total_production_forecast_offshore_wind <- offshore_wind[,.(Year,Stage_1_gen,
                                          Stage_2_gen,Stage_3_gen,Total_gen)]
setnames(total_production_forecast_offshore_wind,'Total_gen','Total_Production')

dbRemoveTable(db,"offshore_wind")
dbWriteTable(db,"offshore_wind",offshore_wind,append=F,row.names=F)
rm(offshor_wind)
#------------------------------------------------------#
# Retrieve latest EIA annual emission data for Virginia

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
file_name = here('raw_data','energy_efficiency_programs.xlsx')
current_EE_programs <- data.table(read_excel(file_name, col_names = TRUE))
rm(file_name)
# This next line does not appear to be used
# dominion_current_EE_data_through_2018 <- current_EE_programs[c(2,4:8),]
rm(current_EE_programs)

#upload to db
dbWriteTable(db, 'current_ee_programs', current_EE_programs, row.names=FALSE, overwrite = TRUE)

### Once again, the source of this data is undocumented. Its quality is unknown and suspect
### This part of the dashboard needs to be completely restructured.
#read in dataset
file_name = here('raw_data','virginia_annual_savings_through_2020.xlsx')
virginia_annual_savings_through_2020 <- data.table(read_excel(file_name, col_names = FALSE))
file_name = here('raw_data','virginia_annual_savings_through_2022.xlsx')
virginia_annual_savings_through_2022 <- data.table(read_excel(file_name, col_names = FALSE))
rm(file_name)
#replacing row names 
setnames(virginia_annual_savings_through_2020,c("Company Name", "MWh"))
setnames(virginia_annual_savings_through_2022,c("Company Name", "MWh"))

dbWriteTable(db, 'virginia_annual_savings_through_2020', virginia_annual_savings_through_2020, row.names=FALSE, overwrite = TRUE)
dbWriteTable(db, 'virginia_annual_savings_through_2022', virginia_annual_savings_through_2022, row.names=FALSE, overwrite = TRUE)
rm(virginia_annual_savings_through_2020,virginia_annual_savings_through_2022)

dbDisconnect(db)
