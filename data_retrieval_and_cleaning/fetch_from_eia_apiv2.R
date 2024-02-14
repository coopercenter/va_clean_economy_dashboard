library("data.table")
library("RPostgreSQL")
library("tidyr")
library("dplyr")
library("arrow")
library("stringr")
library("tools")
library("lubridate")
library("Hmisc")
library("here")
library("readxl")
#library("read_xlsx")
library('httr')
library('jsonlite')
library('readr')
library('eia')

#read in the API key--------------------------------------------------------------------------
key <- source(here('data_retrieval_and_cleaning/my_eia_api_key.R'))

eia_key = key$value
#load the URLS for the API calls-------------------------------------
data_urls <- source(here('data_retrieval_and_cleaning/eia_apiv2_urls.R'))

#fetch the data, clean up the dataframes
plant_data_monthly = GET(data_urls$value[['plant_data_monthly']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE)%>%
  as.data.frame %>%
  select(c("date"=response.data.period,
           "plant_code"=response.data.plantCode,
           "plant_name"=response.data.plantName,
           "generation_fuel"=response.data.fuel2002,
           "generation_fuel_description"=response.data.fuelTypeDescription,
           "prime_mover"=response.data.primeMover,
           "generation_fuel_consuption"=response.data.consumption.for.eg.btu,
           "electricity_generation"=response.data.generation,
           "consumption_units"=response.data.consumption.for.eg.btu.units,
           "generation_units"=response.data.generation.units,
           "series_description"=response.description,
           "data_description"=request.params.data)) %>%
  as.data.table %>%
  setkey('date')

plant_data_annual = GET(data_urls$value[['plant_data_annual']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE)%>%
  as.data.frame %>%
  select(c("date"=response.data.period,
           "plant_code"=response.data.plantCode,
           "plant_name"=response.data.plantName,
           "generation_fuel"=response.data.fuel2002,
           "generation_fuel_description"=response.data.fuelTypeDescription,
           "prime_mover"=response.data.primeMover,
           "generation_fuel_consuption"=response.data.consumption.for.eg.btu,
           "electricity_generation"=response.data.generation,
           "consumption_units"=response.data.consumption.for.eg.btu.units,
           "generation_units"=response.data.generation.units,
           "series_description"=response.description,
           "data_description"=request.params.data)) %>%
  as.data.table %>%
  setkey('date')

emissions_by_fuel_and_sector = GET(data_urls$value[['emissions_by_fuel_and_sector']])%>%
  content('text') %>%
  fromJSON(flatten=TRUE) %>%
  as.data.frame %>%
  select(c('date'=response.data.period,
           'sector'=response.data.sectorId,
           'sector_name'=response.data.sector.name,
           'fuel'=response.data.fuelId,
           'fuel_name'=response.data.fuel.name,
           'emissions'=response.data.value,
           'emissions_units'=response.data.value.units,
           'data_description'=response.description)) %>%
  as.data.table %>%
  setkey('date')

elec_gen_by_fuel_all_sectors_monthly = GET(data_urls$value[['elec_gen_by_fuel_all_sectors_monthly']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE) %>%
  as.data.frame %>%
  select(c('date'=response.data.period,
           'sector'=response.data.sectorDescription,
           'generation_fuel'=response.data.fueltypeid,
           'generation_fuel_name'=response.data.fuelTypeDescription,
           'electricity_generation'=response.data.generation,
           'electricity_generation_units'=response.data.generation.units,
           'data_description'=response.description)) %>%
  as.data.table %>%
  setkey('date')

elec_gen_by_fuel_all_sectors_annual = GET(data_urls$value[['elec_gen_by_fuel_all_sectors_annual']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE) %>%
  as.data.frame %>%
  select(c('date'=response.data.period,
           'sector'=response.data.sectorDescription,
           'generation_fuel'=response.data.fueltypeid,
           'generation_fuel_name'=response.data.fuelTypeDescription,
           'electricity_generation'=response.data.generation,
           'electricity_generation_units'=response.data.generation.units,
           'data_description'=response.description)) %>%
  as.data.table %>%
  setkey('date')

elec_gen_by_fuel_elec_utility_sector_monthly =GET(data_urls$value[['elec_gen_by_fuel_elec_utility_sector_monthly']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE) %>%
  as.data.frame %>%
  select(c('date'=response.data.period,
           'sector'=response.data.sectorDescription,
           'generation_fuel'=response.data.fueltypeid,
           'generation_fuel_name'=response.data.fuelTypeDescription,
           'electricity_generation'=response.data.generation,
           'electricity_generation_units'=response.data.generation.units,
           'data_description'=response.description)) %>%
  as.data.table %>%
  setkey('date')

elec_gen_by_fuel_elec_utility_sector_annual =GET(data_urls$value[['elec_gen_by_fuel_elec_utility_sector_annual']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE) %>%
  as.data.frame %>%
  select(c('date'=response.data.period,
           'sector'=response.data.sectorDescription,
           'generation_fuel'=response.data.fueltypeid,
           'generation_fuel_name'=response.data.fuelTypeDescription,
           'electricity_generation'=response.data.generation,
           'electricity_generation_units'=response.data.generation.units,
           'data_description'=response.description)) %>%
  as.data.table %>%
  setkey('date')

elec_sales_mw_by_sector = GET(data_urls$value[['elec_sales_mw_by_sector']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE) %>%
  as.data.frame %>%
  select(c('date'=response.data.period,
           'sector'=response.data.sectorid,
           'sector_name'=response.data.sectorName,
           'sales'=response.data.sales,
           'sale_units'=response.data.sales.units,
           'data_description'=response.description)) %>%
  as.data.table %>%
  setkey('date')

#start making calculations for the latest years or even months based on the difference between sales and generation
imports = GET(data_urls$value[['imports']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE) %>%
  as.data.frame %>%
  select(c('date'=response.data.period,
           'imports'=response.data.net.interstate.trade,
           'import_units'=response.data.net.interstate.trade.units,
           'data_description'=response.description)) %>%
  as.data.table %>%
  setkey('date')

sun_and_wind_generation_capacity = GET(data_urls$value[['sun_and_wind_generation_capacity']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE)
sun_and_wind_generation_capacity = as.data.frame(sun_and_wind_generation_capacity$response$data) %>%
  as.data.table %>%
  rename('date'='period') %>%
  setkey('date')

elec_customers_by_sector = GET(data_urls$value[['elec_customers_by_sector']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE) %>%
  as.data.frame %>%
  select(c('date'=response.data.period,
           'sector'=response.data.sectorid,
           'sector_name'=response.data.sectorName,
           'customers'=response.data.customers,
           'customers_units'=response.data.customers.units,
           'data_description'=response.description)) %>%
  as.data.table %>%
  setkey('date')

elec_gen_fuel_consumption_monthly = GET(data_urls$value[['elec_gen_fuel_consumption_monthly']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE) %>%
  as.data.frame %>%
  select(c('date'=response.data.period,
           'sector'=response.data.sectorid,
           'sector_name'=response.data.sectorDescription,
           'fuel'=response.data.fueltypeid,
           'fuel_name'=response.data.fuelTypeDescription,
           'consumption_for_generation'=response.data.consumption.for.eg.btu,
           'consumption_units'=response.data.consumption.for.eg.btu.units,
           'data_description'=response.description)) %>%
  as.data.table %>%
  setkey('date')

elec_gen_fuel_consumption_annual = GET(data_urls$value[['elec_gen_fuel_consumption_annual']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE) %>%
  as.data.frame %>%
  select(c('date'=response.data.period,
           'sector'=response.data.sectorid,
           'sector_name'=response.data.sectorDescription,
           'fuel'=response.data.fueltypeid,
           'fuel_name'=response.data.fuelTypeDescription,
           'consumption_for_generation'=response.data.consumption.for.eg.btu,
           'consumption_units'=response.data.consumption.for.eg.btu.units,
           'data_description'=response.description)) %>%
  as.data.table %>%
  setkey('date')

fuel_consumption_by_sector = GET(data_urls$value[['fuel_consumption_by_sector']]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE) %>%
  as.data.frame %>%
  select(c('date'=response.data.period,
           'sector'=response.data.seriesId,
           'sector_description'=response.data.seriesDescription,
           'fuel_consumption'=response.data.value,
           'consumption_units'=response.data.unit,
           'data_description'=response.description)) %>%
  as.data.table %>%
  setkey('date')

#write new tables to the database
db_driver = RPostgres::Postgres()
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
dbWriteTable(db, "eia_fuel_consumption_by_sector", value = fuel_consumption_by_sector, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_elec_gen_fuel_consumption_monthly", value = elec_gen_fuel_consumption_monthly, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_elec_gen_fuel_consumption_annual", value = elec_gen_fuel_consumption_annual, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_elec_customers_by_sector", value = elec_customers_by_sector, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_sun_and_wind_generation_capacity", value = sun_and_wind_generation_capacity, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_imports", value = imports, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_sales_mw_by_sector", value = elec_sales_mw_by_sector, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_gen_by_fuel_elec_utility_sector_monthly", value = elec_gen_by_fuel_elec_utility_sector_monthly, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_gen_by_fuel_elec_utility_sector_annual", value = elec_gen_by_fuel_elec_utility_sector_annual, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_elec_gen_by_fuel_all_sectors_monthly", value = elec_gen_by_fuel_all_sectors_monthly, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_elec_gen_by_fuel_all_sectors_annual", value = elec_gen_by_fuel_all_sectors_annual, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_emissions_by_fuel_and_sector", value = emissions_by_fuel_and_sector, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_plant_data_monthly", value = plant_data_monthly, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(db, "eia_plant_data_annual", value = plant_data_annual, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbDisconnect(db)
