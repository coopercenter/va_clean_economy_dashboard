#Load the packages and connect to the database, run the 'source' code for the data you want to update, then disconnect from the database
lbry<-c("data.table", "RPostgreSQL",  "tidyr", "dplyr","arrow","stringr",
        "tools","lubridate", "Hmisc", "here", "readxl","read_xlsx",'httr','jsonlite')
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

source(here("data_retrieval_and_cleaning/fetch_from_eia_api.R")) #updates eia_annual_data, most heavily used in the dashboard

source(here('data_retrieval_and_cleaning/FRED_series_retrieval.R')) #population data

source(here('data_retrieval_and_cleaning/calculate_intensity_data.R')) #requires the EIA and FRED data to calculate updates

source(here('data_retrieval_and_cleaning/reading_in_VCEA_provisions.R')) 

source(here('data_retrieval_and_cleaning/fetch_eia860_data.R')) #plant capacity data

source(here('data_retrieval_and_cleaning/retrieving_SEDS_emissions_by_fuel.R')) #used for emissions by fuel for one graph

source(here('data_retrieval_and_cleaning/retrieving_energycap_data.R')) #used for the energy efficiency page, tracking state building energy use over time

source(here('data_retrieval_and_cleaning/read_ve_mandate_sheets.R')) #used for the energy efficiency page, tracking progress on a number of state mandates

dbDisconnect(db)




