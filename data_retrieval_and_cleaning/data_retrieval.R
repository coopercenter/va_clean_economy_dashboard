"This file acts as a top-level resource for all the data retrieval processes used for the dashboard, and a directory for
what each source provides to the plots.
See the sourced files for details on how each data source is retrieved and stored, and for troubleshooting source-specific 
errors
Load the packages and connect to the database, run the 'source' code for the data you want to update, then disconnect 
from the database"

lbry<-c("data.table", "RPostgreSQL",  "tidyr", "dplyr","arrow","stringr",
        "tools","lubridate", "Hmisc", "here", "readxl","read_xlsx",'httr','jsonlite','readr','eia')
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)

db_driver = RPostgres::Postgres()
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

#EIA DATA------------------------------------------------------------------------------------------------------------------
#updates EIA data from the API, 99% of data used in the dashboard
source(here("data_retrieval_and_cleaning/fetch_from_eia_apiv2.R"))

#FRED DATA-----------------------------------------------------------------------------------------------------------------
#population data
source(here('data_retrieval_and_cleaning/FRED_series_retrieval.R'))

#VCEA DATA-----------------------------------------------------------------------------------------------------------------
#use for graphing the renewable portfolio schedule
source(here('data_retrieval_and_cleaning/reading_in_vcea_provisions.R')) 

#ENERGYCAP DATA------------------------------------------------------------------------------------------------------------
#used for the energy efficiency page, tracking state building energy use over time
source(here('data_retrieval_and_cleaning/retrieving_energycap_data.R')) 

#ENERGY EFFICIENCY MANDATE DATA--------------------------------------------------------------------------------------------
#used for the energy efficiency page, tracking progress on a number of state mandates
source(here('data_retrieval_and_cleaning/read_ve_mandate_sheets.R')) 

#FACILITY TRACKING SPREADSHEET
#used for the building tracking goals on the Energy Efficiency page
source(here("data_retrieval_and_cleaning/update_cova_facility_tracker_data.R"))

dbDisconnect(db)




