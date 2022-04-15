#Load the packages and connect to the database, run the 'source' code for the data you want to update, then disconnect from the database
lbry<-c("data.table", "RPostgreSQL",  "tidyr", "dplyr","arrow","stringr",
        "tools","lubridate", "Hmisc", "here", "readxl","read_xlsx",'httr','jsonlite')
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

source(here("data_retrieval_and_cleaning/fetch_from_eia_api.R"))

source(here('data_retrieval_and_cleaning/FRED_series_retrieval.R'))

source(here('data_retrieval_and_cleaning/Calculate_intensity_data.R')) #requires the EIA and FRED data to calculate

source(here('data_retrieval_and_cleaning/reading_in_VCEA_provisions.R'))


