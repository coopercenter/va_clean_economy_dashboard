library(dplyr)
library(tidyverse)
library(stringr) # for replacing strings
library(here)
library("RPostgreSQL")
library(readxl)

db_driver = dbDriver("PostgreSQL")
source(here('my_postgres_credentials.R'))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
rm(ra_pwd)

#read in dataset
virginia_annual_savings_through_2020 <- read_excel(here('raw_data','virginia_annual_savings_through_2020.xlsx'), col_names = FALSE)
virginia_annual_savings_through_2022 <- read_excel(here('raw_data','virginia_annual_savings_through_2022.xlsx'), col_names = FALSE)

#replacing row names 
colnames(virginia_annual_savings_through_2020)<- c("Company Name", "MWh")
colnames(virginia_annual_savings_through_2022)<- c("Company Name", "MWh")

#upload to db
dbWriteTable(db, 'virginia_annual_savings_through_2020', virginia_annual_savings_through_2020, row.names=FALSE, overwrite = TRUE)
dbWriteTable(db, 'virginia_annual_savings_through_2022', virginia_annual_savings_through_2022, row.names=FALSE, overwrite = TRUE)

#close db connection
dbDisconnect(db)

