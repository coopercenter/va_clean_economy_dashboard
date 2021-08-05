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
current_EE_programs <- read_excel(here('raw_data','energy_efficiency_programs.xlsx'), col_names = TRUE)
dominion_current_EE_data_through_2018 <- current_EE_programs[c(2,4:8),]

#upload to db
dbWriteTable(db, 'current_ee_programs', current_EE_programs, row.names=FALSE, overwrite = TRUE)

#close db connection
dbDisconnect(db)




















