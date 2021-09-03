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

#-----------------------------------------------------------
# annual
elec_sales_through_2019 <- read_excel(here('raw_data','elec_sales_forecasts_2019-12.xlsx'),sheet = "Annual",col_names = TRUE)
elec_sales_through_2019 <- select(elec_sales_through_2019, c(1,3,5,15,17))
dbWriteTable(db, 'elec_sales_through_2019_annual', elec_sales_through_2019, row.names=FALSE, overwrite = TRUE)

#-----------------------------------------------------------
# monthly
elec_sales_through_2019_monthly<- read_excel(here('raw_data','elec_sales_forecasts_2019-12.xlsx'),sheet = "Monthly",col_names = TRUE)
elec_sales_through_2019_monthly <- select(elec_sales_through_2019_monthly, c(1,2,3,9,11))
dbWriteTable(db, 'elec_sales_through_2019_monthly', elec_sales_through_2019_monthly, row.names=FALSE, overwrite = TRUE)

#close db connection
dbDisconnect(db)




















