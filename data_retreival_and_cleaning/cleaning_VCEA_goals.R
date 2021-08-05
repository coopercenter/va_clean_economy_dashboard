library(dplyr)
library(tidyverse)
library(stringr) # for replacing strings
library(here)
library(readxl)
library("RPostgreSQL")

db_driver = dbDriver("PostgreSQL")
source(here('my_postgres_credentials.R'))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
rm(ra_pwd)

VCEA <- read_excel(here('raw_data','VCEA_goals.xlsx')) 
VCEA <- as.data.frame(VCEA)

#Creating apco_dominion_rps dataset 
VCEA_renewable_portfolio_standards <- select(VCEA, c(1:3))

#Creating apco_dominion_onshore_wind_solar
VCEA_onshore_wind_solar <- select(VCEA, c(1,4:5))

#Creating apco_dominion_storage
VCEA_storage <- select(VCEA, c(1,6:7))

#Creating apco_dominion_energy_effciency
VCEA_energy_effciency <- select(VCEA, c(1,8:9))
#Make sure the values in this table are represented as percentage.
VCEA_energy_effciency$apco_energy_efficiency_as_share_of_2019_sales <- VCEA_energy_effciency$apco_energy_efficiency_as_share_of_2019_sales*100
VCEA_energy_effciency$dominion_energy_efficiency_as_share_of_2019_sales <- VCEA_energy_effciency$dominion_energy_efficiency_as_share_of_2019_sales*100

#Creating apco_dominion_storage
clean_energy_renewable_goals <-data.frame(matrix(ncol = 3, nrow = 60))
colnames(clean_energy_renewable_goals) <- c('Year','Company','percentage_of_energy_from_renewables' )
clean_energy_renewable_goals[,1] <- select(VCEA, 'year')
clean_energy_renewable_goals[1:30, 2] <- "Appalachian"
clean_energy_renewable_goals[31:60, 2] <- "Dominion"
clean_energy_renewable_goals[,3] <- select(VCEA, 'apco_rps')
clean_energy_renewable_goals[31:60,3] <- select(VCEA, 'dominion_rps')
clean_energy_renewable_goals$percentage_of_energy_from_renewables <- clean_energy_renewable_goals$percentage_of_energy_from_renewables*100


#upload to db
dbWriteTable(db, 'VCEA_renewable_portfolio_standards', VCEA_renewable_portfolio_standards, row.names=FALSE, overwrite = TRUE)
dbWriteTable(db, 'VCEA_onshore_wind_solar', VCEA_onshore_wind_solar, row.names=FALSE, overwrite = TRUE)
dbWriteTable(db, 'VCEA_storage',VCEA_storage, row.names=FALSE, overwrite = TRUE)
dbWriteTable(db, 'VCEA_energy_efficiency', VCEA_energy_effciency, row.names=FALSE, overwrite = TRUE)
dbWriteTable(db, 'clean_energy_renewable_goals', clean_energy_renewable_goals, row.names=FALSE, overwrite = TRUE)

#close db connection
dbDisconnect(db)