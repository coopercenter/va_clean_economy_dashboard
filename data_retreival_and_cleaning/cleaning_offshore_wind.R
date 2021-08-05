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
offshore_wind <- read_excel(here('raw_data','offshore_wind_data.xlsx'))
#break down dataset into subsets
total_mw_offshore_wind <- offshore_wind[1:18,]
net_capacity_factor_offshore_wind <- offshore_wind[52:71,1:5]
total_production_forecast_offshore_wind <- offshore_wind[75:92,1:2]

#create function to clean datasets and upload them to db 
simple_clean <- function(df) {
  names(df)<-lapply(df[1,],as.character)
  df <- df[-1,]
  colnames(df)<-str_replace_all(colnames(df),' ','_')
  df <- data.frame(lapply(df,as.numeric))
}

#clean datasets
total_mw_offshore_wind <- simple_clean(total_mw_offshore_wind)
net_capacity_factor_offshore_wind <- simple_clean(net_capacity_factor_offshore_wind)
total_production_forecast_offshore_wind <- simple_clean(total_production_forecast_offshore_wind)
colnames(total_production_forecast_offshore_wind) <- c('Year','Total_Production')

#upload to database
df_name <- deparse(substitute(total_mw_offshore_wind))
dbWriteTable(db, df_name, total_mw_offshore_wind, row.names=FALSE, overwrite = TRUE)
df_name <- deparse(substitute(net_capacity_factor_offshore_wind))
dbWriteTable(db, df_name, net_capacity_factor_offshore_wind, row.names=FALSE, overwrite = TRUE)
df_name <- deparse(substitute(total_production_forecast_offshore_wind))
dbWriteTable(db, df_name, total_production_forecast_offshore_wind, row.names=FALSE, overwrite = TRUE)

#close db connection
dbDisconnect(db)
