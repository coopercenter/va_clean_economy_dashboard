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

#-----------------------------------------------------------------------------------
#PJM Solar
pjm_solar <- read_excel(here('raw_data','pjm_solar.xlsx'))
dbWriteTable(db, 'pjm_solar', pjm_solar, row.names=FALSE, overwrite = TRUE)

#-----------------------------------------------------------------------------------
#PJM Wind
pjm_wind <- read_excel(here('raw_data','pjm_wind.xlsx'))
dbWriteTable(db, 'pjm_wind', pjm_wind, row.names=FALSE, overwrite = TRUE)

#-----------------------------------------------------------------------------------
#PJM Storage
pjm_storage <- read_excel(here('raw_data','pjm_storage.xlsx'))
dbWriteTable(db, 'pjm_storage', pjm_storage, row.names=FALSE, overwrite = TRUE)

#-----------------------------------------------------------------------------------
#PJM GATS
pjm_gats_generators <- read.csv(here("raw_data","pjm_gats_registered_renewable_generators.csv"),header=F)
names(pjm_gats_generators)<-lapply(pjm_gats_generators[1,],as.character)
pjm_gats_generators <- pjm_gats_generators[-1,]
colnames(pjm_gats_generators)<-str_replace_all(colnames(pjm_gats_generators),' ','_')
colnames(pjm_gats_generators)<-tolower(colnames(pjm_gats_generators))
dbWriteTable(db, 'pjm_gats_generators', pjm_gats_generators, row.names=FALSE, overwrite = TRUE)

#-----------------------------------------------------------------------------------
#close db connection
dbDisconnect(db)
