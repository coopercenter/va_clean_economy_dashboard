library(dplyr)
library(tidyverse)
library(stringr) # for replacing strings
library(here)
library("RPostgreSQL")

db_driver = dbDriver("PostgreSQL")
source(here('my_postgres_credentials.R'))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
rm(ra_pwd)

emission <- read.csv(here('raw_data', 'emission.csv'))

emission <- as.data.frame(t(emission))
emission <- emission[,2:24]
names(emission)<-lapply(emission[1,],as.character)
emission <- emission[-1,]
colnames(emission)[1] <- 'Year'
emission[,1] <-str_replace_all(emission[,1],'Year','')

for (i in 1:ncol(emission)){
  emission[,i]<-as.numeric(gsub(",", "", emission[,i]))
}

colnames(emission)<-str_replace_all(colnames(emission),' ','_')
co2_emissions <- emission[,c(1,15:19)]
colnames(co2_emissions)<-tolower(colnames(co2_emissions))
sulfur_emissions <- emission[,c(1,3:7)]
colnames(sulfur_emissions) <- tolower(colnames(sulfur_emissions))
nitrogen_emissions <- emission[,c(1,9:13)]
colnames(nitrogen_emissions) <- tolower(colnames(nitrogen_emissions))


#upload to db
# dbWriteTable(db, 'emission', emission, row.names=FALSE, overwrite = TRUE)
dbWriteTable(db, 'emissions_co2_by_source_va', co2_emissions, row.names=FALSE, overwrite = TRUE)
dbWriteTable(db, 'emissions_no_by_source_va', nitrogen_emissions, row.names=FALSE, overwrite = TRUE)
dbWriteTable(db, 'emissions_so2_by_source_va', sulfur_emissions, row.names=FALSE, overwrite = TRUE)
#close db connection
dbDisconnect(db)