# Connection to the database
# "my_postgres_credentials.R" contains the log-in informations of RAs
library(here)
library('RPostgreSQL')
library(tidyverse)
source(here("my_postgres_credentials.R"))

db_driver <- dbDriver("PostgreSQL")
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
rm(ra_pwd)

# check the connection
dbExistsTable(db, "metadata")

#-------------------------------------------------------------------------------------------------------
# get the datasets of gdp from 1997 to 2017
gdp <- dbGetQuery(db,'SELECT * from fred_vangsp')

# unit of gdp is millions of dollars
gdp$value<- as.numeric(gdp$value)*1000 
gdp$date <- as.Date(gdp$date)
#Now this information will show total dollars. 1997 to 2017

tot_emission <- dbGetQuery(db,'SELECT * from eia_emiss_co2_totv_tt_to_va_a') 
# original unit of emission is million metric tons CO2
tot_emission$value<- tot_emission$value*1000000

# join data
merged <- full_join(gdp, tot_emission, by="date", suffix=c("_gdp", "_emission"))

# derive the values
merged$co2_emission_per_thousand_dollars_of_gdp <- (merged$value_emission/merged$value_gdp)
e_per_gdp_df <- merged %>% select(year, co2_emission_per_thousand_dollars_of_gdp) %>% filter(!is.na(co2_emission_per_thousand_dollars_of_gdp))

#upload to db
dbWriteTable(db, 'co2_emission_per_thousand_dollars_of_gdp_va', e_per_gdp_df, row.names=FALSE, overwrite = TRUE)
#-------------------------------------------------------------------------------------------------------
# get the datasets of population from 1980 to 2017
pop <- dbGetQuery(db,'SELECT * from fred_vapop')

# unit of population is thousands of persons
pop$value <- as.numeric(pop$value)*1000 
pop$date <- as.Date(pop$date)

merged <- full_join(pop, tot_emission, by="date", suffix=c("_pop", "_emission"))

# derive the values
merged$co2_emission_per_capita <- merged$value_emission/merged$value_pop

e_per_cap_df <- merged %>% select(year, co2_emission_per_capita) %>% filter(!is.na(co2_emission_per_capita))

# upload to db
dbWriteTable(db, 'co2_emission_per_capita_va', e_per_cap_df, row.names=FALSE, overwrite = TRUE)

#-------------------------------------------------------------------------------------------------------
#close db connection
dbDisconnect(db)
