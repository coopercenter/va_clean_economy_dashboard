# Connection to the database
# "my_postgres_credentials.R" contains the log-in informations of RAs
library(here)
library('RPostgreSQL')
library(tidyverse)
source(here("my_postgres_credentials.R"))
library(lubridate)

db_driver <- dbDriver("PostgreSQL")
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
rm(ra_pwd)

# check the connection
dbExistsTable(db, "metadata")

#-----------------------------------------------------------------------------------------------
# get the datasets
pop <- dbGetQuery(db,'SELECT * from fred_vapop')

# unit of population is thousands of persons
pop$value <- as.numeric(pop$value)*1000
pop$date <- as.Date(pop$date)

# load consumption data
tot_consumption <- dbGetQuery(db,'SELECT * from eia_seds_tetcb_va_a')

# merge on shared values
merged_data <- full_join(pop, tot_consumption, by="date", suffix=c('_pop', "_consumption"))
df <- merged_data %>% select(value_pop, value_consumption, date)

# derive the values
df$consumption_per_capita <- df$value_consumption/df$value_pop
df$year <- year(as.POSIXct(df$date))

c_per_cap_df <- df %>% select(year, consumption_per_capita) %>% filter(!is.na(consumption_per_capita))

#upload to db
dbWriteTable(db, 'energy_consumption_per_capita_va', c_per_cap_df, row.names=FALSE, overwrite = TRUE)
#-----------------------------------------------------------------------------------------------

# get the datasets
gdp <- dbGetQuery(db,'SELECT * from fred_vangsp')

# unit of GDP is in million of dollar, convert to dollars
gdp$value <- as.numeric(gdp$value)*1000000
gdp$date <- as.Date(gdp$date)

# tot_consumption has more history than GDP
c_per_gdp_df <- full_join(gdp, tot_consumption, by="date", suffix=c("_gdp", "_consumption"))

# derive the values, convert value_consumption from billion btu to btu
c_per_gdp_df$consumption_per_unit_gdp <- c_per_gdp_df$value_consumption*1000000000/c_per_gdp_df$value_gdp

c_per_gdp_df <- c_per_gdp_df %>% select(consumption_per_unit_gdp, year) %>% filter(!is.na(consumption_per_unit_gdp))

#upload to db
dbWriteTable(db, 'energy_consumption_per_unit_gdp_va', c_per_gdp_df, row.names=FALSE, overwrite = TRUE)

#-----------------------------------------------------------------------------------------------
#close db connection
dbDisconnect(db)
