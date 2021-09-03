# This script extracts datasets from the US Energy Information Administration's
# (EIA) via API given a txt file with pre-identified series ids of the target
# datasets or a variable storing the series ids, and uploads them to the postgres database

# The script used to upload the metadata corresponding to the EIA datasets are 
# stored in the metadata folder named metadata.R
#----------------------------------------------------------------------------------------
library(groundhog)
groundhog.day = "2021-09-01"
pkgs = c("httr", "here", "dplyr", 'tibble', "readr", "RPostgreSQL",
         "eia", "stringr")
groundhog.library(pkgs, groundhog.day)

# Make a list of series ids
# If you have a large number of datasets, create a txt file storing the series ids
## reads in a txt file containing the series ids of the datasets we need
series_id_vec <- read_file(here("api_data_code","series_ids.txt"))

## transform the content to a list that we can later feed into the fetch function
series_id_list <- unlist(strsplit(series_id_vec,'\r\n'))

#---------------------------------------------------------------------------
# If you only have a few datasets, here is an example
## series_id_list <- c("SEDS.TERCB.VA.A","SEDS.TECCB.VA.A","SEDS.TEICB.VA.A",
##            "SEDS.TEACB.VA.A","SEDS.TETCB.VA.A")
#----------------------------------------------------------------------------

# set the url root for the EIA data
url_root <- "http://api.eia.gov/series/"

# read in your eia api key by sourcing
## if you don't have one, apply for one at EIA's website and save it in a separate 
## R script in the etl folder with the exact script name and variable name specified below

source(here("api_data_code","my_eia_api_key.R"))
eia_set_key(eiaKey)

# function used to fetch the eia series given the series id
fetch_eia_series <- function(series_id){
  
  data_series <- eia_series(series_id)
  
  return(data_series)
}

# Fetching a large number of datasets
## apply the function to each series id in the series id list using lappy
all_data_series <-lapply(series_id_list,fetch_eia_series)

## create an empty list to store the data tables
all_tables<- vector("list", length(series_id_list))


# function used to display data
displaydata <- function(series) {
  (series$data[[1]] -> series_data_tbl)
  return(series_data_tbl)
}

# loops through the all data series and store the tables in to a big list
for (i in 1:length(all_data_series)){
  series<-all_data_series[[i]]
  all_tables[[i]]<-displaydata(series)
}

#--------------------------------------------------------------------------------
# No need to go beyond this line if you do not want to upload the dataset to the database

# Connect to the database
# Check that the VPN is on
# "my_postgres_credentials.R" contains the log-in informations of RAs
source(here("my_postgres_credentials.R"))
db_driver = dbDriver("PostgreSQL")
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)


# Function used to change the names of tables to the format used in MySQL
## Key: all lower case and no punctuation other than _
get_name <- function(series_id) {
  db_table_name <- str_to_lower(paste("eia", str_replace_all(series_id, "[.-]", "_"), sep="_"))
  return(db_table_name)
}

# apply the function to the list of series id to get the names for the data tables
db_table_names <- lapply(series_id_list,get_name)

# Loops through the list of tables and write data series to the PostgreSQL database 
# then query the data from postgreSQL (query is SQL language meaning writing commands in SQL)
# "SELECT * FROM datatable_name" means getting the whole table
# Note: This code OVERWRITES EXISTING TABLE!

for (i in 1:length(all_data_series)){
  dbWriteTable(db, db_table_names[[i]], value = all_tables[[i]], append = FALSE, overwrite = TRUE, row.names = FALSE)
  dbGetQuery(db, paste("SELECT * from",db_table_names[[i]])) %>% as_tibble() -> df_postgres
}

# Close connection
dbDisconnect(db)
dbUnloadDriver(db_driver)

