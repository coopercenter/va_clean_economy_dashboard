library("data.table")
library("RPostgreSQL")
library("tidyr")
library("dplyr")
library("arrow")
library("stringr")
library("tools")
library("lubridate")
library("Hmisc")
library("here")
library("readxl")
#library("read_xlsx")
library('httr')
library('jsonlite')
library('readr')
library('eia')

#read in the API key--------------------------------------------------------------------------
key <- source(here('data_retrieval_and_cleaning/my_eia_api_key.R'))
eia_key = key$value
#Read in the original series IDs that map to the database-------------------------------------
series_urls <- source(here('data_retrieval_and_cleaning/eia_apiv2_all_urls.R'))
#for windows
series_ids <- readr::read_file(here('data_retrieval_and_cleaning/series_ids_new.txt'))
series_id_list <- unlist(strsplit(series_ids,'\r\n'))
#replace '\r\n' with just '\n' for Mac
#get the SEDS IDs
seds_ids_list <- series_id_list[grepl("SEDS",series_id_list)]
#SALES IDs
sales_ids_list <- series_id_list[grepl("SALES",series_id_list)]
#PRICE IDs
price_ids_list <- series_id_list[grepl("PRICE",series_id_list)]
#CUSTOMERS
customers_ids_list <- series_id_list[grepl("CUSTOMERS",series_id_list)]
#All GEN IDs
gen_ids_list <- series_id_list[grepl("GEN",series_id_list)]
#emissions
emiss_ids_list <- series_id_list[grepl("EMISS",series_id_list)]

#Query and transform the data---------------------------------------
#General method for turning the JSON response into a data table
flatten_api_return <- function(series_id,date_column,data_column){
  data_table <- GET(series_urls$value[[series_id]]) %>%
    content('text') %>%
    fromJSON(flatten=TRUE)%>%
    as.data.frame %>%
    select(date_column,data_column) %>%
    rename("date" = date_column, !!series_id := data_column)
  data_table
}

#Get just the dates for the SEDS data
seds_data <- GET(series_urls$value[["SEDS_TETCB_VA_A"]]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE)%>%
  as.data.frame %>%
  select("response.data.period") %>%
  rename("date" = "response.data.period")

for (item in seds_ids_list){
 data <-flatten_api_return(item,"response.data.period","response.data.value")
 seds_data <- left_join(seds_data,data,by="date")
}

#Get the dates for the Sales data
sales_data <- GET(series_urls$value[["ELEC_SALES_VA_ALL_A"]]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE)%>%
  as.data.frame %>%
  select("response.data.period") %>%
  rename("date" = "response.data.period")

for (item in sales_ids_list){
  data <-flatten_api_return(item,"response.data.period","response.data.sales")
  sales_data <- left_join(sales_data,data,by="date")
}

#Get the dates for the Price data
price_data <- GET(series_urls$value[["ELEC_PRICE_VA_ALL_A"]]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE)%>%
  as.data.frame %>%
  select("response.data.period") %>%
  rename("date" = "response.data.period")

for (item in price_ids_list){
  data <-flatten_api_return(item,"response.data.period","response.data.price")
  price_data <- left_join(price_data,data,by="date")
}

#Get the Customers data
customers_data <-flatten_api_return("ELEC_CUSTOMERS_VA_ALL_M","response.data.period","response.data.customers")

#Get the Electricity Generation data
gen_data <- GET(series_urls$value[["ELEC_GEN_ALL_VA_99_A"]]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE)%>%
  as.data.frame %>%
  select("response.data.period") %>%
  rename("date" = "response.data.period")

for (item in gen_ids_list){
  data <-flatten_api_return(item,"response.data.period","response.data.generation")
  gen_data <- left_join(gen_data,data,by="date")
}


#Get the Emissions data
emiss_data <- GET(series_urls$value[["EMISS_CO2_TOTV_EC_TO_VA_A"]]) %>%
  content('text') %>%
  fromJSON(flatten=TRUE)%>%
  as.data.frame %>%
  select("response.data.period") %>%
  rename("date" = "response.data.period")

for (item in emiss_ids_list){
  data <-flatten_api_return(item,"response.data.period","response.data.value")
  emiss_data <- left_join(emiss_data,data,by="date")
}

#Consolidate the monthly data------------------------------------
#eia_monthly_data <- merge(gen_data,c(sales_data,price_data,customers_data),by="date",all=TRUE) %>%
  #select(-c(date.1,date.2))

#Consolidate the annual data-------------------------------------
eia_annual_data <- full_join(seds_data,emiss_data,by="date")
eia_annual_data <- eia_annual_data %>%
  full_join(sales_data,by="date")%>%
  full_join(price_data,by="date") %>%
  full_join(gen_data,by="date") %>%
  mutate(date=paste0(date,"-01-01"))
  
#generate annual summaries from the monthly data
eia_monthly_data <- customers_data %>% 
  #get the date column into the right format
  mutate(date=as.Date(paste0(date,"-01"))) %>%
  mutate(year = year(date))
#Select the latest full year of data
#all the years with December values
latest_year <- eia_monthly_data %>% filter(month(date) == 12)
#latest year with a December value, the latest full year
latest_year <- as.numeric(max(latest_year$year))

#summarize the monthly data by year
annual_data.fromMonthly <- eia_monthly_data %>%
  group_by(year) %>%
  summarise(ELEC_CUSTOMERS_VA_ALL_A = sum(ELEC_CUSTOMERS_VA_ALL_M,na.rm=TRUE)) %>%
  filter(year <= latest_year) %>%
  #recreate the date column
  mutate(date = paste0(year,"-01-01")) %>%
  #remove the year column as we are done with it
  select(-year)
#replace the M suffix for 'Monthly' with an A suffix for 'Annual'
#colnames(annual_data.fromMonthly) <- str_replace_all(colnames(annual_data.fromMonthly),"_M","_A")
#Tidy up the extra column now that we're done using it
eia_monthly_data <- eia_monthly_data %>% select(-year)
# Merge all annual series
eia_annual_data <- full_join(eia_annual_data,annual_data.fromMonthly, by="date") %>%
  arrange(desc(date))

#convert monthly and annual data to data tables so as to set keys for the PostgreSQL database
#eia_monthly_data <- eia_monthly_data %>%
 # mutate(date = as.Date(date)) %>%
  #as.data.table()
eia_annual_data <- eia_annual_data %>%
  mutate(date = as.Date(date)) %>%
  as.data.table()
#setkey(eia_monthly_data,date)
setkey(eia_annual_data,date)

#Store Monthly and Annual Tables in the Database------------------
#dbWriteTable(db, "eia_monthly_data", value = eia_monthly_data, append = FALSE, overwrite = TRUE, row.names = FALSE)
#dbExecute(db, "alter table eia_monthly_data add primary key (date);")

dbWriteTable(db, "eia_annual_data", value = eia_annual_data, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbExecute(db, "alter table eia_annual_data add primary key (date);")

#clean up variables once done
rm(eia_monthly_data,sales_data,seds_data,gen_data,price_data,emiss_data,series_id_list,series_ids,series_urls,gen_ids_list,sales_ids_list,price_ids_list,emiss_ids_list,customers_data,customers_ids_list,data)



