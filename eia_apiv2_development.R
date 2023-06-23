lbry<-c("data.table", "RPostgreSQL",  "tidyr", "dplyr","arrow","stringr",
        "tools","lubridate", "Hmisc", "here", "readxl","read_xlsx",'httr','jsonlite','readr')
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)
#EIA key
key <- source(here('data_retrieval_and_cleaning/my_eia_api_key.R'))
#URL version
EIA_request_test <- GET("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=7ee3cdbf1ded6bcfb9de1e50d722ebd4&frequency=monthly&data[0]=sales&facets[sectorid][]=ALL&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000")
#turn the request into a more readable data table.
eia_request_flat <- EIA_request_test %>% 
  content("text") %>% 
  fromJSON(flatten=TRUE) %>% 
  as.data.frame() %>%
  #shrink it down to just the key information
  select("response.data.period","response.data.sectorName","response.data.sales")


