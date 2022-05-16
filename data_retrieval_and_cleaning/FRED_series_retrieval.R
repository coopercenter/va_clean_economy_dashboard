fetch_fred_series <- function(fred_series_id,start_date="1990-01-01") {
  #  start_date="1990-01-01"
  #fred_series_id = "VANGSP" 
  library(lubridate);
  library(fredr);library(data.table)
  #
  # Download the series 
  fredr_set_key("d7e23bfbe22e3a24b163719b064385f0")
  fred_data = data.table(fredr_series_observations(series_id = fred_series_id,
                                                   observation_start = as.Date(start_date))) 
  fred_data[,`:=`(series_id=NULL,realtime_start=NULL,realtime_end=NULL)]
  setkey(fred_data,"date")
  return(fred_data)
}

#Retrieve the population data
va_pop = fetch_fred_series("VAPOP","1960-01-01") 
setnames(va_pop,2,"va_pop")

# Retrieve the real GDP data
va_real_gsp = fetch_fred_series("VARGSP","1960-01-01") 
setnames(va_real_gsp,2,"va_rgsp")
va_state_info = merge(va_pop,va_real_gsp,by="date",all=TRUE)
dbRemoveTable(db,"va_state_info")
dbWriteTable(db,"va_state_info",va_state_info,append=F,row.names=F)