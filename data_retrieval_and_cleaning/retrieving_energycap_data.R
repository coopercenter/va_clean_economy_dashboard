#Retrieving the EnergyCAP data for the Energy Efficiency page---------------------------------------------------------------------

#get the key
key <- source(here('data_retrieval_and_cleaning/EnergyCAP_API_key.R'))

#make the API requests and convert them into dataframes

#get the first page of the place data so we can use the headers to tell us how many more pages we need
energyCAP_building_data <- GET("https://app.energycap.com/api/v3/place",
                               add_headers(.headers=c('ECI-ApiKey'=energycap_key)))

#store the number of pages before flattening to a JSON
pages <- as.numeric(energyCAP_building_data$headers$totalpages)

#flatten the request results
energyCAP_building_data <- energyCAP_building_data %>% 
  content("text") %>% 
  fromJSON(flatten=TRUE) %>%
  as.data.frame() %>% 
  unnest(cols=c(meters))

#use a for-loop to go through the places page by page to capture all the building data
for(i in (2:pages)){
  next_page <- GET(paste("https://app.energycap.com/api/v3/place?pageNumber=",i,sep=""), 
                   add_headers(.headers=c('ECI-ApiKey'=energycap_key))) %>% 
    content("text") %>% 
    fromJSON(flatten=TRUE) %>% 
    as.data.frame() %>% 
    unnest(cols=c(meters))
  energyCAP_building_data <- rbind(energyCAP_building_data,next_page)
  
}

#create a function to process the API results for a given query for the meter and savings data
fetch_from_energyCAP <- function(query, unnest_col){
  GET(query,add_headers(.headers=c('ECI-ApiKey'=energycap_key))) %>% 
    content("text") %>% 
    fromJSON(flatten=TRUE) %>% 
    as.data.frame() %>% 
    unnest(cols=all_of(unnest_col))
}

#get the meter data which has use and cost, convert to dataframe then unnest the deeper columns
meter_query <- "https://app.energycap.com/api/v3/meter/digest/actual/yearly"
energyCAP_meter_data <- fetch_from_energyCAP(meter_query,'results')

#get the savings data
savings_query <- "https://app.energycap.com/api/v3/meter/digest/savings/yearly"
energyCAP_savings_data <- fetch_from_energyCAP(savings_query,'results')

#energyCAP_meter_data <- GET("https://app.energycap.com/api/v3/meter/digest/actual/yearly", 
#                           add_headers(.headers=c('ECI-ApiKey'=energycap_key)))  %>% 
#content("text") %>% 
#fromJSON(flatten=TRUE) %>%
#as.data.frame() %>% 
#unnest(cols=results)

#energyCAP_savings_data <- GET("https://app.energycap.com/api/v3/meter/digest/savings/yearly", 
#                             add_headers(.headers=c('ECI-ApiKey'=energycap_key))) %>% 
#content("text") %>% 
#fromJSON(flatten=TRUE) %>%
#as.data.frame() %>% 
#unnest(cols=results)

#remove the excess variables
rm(energycap_key,key)

#join the meter dataframe with the building dataframe on meterId
energyCAP_data <- full_join(energyCAP_building_data,energyCAP_meter_data,by='meterId')

#join the meter and building data with the savings data on meterId and year
energyCAP_data <- full_join(energyCAP_data,energyCAP_savings_data,by=c('meterId','year'))

#pare the data down to just the important parts to explore for visualizations
columns_to_keep <- c('placeId','parent.placeInfo','placeInfo',
                     'primaryUse.primaryUseInfo',
                     'size.value','year','totalCost.x','commonUse.x','savingsCommonUse')

#filter by electric meter
energyCAP_data <- energyCAP_data %>% filter(commodity.commodityCode=='ELECTRIC') %>% 
  #filter by size
  filter(as.numeric(size.value) >= 5000) %>%
  #pare down to just the columns we want
  select(all_of(columns_to_keep)) %>%
  #clear up any duplicates that may have come from merging
  distinct() %>%
  #rename a couple of columns
  rename(commonUse=commonUse.x) %>% 
  rename(totalCost=totalCost.x)

rm(energyCAP_building_data,energyCAP_meter_data,energyCAP_savings_data,columns_to_keep)


#write to the database
dbRemoveTable(db,"energycap_place_meter_and_savings_data")
dbWriteTable(db,"energycap_place_meter_and_savings_data",energyCAP_data)