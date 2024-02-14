#data cleaning and calculations for the EIA APIv2 data returns
#PREP------------------------------------------------------------------------------------------------------------------------------------
lbry<-c("data.table", "RPostgreSQL", "scales", 'maps', "tidyr", "dplyr",
        "tools", "sf", "rnaturalearth", "rnaturalearthdata", "rgeos",
        "ggplot2", "zoo", "lubridate", "Hmisc", "here",'stringr')
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)

##create the driver and connect to the database-------------------------------------------------------------------------------
db_driver = RPostgres::Postgres()
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

##read in all the EIA data---------------------------------------------------------------------------------------------------
sector_fuel_consumption <- data.table(dbGetQuery(db, "select * from eia_fuel_consumption_by_sector ;"))
generation_fuel_consumption_m <- data.table(dbGetQuery(db, "select * from eia_elec_gen_fuel_consumption_monthly ;"))
generation_fuel_consumption_a <- data.table(dbGetQuery(db, "select * from eia_elec_gen_fuel_consumption_annual ;"))
sector_customers <- data.table(dbGetQuery(db, "select * from eia_elec_customers_by_sector ;"))
sun_and_wind_capacity <- data.table(dbGetQuery(db, "select * from eia_sun_and_wind_generation_capacity ;"))
imports <- data.table(dbGetQuery(db, "select * from eia_imports ;"))
sector_sales <- data.table(dbGetQuery(db, "select * from eia_sales_mw_by_sector ;"))
utility_gen_m <- data.table(dbGetQuery(db, "select * from eia_gen_by_fuel_elec_utility_sector_monthly ;"))
utility_gen_a <- data.table(dbGetQuery(db, "select * from eia_gen_by_fuel_elec_utility_sector_annual ;"))
all_sector_gen_m <- data.table(dbGetQuery(db, "select * from eia_elec_gen_by_fuel_all_sectors_monthly ;"))
all_sector_gen_a <- data.table(dbGetQuery(db, "select * from eia_elec_gen_by_fuel_all_sectors_annual ;"))
emissions <- data.table(dbGetQuery(db, "select * from eia_emissions_by_fuel_and_sector ;"))
plant_data_m <- data.table(dbGetQuery(db, "select * from eia_plant_data_monthly ;"))
plant_data_a <- data.table(dbGetQuery(db, "select * from eia_plant_data_annual ;"))

#Energy Efficiency Page Data Loading------------------------------------------------------------------------------------------------------
lead_by_example_data <- data.table(dbGetQuery(db,"select * from energycap_place_meter_and_savings_data ;"))
LBE_building_tracker <- data.table(dbGetQuery(db,'select * from agency_facility_tracking ;'))
vcea_spending_prog <- data.table(dbGetQuery(db,'select * from energy_efficiency_spending_progress ;'))
vcea_standard_projections <- data.table(dbGetQuery(db,'select * from energy_efficiency_resource_standard_projections'))
vcea_spending_requirements <- data.table(dbGetQuery(db,'select * from energy_efficiency_spending_requirements'))

dbDisconnect(db)

#Page: Summary----------------------------------------------------------------------------------------------------------------------------

#Visualizations:
##RPS Donut-----------------------------------------------------------------------------------------------------------------------

#filter the monthly electricity generation data from all sectors to just wind, solar, hydro, and total generation
rps_donut_data <- all_sector_gen_m %>% 
  filter(year(paste0(all_sector_gen_m$date,"-01"))==year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  filter(generation_fuel %in% c("ALL","TPV","WND","HYC")) %>%
  group_by(generation_fuel) %>%
  summarise(ytd_gen=sum(electricity_generation)) %>%
  as.data.table
#calculate the percent of renewable generation to the total generation
percent_renewable <-round(((rps_donut_data[generation_fuel=='TPV',]$ytd_gen + rps_donut_data[generation_fuel=='WND',]$ytd_gen + rps_donut_data[generation_fuel=='HYC',]$ytd_gen) /rps_donut_data[generation_fuel=='ALL',]$ytd_gen * 100),1)
#store the latest year
latest_year = year(paste0(last(all_sector_gen_m$date),"-01"))
#target percent
renewable_percent_gen_2050_goal = 100
#create a final set of data that will be read by the donut generation code
renewable_ring = data.frame(
  category = c(
    paste(latest_year, "RPS generation (%)"),
    "Additional progress necessary to reach goal (%)"
  ),
  value = c(
    percent_renewable,
    renewable_percent_gen_2050_goal - percent_renewable
  )
)
#clean up placeholder variables/data after every visualization table is ready
rm(rps_donut_data,percent_renewable,latest_year,renewable_percent_gen_2050_goal)


##Carbon-Free Generation Donut-------------------------------------------------------------------------------------------------
carbon_free_donut_data <- all_sector_gen_m %>% 
  filter(year(paste0(all_sector_gen_m$date,"-01"))==year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  filter(generation_fuel %in% c("ALL","TPV","WND","HYC","NUC")) %>%
  group_by(generation_fuel) %>%
  summarise(ytd_gen=sum(electricity_generation)) %>%
  as.data.table

#calculate the percent of renewable generation to the total generation
percent_carbon_free <-round(((carbon_free_donut_data[generation_fuel=='TPV',]$ytd_gen + carbon_free_donut_data[generation_fuel=='WND',]$ytd_gen + carbon_free_donut_data[generation_fuel=='HYC',]$ytd_gen + carbon_free_donut_data[generation_fuel=='NUC',]$ytd_gen) /carbon_free_donut_data[generation_fuel=='ALL',]$ytd_gen * 100),1)
#store the latest year
latest_year = year(paste0(last(all_sector_gen_m$date),"-01"))
#target percent
carbon_free_percent_gen_2050_goal = 100
#create a final set of data that will be read by the donut generation code
carbon_free_ring = data.frame(
  category = c(
    paste(latest_year, "carbon-free generation (%)"),
    "Additional progress necessary to reach goal (%)"
  ),
  value = c(
    percent_carbon_free,
    carbon_free_percent_gen_2050_goal - percent_carbon_free
  )
)
rm(carbon_free_donut_data,percent_carbon_free,latest_year,carbon_free_percent_gen_2050_goal)

#Energy storage capacity donut: PJM or EIA I need help

##Electricity production percent by fuel type pie chart-------------------------------------------------------------------------------
annual_production_pie_data <- all_sector_gen_m %>%
  #get the latest data, YTD
  filter(year(paste0(all_sector_gen_m$date,"-01"))==year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  #filter for the fuels we are tracking
  filter(generation_fuel %in% c("ALL","SUN","WND","HYC","NUC","COW","DFO","NG","DPV","WWW","BIO","PET")) %>%
  #rename the values for laypersons
  mutate(generation_fuel = recode(generation_fuel,"ALL"="Total","SUN"="Solar, utility","DPV"="Solar, distributed",
                                  "WND"="Wind","HYC"="Hydropower","WWW"="Wood","BIO"="Other biomass",
                                  "NUC"="Nuclear","DFO"="Oil","NG"="Natural gas","COW"="Coal","PET"="Petroleum")) %>%
  #group by fuel, then summarise by fuel totals for the year
  group_by(generation_fuel) %>%
  summarise(ytd_gen=sum(electricity_generation, na.rm=TRUE)) %>%
  #convert to data table to do the percent calculation
  as.data.table %>%
  mutate(percent_gen = round((ytd_gen/.[generation_fuel=="Total",]$ytd_gen*100),1)) %>%
  #select only the columns we need for the visualization, and rename them according to the chart convention
  select("value"="percent_gen","variable"="generation_fuel")

##Electricity production by fuel type filled area chart----------------------------------------------------------------------------
annual_production_latest <- all_sector_gen_m %>%
  filter(year(paste0(all_sector_gen_m$date,"-01"))==year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  filter(generation_fuel %in% c("SUN","WND","HYC","NUC","COW","DFO","NG","DPV","WWW","BIO","PET")) %>%
  mutate(generation_fuel = recode(generation_fuel,"SUN"="Solar, utility","DPV"="Solar, distributed",
                                  "WND"="Wind","HYC"="Hydropower","WWW"="Wood","BIO"="Other biomass",
                                  "NUC"="Nuclear","DFO"="Oil","NG"="Natural gas","COW"="Coal","PET"="Petroleum")) %>%
  group_by(fill_value=generation_fuel) %>%
  summarise(y_value=sum(electricity_generation, na.rm=TRUE),x_value=year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  as.data.table

annual_production_past <- all_sector_gen_a %>%
  filter(generation_fuel %in% c("SUN","WND","HYC","NUC","COW","DFO","NG","DPV","WWW","BIO","PET")) %>%
  mutate(generation_fuel = recode(generation_fuel,"SUN"="Solar, utility","DPV"="Solar, distributed",
                                  "WND"="Wind","HYC"="Hydropower","WWW"="Wood","BIO"="Other biomass",
                                  "NUC"="Nuclear","DFO"="Oil","NG"="Natural gas","COW"="Coal","PET"="Petroleum")) %>%
  group_by(x_value=date,fill_value=generation_fuel) %>%
  summarise(y_value=sum(electricity_generation,na.rm=TRUE))%>%
  as.data.table
  
annual_production_area_data <- rbind(annual_production_past,annual_production_latest) %>%
  as.data.table
rm(annual_production_latest,annual_production_past)
##Energy consumption percent by Sector Pie Chart-----------------------------------------------------------------------------------
latest_consumption_pie <- sector_fuel_consumption %>%
  filter(date==last(date)) %>%
  filter(sector %in% c("TEACB","TECCB","TEEIB","TERCB","TEICB")) %>%
  mutate(sector=recode(sector,"TEACB"="Transportation","TECCB"="Commercial","TEEIB"="Electric Power",
                       "TERCB"="Residential","TEICB"="Industrial")) %>%
  mutate(percent_consumption=round(fuel_consumption/sum(fuel_consumption)*100,1)) %>%
  select("value"="percent_consumption","variable"="sector") %>%
  as.data.table
  

##Energy Consumption by Sector filled area chart--------------------------------------------------------------------------------------------
annual_consumption_data <- sector_fuel_consumption %>%
  filter(sector %in% c("TEACB","TECCB","TEEIB","TERCB","TEICB")) %>%
  mutate(sector=recode(sector,"TEACB"="Transportation","TECCB"="Commercial","TEEIB"="Electric Power",
                       "TERCB"="Residential","TEICB"="Industrial")) %>%
  select("x_value"="date","fill_variable"="sector","y_value"="fuel_consumption")%>%
  as.data.table

#Page: Generation and Capacity--------------------------------------------------------------------------------------------------
#Visualizations:

##Onshore Wind and Solar Capacity Donut-----------------------------------------------------------------------------------------------------------
onshore_wind_and_solar <- sun_and_wind_capacity %>%
  filter(sun_and_wind_capacity$date ==last(sun_and_wind_capacity$date)) %>%
  #there is no onshore wind yet, so just solar for now
  filter(`energy-source-desc`=="Solar") %>%
  group_by(`energy-source-desc`) %>%
  summarise(capacity = sum(`nameplate-capacity-mw`))
latest_year = year(paste0(last(sun_and_wind_capacity$date),"-01"))
#target capacity
onshore_wind_and_solar_2035_goal = 16100
#create a final set of data that will be read by the donut generation code
onshore_wind_and_solar_capacity_ring = data.frame(
  category = c(
    paste(latest_year, "capacity (MW)"),
    "Additional capacity necessary to reach goal (MW)"
  ),
  value = c(
    onshore_wind_and_solar$capacity[1],
    onshore_wind_and_solar_2035_goal - onshore_wind_and_solar$capacity[1]
  )
)
rm(onshore_wind_and_solar,latest_year,onshore_wind_and_solar_2035_goal)
##RPS Donut (again): See Summary page for creation details

##Offshore Wind Capacity donut: sun_and_wind_capacity-----------------------------------------------------------------------------
offshore_wind <- sun_and_wind_capacity %>%
  filter(sun_and_wind_capacity$date ==last(sun_and_wind_capacity$date)) %>%
  #just focusing on the offshore wind for now
  filter(`energy-source-desc`=="Wind") %>%
  group_by(`energy-source-desc`) %>%
  summarise(capacity = sum(`nameplate-capacity-mw`))
latest_year = year(paste0(last(sun_and_wind_capacity$date),"-01"))
#target capacity
offshore_wind_2034_goal = 5200
#create a final set of data that will be read by the donut generation code
offshore_wind_capacity_ring = data.frame(
  category = c(
    paste(latest_year, "capacity (MW)"),
    "Additional capacity necessary to reach goal (MW)"
  ),
  value = c(
    offshore_wind$capacity[1],
    offshore_wind_2034_goal - offshore_wind$capacity[1]
  )
)

##Virginia recent renewable and carbon-free electricity generation line graph-------------------------------------------------------------------------------
all_latest <- all_sector_gen_m %>%
  filter(year(paste0(all_sector_gen_m$date,"-01"))==year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  filter(generation_fuel=="ALL")%>%
  group_by(generation_fuel)%>%
  summarise(ytd_gen=sum(electricity_generation)) %>%
  as.data.table

renewable_latest <- all_sector_gen_m %>% 
  filter(year(paste0(all_sector_gen_m$date,"-01"))==year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  filter(generation_fuel %in% c("TPV","WND","HYC")) %>%
  group_by(generation_fuel) %>%
  summarise(ytd_gen = sum(electricity_generation)) %>% 
  mutate(x_value=year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  group_by(x_value) %>%
  summarise(renewable_gen=sum(ytd_gen)) %>%
  mutate(y_value=round(renewable_gen/all_latest$ytd_gen*100,2),
         fill_variable="Percent renewable") %>%
  select(-c(renewable_gen)) %>%
  as.data.table

carbon_free_latest <- all_sector_gen_m %>% 
  filter(year(paste0(all_sector_gen_m$date,"-01"))==year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  filter(generation_fuel %in% c("TPV","WND","HYC","NUC")) %>%
  group_by(generation_fuel) %>%
  summarise(ytd_gen = sum(electricity_generation)) %>% 
  mutate(x_value=year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  group_by(x_value) %>%
  summarise(carbon_free_gen=sum(ytd_gen)) %>%
  mutate(y_value=round(carbon_free_gen/all_latest$ytd_gen*100,2),
         fill_variable="Percent carbon-free") %>%
  select(-c(carbon_free_gen)) %>%
  as.data.table

renewable_historic <- all_sector_gen_a %>%
  filter(generation_fuel %in% c("TPV","WND","HYC")) %>%
  group_by(x_value=date) %>%
  summarise(annual_renewable_gen=sum(electricity_generation)) %>%
  mutate(y_value=round(annual_renewable_gen/all_sector_gen_a[generation_fuel=="ALL",]$electricity_generation*100,2),
         fill_variable="Percent renewable") %>%
  select(-c(annual_renewable_gen)) %>%
  as.data.table

carbon_free_historic <- all_sector_gen_a %>%
  filter(generation_fuel %in% c("TPV","WND","HYC","NUC")) %>%
  group_by(x_value=date) %>%
  summarise(annual_carbon_free_gen = sum(electricity_generation)) %>%
  mutate(y_value=round(annual_carbon_free_gen/all_sector_gen_a[generation_fuel=="ALL",]$electricity_generation*100,2),
         fill_variable="Percent carbon-free") %>%
  select(-c(annual_carbon_free_gen)) %>%
  as.data.table

renewable_and_carbon_free_line <- rbind(renewable_historic,renewable_latest,carbon_free_historic,carbon_free_latest)

##Virginia RPS schedule line graph: I think we're getting rid of this
##Virginia Net Electricity Imports line graph----------------------------------------------------------------------------------------------------------------
import_data_historic <- imports %>%
  select(x_value=date,y_value=imports) %>%
  #convert MWh to GWh
  mutate(y_value=abs(y_value)/1000)

latest_sales <- sector_sales %>%
  filter(year(paste0(sector_sales$date,"-01"))==year(paste0(last(sector_sales$date),"-01"))) %>%
  filter(sector=="ALL") %>%
  group_by(sector) %>%
  summarise(ytd_sales=sum(sales))

latest_gen <- all_sector_gen_m %>%
  filter(year(paste0(all_sector_gen_m$date,"-01"))==year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  filter(generation_fuel=="ALL") %>%
  group_by(generation_fuel) %>%
  summarise(ytd_gen=sum(electricity_generation))

import_estimate_latest <- data.table(x_value = year(paste0(last(sector_sales$date),"-01")),y_value=latest_sales$ytd_sales - latest_gen$ytd_gen)

import_data <- rbind(import_data_historic,import_estimate_latest) %>%
  as.data.table

rm(import_data_historic,lateat_sales,latest_gen,import_estimate_latest)

##Virginia Carbon-free electricity generation by source line graph-----------------------------------------------------------------------------------------------
carbon_free_latest <- all_sector_gen_m %>% 
  filter(year(paste0(all_sector_gen_m$date,"-01"))==year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  filter(generation_fuel %in% c("SUN","WND","HYC","NUC","DPV")) %>%
  group_by(fill_variable = generation_fuel) %>%
  summarise(y_value=sum(electricity_generation)) %>%
  mutate(x_value = year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  mutate(fill_variable=recode(fill_variable,"SUN"="Solar, utility","WND"="Wind","HYC"="Hydropower","NUC"="Nuclear","DPV"="Solar, distributed"))

carbon_free_historic <- all_sector_gen_a %>%
  filter(generation_fuel %in% c("SUN","WND","HYC","NUC","DPV")) %>%
  select(c("x_value"="date","y_value"="electricity_generation","fill_variable"="generation_fuel")) %>%
  mutate(fill_variable=recode(fill_variable,"SUN"="Solar, utility","WND"="Wind","HYC"="Hydropower","NUC"="Nuclear","DPV"="Solar, distributed")) %>%
  #exclude anything with a value of 0 since it'll create long lines when there should be a starting point
  filter(y_value > 0)

annual_carbon_free_line_data <- rbind(carbon_free_historic,carbon_free_latest) %>%
  as.data.table
rm(carbon_free_historic,carbon_free_latest)

##Virginia Solar Electricity Generation utility scale and distributed line graph-----------------------------------------------------------------------------------
solar_latest <- all_sector_gen_m %>% 
  filter(year(paste0(all_sector_gen_m$date,"-01"))==year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  filter(generation_fuel %in% c("SUN","DPV")) %>%
  group_by(fill_variable = generation_fuel) %>%
  summarise(y_value=sum(electricity_generation)) %>%
  mutate(x_value = year(paste0(last(all_sector_gen_m$date),"-01"))) %>%
  mutate(fill_variable=recode(fill_variable,"SUN"="Solar, utility","DPV"="Solar, distributed"))

solar_historic <- all_sector_gen_a %>%
  filter(generation_fuel %in% c("SUN","DPV")) %>%
  select(c("x_value"="date","y_value"="electricity_generation","fill_variable"="generation_fuel")) %>%
  mutate(fill_variable=recode(fill_variable,"SUN"="Solar, utility","DPV"="Solar, distributed")) %>%
  #exclude anything with a value of 0 since it'll create long lines when there should be a starting point
  filter(y_value > 0)

annual_solar_gen_line_data <- rbind(solar_historic,solar_latest) %>%
  as.data.table
rm(solar_latest,solar_historic)

#Projected offshore wind electricity generation line graph: I think this can go now

#CVOW capacity planned line graph (REMOVE)
#Carbon-free generation donut (again): see details for its occurrence on the previous page

#Page: Energy Efficiency-------------------------------------------------------------------------------------------------------------------------------------------------------
#FORMATTING THE ENERGYCAP API DATA
##annual_kwh_by_square_feet------------------------------------------------------------------------------------------------------------------------------------------
#data aggregation for energy efficiency plots
energycap_by_building_size_data <- function(){
  df <- lead_by_example_data %>% 
    group_by(year,size_range) %>%
    dplyr::summarize(cost=sum(unique(totalCost),na.rm=TRUE),kWh=sum(unique(commonUse),na.rm=TRUE),
                     sqft=sum(unique(size.value),na.rm=TRUE),buildings=n_distinct(placeId),
                     savings=sum(unique(savingsCommonUse),na.rm = TRUE)) %>%
    mutate(size_range=factor(size_range, levels=c("5,000 - 50,000","50,001 - 100,000","100,001 - 250,000","250,001 - 500,000","500,001 - 990,000")))
  
  return(df)
}

#write a function to filter by specified building size
filter_by_building_size <- function(set_size_range){
  df <- lead_by_example_data %>% filter(size_range==set_size_range)
  return(df)
}

#group the specified building size by primary use
group_by_place_use <- function(set_size_range){
  base_data <- filter_by_building_size(set_size_range)
  data_by_use <- base_data %>% group_by(year,primaryUse.primaryUseInfo) %>%
    dplyr::summarize(cost=sum(unique(totalCost),na.rm=TRUE),kWh=sum(unique(commonUse),na.rm=TRUE),sqft=sum(unique(size.value),na.rm=TRUE),
                     buildings=n_distinct(placeId),size=size_range,
                     savings=sum(unique(savingsCommonUse),na.rm = TRUE))
  data_by_use <- distinct(data_by_use)
  return(data_by_use)
}

##buildings_tracked----------------------------------------------------------------------------------------------------------------------------------------
#group LBE_building_tracker by agency category
building_tracking_by_agency_category_data <- function(){
  df <- LBE_building_tracker %>% group_by(agency_category) %>% 
    summarise(facilities_over_5000_sqft=sum(as.numeric(facilities_over_5000_sqft),na.rm=TRUE),
              facilities_over_5000_sqft_tracked = sum(as.numeric(accounts_in_warehouse),na.rm=TRUE),
              percent_done = sum(as.numeric(accounts_in_warehouse),na.rm=TRUE)/sum(as.numeric(facilities_over_5000_sqft),na.rm=TRUE),
              number_of_agencies= n_distinct(agency_name)) %>%
    filter(facilities_over_5000_sqft !=0)
  
  
  #add the yearly targets and their labels, a little awkward since the lengths are different but this works in lieu of a better solution
  df$yearly_goal <- c(0.05,0.20,0.45,0.70,1.00,1.00,1.00,1.00,1.00,1.00,1.00)
  df$yearly_goal_label <- c('5% by January 2021','20% by January 2022','45% by January 2023',
                            '70% by January 2024',
                            '100% by January 2025',
                            '100% by January 2025',
                            '100% by January 2025',
                            '100% by January 2025',
                            '100% by January 2025',
                            '100% by January 2025',
                            '100% by January 2025')
  #factor for proper legend order
  df$yearly_goal_label <- factor(df$yearly_goal_label,levels=c("5% by January 2021",
                                                               "20% by January 2022",
                                                               "45% by January 2023",
                                                               "70% by January 2024",
                                                               "100% by January 2025"))
  #factor the agency categories for cleaner plotting
  df$agency_category <- factor(df$agency_category,levels=c("Administration",
                                                           "Agriculture and Forestry",
                                                           "Commerce and Trade",
                                                           "Culture",
                                                           "Education",
                                                           "Health and Human Services",
                                                           "Natural Resources",
                                                           "Public Safety and Homeland Security",
                                                           "Transportation",
                                                           "Veterans and Defense Affairs",
                                                           "Total"))
  
  return(df)
}

#now, for each category, we're going to filter, filter again by 5000 square feet, and get the names of the agencies
#make it a function
filter_by_agency_categories <- function(category){
  data <- LBE_building_tracker %>% filter(agency_category==category) %>% 
    group_by(agency_name) %>% 
    summarise(facilities_over_5000_sqft=sum(as.numeric(facilities_over_5000_sqft),na.rm=TRUE),
              facilities_over_5000_sqft_tracked = sum(as.numeric(accounts_in_warehouse),na.rm=TRUE),
              percent_done = sum(as.numeric(accounts_in_warehouse),na.rm=TRUE)/sum(as.numeric(facilities_over_5000_sqft),na.rm=TRUE)) %>%
    filter(facilities_over_5000_sqft !=0)
  return(data)
}

##Function to reshape the spending mandate data for apco_mandates_and_progress, dominion_mandates_and_progress, and odp_mandates_and_progress
reshape_spending_data <- function(utility){
  util_current <- vcea_spending_prog %>% filter(date==max(date)) %>% 
    select(date,contains(utility))
  util_reshaped <- melt(util_current,'date',c(paste0(utility,'_ee_costs'),paste0(utility,'_15_percent_carve_out'),
                                              paste0(utility,'_hb2789_petition')),
                        variable.name='spending_category',value.name='spending_to_date') %>%
    bind_cols(spending_goal=vcea_spending_requirements$spending_category,
              spending_requirements=vcea_spending_requirements[[paste0(utility,'_required_2028')]]) %>%
    select(-spending_category)
  return(util_reshaped)
  
}


#Page: Emissions---------------------------------------------------------------------------------------------------------------------------------------------------
#Visualizations:
##CO2 emissions from electricity production vs all sectors line graph-----------------------------------------------------------------------------------------------------
co2_emissions_total <- emissions %>%
  filter(sector=="TT",fuel=="TO") %>%
  select(c('x_value'=date,"y_value"=emissions,"fill_variable"=sector)) %>%
  mutate(fill_variable=recode(fill_variable, "TT"="All sectors"))

co2_emissions_electricity <- emissions %>%
  filter(sector=="EC",fuel=="TO") %>%
  select(c('x_value'=date,"y_value"=emissions,"fill_variable"=sector)) %>%
  mutate(fill_variable=recode(fill_variable, "EC"="Electricity sector"))

#can only estimate current Electricity sector emissions since monthly numbers for fuel consumption by other industries are not available
#carbon content coefficients are in units of MMT CO2/QBtu, so divide by 1000 to get to million MMBtu
#Natural Gas coefficient: 52.91
#electric power coal: 95.82
#Petroleum Liquids: 73.33
co2_emissions_electricity_estimated <- generation_fuel_consumption_m %>%
  filter(year(paste0(generation_fuel_consumption_m$date,"-01")) > last(emissions$date)) %>%
  filter(fuel %in% c("COW","NG","PET")) %>%
  group_by(date=year(paste0(date,"-01")),fuel) %>%
  summarise(consumption_for_generation = sum(consumption_for_generation,na.rm = TRUE)) %>%
  as.data.table %>%
  mutate(estimated_emissions=fuel) %>%
  mutate(estimated_emissions=recode(estimated_emissions,"COW"=consumption_for_generation/1000*95.82,"NG"=consumption_for_generation/1000*52.91,
                                    "PET"=consumption_for_generation/1000*73.33)) %>%
  group_by(x_value=date) %>%
  summarise(y_value=sum(estimated_emissions)) %>%
  mutate(fill_variable="Electricity sector, estimated")

annual_co2_data <- rbind(co2_emissions_total,co2_emissions_electricity,co2_emissions_electricity_estimated) %>%
  as.data.table
rm(co2_emissions_electricity,co2_emissions_electricity_estimated,co2_emissions_total)

##CO2 emissions from electricity production by fuel type area graph-----------------------------------------------------------------------------------------------------

electric_co2_by_fuel_historic <- emissions %>%
  filter(sector=="EC",fuel %in% c("CO","PE","NG")) %>%
  select(c(x_value=date,y_value=emissions,fill_variable= fuel_name)) %>%
  as.data.table

electric_co2_emissions_by_fuel_estimated <- generation_fuel_consumption_m %>%
  filter(year(paste0(generation_fuel_consumption_m$date,"-01")) > last(emissions$date)) %>%
  filter(fuel %in% c("COW","NG","PET")) %>%
  group_by(date=year(paste0(date,"-01")),fuel) %>%
  summarise(consumption_for_generation = sum(consumption_for_generation,na.rm = TRUE)) %>%
  as.data.table %>%
  mutate(estimated_emissions=fuel) %>%
  mutate(estimated_emissions=recode(estimated_emissions,"COW"=consumption_for_generation/1000*95.82,"NG"=consumption_for_generation/1000*52.91,
                                    "PET"=consumption_for_generation/1000*73.33)) %>%
  select(c(x_value=date,y_value=estimated_emissions,fill_variable=fuel)) %>%
  mutate(fill_variable=recode(fill_variable,"COW"="Coal, estimated","NG"="Natural Gas, estimated","PET"="Petroleum, estimated")) %>%
  as.data.table

electric_co2_by_fuel_data <- rbind(electric_co2_by_fuel_historic,electric_co2_emissions_by_fuel_estimated) %>%
  as.data.table

rm(electric_co2_by_fuel_historic,electric_co2_emissions_by_fuel_estimated)

##CO2 emissions per capita line graph----------------------------------------------------------------------------------------------------------------------------------------
#Can't estimate further since there's not monthly data on the types of fuel consumed for each sector
#tidy up the VA state data for calculations
va_state_info <- va_state_info %>%
  mutate(date=year(paste0(date,"-01")),va_pop=va_pop*1000,va_rgsp=va_rgsp*1000000) %>%
  filter(date >= first(emissions$date)) %>%
  filter(date <= last(emissions$date))

emissions_total <- emissions %>%
  filter(sector=="TT",fuel=="TO")

emissions_per_capita <- merge(emissions_total,va_state_info,by=c('date')) %>%
  select(c(x_value=date,y_value=emissions,va_pop)) %>%
  mutate(y_value=y_value*1000000/va_pop,fill_variable="CO2 Per Capita") %>% #multiply by another million to convert MMTons of CO2 into Tons of CO2
  select(-c(va_pop)) %>%
  as.data.table

##CO2 emissions per dollar of GDP line graph--------------------------------------------------------------------------------------------
#data is presented as metric tons/thousand $ of GDP
#source data is in MMTons and ???? dollars
emissions_per_gdp <- merge(emissions_total, va_state_info,by=c("date")) %>%
  select(c(x_value=date,y_value=emissions,va_rgsp)) %>%
  mutate(y_value=y_value*1000000*1000/va_rgsp, fill_variable="CO2 Per GDP") %>%
  select(-c(va_rgsp)) %>%
  as.data.table
