#PREP------------------------------------------------------------------------------------------------------------------------------------
lbry<-c("data.table", "RPostgreSQL", "scales", 'maps', "tidyr", "dplyr",
        "tools", "sf", "rnaturalearth", "rnaturalearthdata", "rgeos",
        "ggplot2", "zoo", "lubridate", "Hmisc", "here",'stringr')
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)

#CONNECT TO DATABASE----------------------------------------------------------------------------------------------------------------------
# Open database connection, load in all saved data tables then close db connection

#db_driver = dbDriver("PostgreSQL")
db_driver = RPostgres::Postgres()
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

#Metadata--------------------------------------------------------------------------------------------------------------------
metadata <- data.table(dbGetQuery(db,"select * from metadata ;"))

#Emissions Page Data Loading--------------------------------------------------------------------------------------------------------------
#load in data on SO2, NOx and CO2
va_electricity_emissions_by_fuel <- data.table(dbGetQuery(db, "select * from va_electricity_emissions_by_fuel ;"))

#load in energy efficiency data: energy and CO2 per capita and per GDP
intensity_data <- data.table(dbGetQuery(db,"select * from intensity_data ;"))

#Energy Efficiency Page Data Loading------------------------------------------------------------------------------------------------------
lead_by_example_data <- data.table(dbGetQuery(db,"select * from energycap_place_meter_and_savings_data ;"))
LBE_building_tracker <- data.table(dbGetQuery(db,'select * from agency_facility_tracking ;'))
eia_spending_prog <- data.table(dbGetQuery(db,'select * from energy_efficiency_spending_progress ;'))
eia_standard_projections <- data.table(dbGetQuery(db,'select * from energy_efficiency_resource_standard_projections'))
eia_spending_requirements <- data.table(dbGetQuery(db,'select * from energy_efficiency_spending_requirements'))


#Generation and Capacity Page Data Loading------------------------------------------------------------------------------------------------
#load in offshore wind projections
# This data is suspect and needs to be vetted carefully
offshore_wind_data <- data.table(dbGetQuery(db,"select * from offshore_wind ;"))

#load in pjm solar and wind data & apco/dominion goals
# Replace the subsequent 3 lines with 
plant_capacities = data.table(dbGetQuery(db,"select * from eia_plant_capacities ;"))

VCEA_onshore_wind_solar <- data.table(dbGetQuery(db,"select * from \"VCEA_onshore_wind_solar\" ;"))

#load in APCO & Dom RPS
VCEA <- data.table(dbGetQuery(db,"select * from vcea_provisions ;"))
setnames(VCEA,"year","Year")

#EIA Data Loading for Summary and Generation and Capacity Pages---------------------------------------------------------------------------
# Load EIA annual time series table
eia_annual_data <-data.table(dbGetQuery(db,"select * from eia_annual_data ;"))

# End of db access for now
dbDisconnect(db)
# All data saved in the database is now loaded.

#CLEANING AND RESTRUCTURING EIA DATA------------------------------------------------------------------------------------------------------

#
# Map local names to EIA data series

eia_annual_data <- eia_annual_data %>% 
  rename("Coal"="ELEC_GEN_COW_VA_99_A",
         "Oil"="ELEC_GEN_PEL_VA_99_A",
         "Gas"="ELEC_GEN_NG_VA_99_A",
         "Nuclear"="ELEC_GEN_NUC_VA_99_A",
         "Solar_utility"="ELEC_GEN_SUN_VA_99_A", 
         "Solar_distributed"="ELEC_GEN_DPV_VA_99_A",
         "Hydropower"="ELEC_GEN_HYC_VA_99_A",
         "Pumped_storage"="ELEC_GEN_HPS_VA_99_A",
         "Wind"="ELEC_GEN_WND_VA_99_A",
         "Wood"="ELEC_GEN_WWW_VA_99_A",
         "Other_biomass"="ELEC_GEN_WAS_VA_99_A",
         "Total_gen"="ELEC_GEN_ALL_VA_99_A",
         "Total_energy_cons"="SEDS_TETCB_VA_A",
         "Residential"="SEDS_TERCB_VA_A",
         "Commercial"="SEDS_TECCB_VA_A",
         "Industrial"="SEDS_TEICB_VA_A",
         "Transportation"="SEDS_TEACB_VA_A",
         "Imported_electricity"="SEDS_ELISP_VA_A",
         "Electric_sector_CO2_emissions"="EMISS_CO2_TOTV_EC_TO_VA_A",
         "Total_CO2_emissions"="EMISS_CO2_TOTV_TT_TO_VA_A")
#
# Now calculate variables that use only EIA data and store those in the EIA data table
#      For now, this is only annual data. No monthly data is used.

# Some additions and adjustments
eia_annual_data[Solar_utility == 0,Solar_utility:=NA] #random fix for visual purposes later on (redundant, as this is redone in later data calculations)
# Not sure if this should be done here or possibly closer to display time
eia_annual_data[is.na(eia_annual_data)]=0
### Check the energy math in the "other" calculation. For one thing, why is dpv in here?
### Should we net out pumped storage electricity use?
# Total annual carbon-free generation (incl. dpv)
# Note: the  definition of renewable is only wind, solar and hydro
# HWS_renewable: all hydro, wind and solar, including distributed
# RPS_renewable: hydro, wind and solar (utility-scale) [also called Renewable]
# Percent_renewable is the percent of non-nuclear generation (or is it sales? check)
eia_annual_data[,`:=`(Year = year(date),
                      All_solar = Solar_distributed +  Solar_utility,
                      Other = Total_gen-(Coal+Oil+Gas+Nuclear+Solar_utility+Solar_distributed+Hydropower+Wind+Wood+Other_biomass),
                      Carbon_free = Wind+Hydropower+Solar_utility+Nuclear,
                      HWS_Renewable = Wind+Solar_distributed+Solar_utility+Hydropower,
                      RPS_Renewable = Wind+Solar_utility+Hydropower,
                      Renewable = Wind+Solar_utility+Solar_distributed+Hydropower,
                      Total_energy_cons_new=Residential+Commercial+Transportation+Industrial),]

eia_annual_data[Total_gen!=0,`:=`(Percent_renewable = (Renewable/(Total_gen-Nuclear))*100, # Percent renewable generation of total generation
                                  Percent_carbon_free = (Carbon_free/Total_gen)*100,        # Percent carbon-free generation of total elec. generation
                                  Not_renewable=Total_gen-Renewable,
                                  Carbon_emitting=Total_gen-Carbon_free)]
setkey(eia_annual_data,Year)
# Renewable and carbon free percent gen---------------------------------------------------------------------
#percent_renewable_and_carbon_free_line_p
renewable_and_carbon_free_plot_data <- function(){
df <- melt(eia_annual_data[!is.na(Percent_renewable | !is.na(Percent_carbon_free)),
                                                                  .(Year,`Percent renewable`=Percent_renewable,
                                                                    `Percent carbon-free`=Percent_carbon_free)],id="Year")
setnames(df,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
return(df)
}


# APCO and Dominion RPS schedules are in the VCEA data.table
#rps_renewable_line_p
renewable_line_plot_data <- function(){
df = melt(VCEA[,.(Year,APCO=apco_rps,Dominion=dominion_rps)], id = "Year")
return(df)
}

# Solar & Wind Capacity vs VCEA goals -----------------------------------------------------------------------------------------------------
# Creating working versions to keep formatting of tables intact when they are uploaded to dashboard
#
### All of the results using PJM data for capacity measure must be replaced.
### This will require loading EIA power plant data and parsing it for capacity
### The PJM measure is incomplete and misleading
#
va_solar = plant_capacities[Prime_Mover=="PV",
                            .(id,capacity_mw = Nameplate_Capacity_MW,
                              Plant_Name,Operating_Year)]

#setting up the data and variables for single_ring_sw_capacity_donut_p
#recent_year = va_solar[,max(Operating_Year,na.rm=TRUE)]
#solar_capacity_current_mw = va_solar[,sum(capacity_mw)] 
### This should reflect onshore wind only!! There is none as of Jan 2022
single_ring_capacity_donut_data <- function(){
onshore_wind_capacity_current_mw = 0  #va_wind[,sum(capacity_mw)]
sw_capacity_2035_mw_goal = 16100 #16,100 MW of solar and onshore wind by January 1, 2024 (from VCEA Summary 3.0)

df = data.frame(
  category = c(paste0(va_solar[,max(Operating_Year,na.rm=TRUE)], " capacity"),"Additional capacity necessary to reach goal"),
  value = c(
    va_solar[,sum(capacity_mw)] + onshore_wind_capacity_current_mw,
    sw_capacity_2035_mw_goal - (va_solar[,sum(capacity_mw)] + onshore_wind_capacity_current_mw)
  )
)
return(df)
}

#Energy Storage
# Currently this is battery storage only. VCEA specifies battery storage amounts
# The display of storage may benefit from a little rethinking
#comment out until we know what we're doing here, since this is currently none when it should be 4
#va_storage <- plant_capacities[Prime_Mover=="BA",
                              # .(id,capacity_mw = Nameplate_Capacity_MW,
                               #  Plant_Name,Operating_Year)]

# Need to fix how on-shore and off-shore wind are derived
# Currently there is no on-shore wind in Virginia
# The 12 MW of off-shore wind in service in 2021 is handled manually at the plot
#Comment this out until we actually need it to avoid unnecessary runtime
#va_wind <- plant_capacities[Prime_Mover=="WT",
                            #.(id,capacity_mw = Nameplate_Capacity_MW,
                             # Plant_Name,Operating_Year)]

# Projected Offshore Wind Capacity (wind_projected_capacity_line_p)
wind_projected_capacity_data <- function(){
df = offshore_wind_data[,.(Year,CVOW_Pilot,CVOW_Stage_I,CVOW_Stage_II,CVOW_Stage_III,Total=Total_mw)]
df[Year<2022,`:=`(CVOW_Stage_I=NA,CVOW_Stage_II=NA,CVOW_Stage_III=NA)] 
df[Year<2020] <- NA
df <-melt(df,id="Year")
# Remove underscores from variable names for clean plotting
df[,variable:=gsub("_"," ",variable)]
setnames(df,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
return(df)
}

#Projected generation (wind_projected_generation_time_series_line_p)
wind_projected_generation_data <- function(){
df <- offshore_wind_data[,.(Year,Total_gen)]  #Stage_1_gen,Stage_2_gen,Stage_3_gen,
setnames(df,'Total_gen','Total Production')
df <- melt(df[Year>2019], id = "Year")
setnames(df,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
return(df)
}

#FORMATTING GENERATION DATA--------------------------------------------------------------------------------------------------
#table for va_annual_production_area_p and va_annual_production_pie_chart_p_with_legend
annual_production_data <- function(){
  df = melt(eia_annual_data[Year>2000,.(Year,Coal,Oil,Gas,Nuclear,`Solar, utility`=Solar_utility,
                                        `Solar, distributed`=Solar_distributed, Hydropower,Wind,Wood,
                                        `Other biomass`=Other_biomass
                                       )],id="Year")
setnames(df,c("Year","variable","value"),
         c("x_value","fill_variable","y_value")
)
return(df)
}

#additional setup for va_annual_production_pie_chart_p_with_legend
annual_production_pie_data <- function(){
  df_area <- annual_production_data()
 #latest_year = max(generation_stacked_area_data$x_value)
 df_pie = df_area[x_value==max(df_area$x_value) ][,x_value:=NULL]
 setnames(df_pie,c("y_value","fill_variable"),c("value","variable"))
return(df_pie)
}

#table for single_ring_renewable_donut_p
renewable_ring_data <- function(){
recent_year = eia_annual_data[!is.na(Percent_renewable),last(Year)]
renewable_percent_gen_recent = round(eia_annual_data[!is.na(Percent_renewable),
                                                     last(Percent_renewable)], 1)
renewable_percent_gen_2050_goal = 100

renewable_ring = data.frame(
  category = c(
    paste(recent_year, "RPS generation (%)"),
    "Additional progress necessary to reach goal (%)"
  ),
  value = c(
    renewable_percent_gen_recent,
    renewable_percent_gen_2050_goal - renewable_percent_gen_recent
  )
)
return(renewable_ring)
}

#single_ring_offshore_wind_capacity_donut_p
#set up the data
offshore_wind_capacity_data <- function(){
#recent_year = pjm_wind_working[,max(year(actual_in_service_date),na.rm=TRUE)]
recent_year = 2021

# The 12 MW of offshore wind is not showing up on the PJM queue
# offshore_wind_current_mw = pjm_wind_working[fuel == "Offshore Wind" &
#                                               status == "In Service", sum(mfo)]
offshore_wind_current_mw = 12
offshore_wind_2034_mw_goal = 5200 #Requires Dominion to develop 5,200 MW of offshore wind by Jan. 1, 2034 (from VCEA Summary 3.0)

offshore_wind_ring = data.frame(
  category = c(paste(recent_year, "capacity"),"additional capacity necessary to reach goal"),
  value = c(
    offshore_wind_current_mw,
    offshore_wind_2034_mw_goal - offshore_wind_current_mw
  )
)
return(offshore_wind_ring)
}

#solar generation, solar_generation_time_series_line_p
#data edits
solar_gen_data <- function(){
# Solar (broken into distributed and utility) over time
df <- melt(eia_annual_data[Year>2012,.(Year=Year,`Solar, utility`=Solar_utility,
                                               `Solar, distributed`=Solar_distributed)],id="Year")
# Don't display long lines at zero.
df[Year<2016 & variable=='Solar, utility',value := ifelse(value==0,NA,value)]
df[,Year:=as.factor(Year)]
setnames(df,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
return(df)
}

#IMPORT PLOT DATA----------------------------------------------------------------------------------------------------------
#set up the plot data for va_elec_net_imports_line_p
# VA Electricity Net Imports
import_data <- function(){
df <- eia_annual_data[Imported_electricity!=0,
                      .(x_value=Year,y_value=Imported_electricity,
                      fill_variable="Imports")]
return(df)
}
#CARBON FREE PLOT DATA-----------------------------------------------------------------------------------------------------
#annual_carbon_free_generation_by_type_line_p

# Solar, Hydro, and Nuclear Generation over Time
annual_carbon_free_data <- function(){
df <- melt(eia_annual_data[Nuclear!=0,.(Year,Nuclear,`Solar, utility` = Solar_utility,
                                                      `Solar, distributed` = Solar_distributed,Hydropower,Wind)],id="Year")
# Don't display long lines at zero.
df[Year<2016 & variable=='Solar, utility',value := ifelse(value==0,NA,value)]
df[Year<2013 & variable=='Solar, distributed',value := ifelse(value==0,NA,value)]
df[Year<2021 & variable=='Wind',value := ifelse(value==0,NA,value)]
#carbon_free_data[,Year:=as.factor(Year)]
setnames(df,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
return(df)
}

#single_ring_carbon_free_donut_p 
#set up data
carbon_free_ring_data <- function(){
recent_year = eia_annual_data[!is.na(Percent_carbon_free),last(Year)]
carbon_free_percent_gen_recent = round(eia_annual_data[!is.na(Percent_carbon_free),last(Percent_carbon_free)], 1)
carbon_free_percent_gen_2050_goal = 100 #100% of Virginia’s electricity from carbon-free sources by 2050

carbon_free_ring = data.frame(
  category = c(
    paste0(recent_year," carbon free generation (%)"),
    "Additional progress necessary to reach goal (%)"
  ),
  value = c(
    carbon_free_percent_gen_recent,
    carbon_free_percent_gen_2050_goal - carbon_free_percent_gen_recent
  )
)
return(carbon_free_ring)
}
#EMISSIONSA DATA----------------------------------------------------------------------------------------------------------------
#co2_combined_emissions_line_p
# CO2 total emissions & CO2 emissions from electric sector on same figure
# The SEDS data is greatly delayed. Maybe we can think of a better presentation here.
# Two more years of data is available beyond what SEDS has. I don't think that includes
#    total energy related CO2 emissions.
co2_combined_emissions_data <- function(){
df = melt(eia_annual_data[Total_CO2_emissions!=0,
                                    .(Year=year(date),
                                      `All sectors` = Total_CO2_emissions,
                                      `Electricity sector` = Electric_sector_CO2_emissions)],id="Year")
#CO2_emissions = merge(electricity_CO2_emissions,all_CO2_emissions,by="Year",all=TRUE)
setnames(df,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
return(df)
}

#carbon_by_fuel_emissions_stacked_p
co2_by_fuel_data <- function(){
df = melt(va_electricity_emissions_by_fuel[, .(
  Year = Year,
  Coal = CO2_Coal / 1000,
  `Natural gas` = CO2_Natural_gas / 1000,
  Petroleum = CO2_Petroleum / 1000,
  Other = CO2_Other / 1000
)], id = "Year")

setnames(df,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
return(df)
}

#emissions_per_capita_line_p
co2_per_capita_data <- function(){
df <- intensity_data[!is.na(co2_per_capita),
                                 .(x_value=as.factor(year(date)),y_value=co2_per_capita,
                                   fill_variable="CO2 Per Capita")]
return(df)
}

#emissions_per_gdp_line_p
co2_per_gdp_data <- function(){
df <- intensity_data[!is.na(co2_per_gdp),
                              .(x_value=as.factor(year(date)),y_value=co2_per_gdp,
                                fill_variable="CO2 Per GDP")]
return(df)
}
#FORMATTING CONSUMPTION DATA-----------------------------------------------------------------------------------------------------------
#set up the table for va_annual_consumption_area_p and va_annual_consumption_pie_chart_p_with_legend
consumption_data <- function(){
df = melt(eia_annual_data[,.(Year,Residential,Commercial ,Transportation,Industrial)],
                               id="Year")
setnames(df,c("Year","variable","value"),
         c("x_value","fill_variable","y_value")
) 
df <- df %>% filter(y_value !=0)
return(df)
}

#additional parameter setting for con_pie
annual_consumption_pie_data <- function(){
#latest_year = max(eia_annual_data[Commercial!=0,Year])
  df_con <- consumption_data()
  df_pie = df_con[x_value==max(eia_annual_data[Commercial!=0,Year]) ][,x_value:=NULL]
  setnames(df_pie,c("y_value","fill_variable"),c("value","variable"))
  return(df_pie)
}

#FORMATTING THE ENERGYCAP API DATA----------------------------------------------------------------------------------------------
#annual_kwh_by_square_feet
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

#buildings_tracked
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

#Function to reshape the spending mandate data for apco_mandates_and_progress, dominion_mandates_and_progress, and odp_mandates_and_progress
reshape_spending_data <- function(utility){
  util_current <- eia_spending_prog %>% filter(date==max(date)) %>% 
    select(date,contains(utility))
  util_reshaped <- melt(util_current,'date',c(paste0(utility,'_ee_costs'),paste0(utility,'_15_percent_carve_out'),
                                              paste0(utility,'_hb2789_petition')),
                        variable.name='spending_category',value.name='spending_to_date') %>%
    bind_cols(spending_goal=eia_spending_requirements$spending_category,
              spending_requirements=eia_spending_requirements[[paste0(utility,'_required_2028')]]) %>%
    select(-spending_category)
  return(util_reshaped)
  
}

#PROGRESS TOWARDS RENEWABLES-------------------------------------------------------------------------------------------------
#single_ring_storage_capacity_donut_p
#prepping the plot data/variables

storage_capacity_ring_data <- function(){
  #storage_capacity_current_mw = va_storage[,sum(capacity_mw)] 
  ## Temporary fix until storage data retrieval can be fixed
  recent_year=2021
  storage_capacity_current_mw = 4
  storage_capacity_2035_mw_goal = 3100

  storage_ring = data.frame(
    category = c(paste(recent_year, "capacity"),
                 "Additional capacity necessary to reach goal"),
    value = c(
      storage_capacity_current_mw,
      storage_capacity_2035_mw_goal - storage_capacity_current_mw
  )
)
return(storage_ring)
}
