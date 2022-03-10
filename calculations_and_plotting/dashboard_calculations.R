# library(groundhog)
# groundhog.day = "2021-09-01"
# pkgs = c("data.table", "RPostgreSQL", "scales", 'maps', "tidyr", "dplyr",
#         "tools", "sf", "tools", "rnaturalearth", "rnaturalearthdata", "rgeos",
#          "ggplot2", "zoo", "lubridate", "Hmisc", "here")
# groundhog.library(pkgs, groundhog.day)
lbry<-c("data.table", "RPostgreSQL", "scales", 'maps', "tidyr", "dplyr",
        "tools", "sf", "tools", "rnaturalearth", "rnaturalearthdata", "rgeos",
        "ggplot2", "zoo", "lubridate", "Hmisc", "here")
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)
max_eia_annual_data_year = 2020
#custom color palette
# need a new color for wind.
ceps_pal <- c("#00A087B2", "#3C5488B2", "#CEA5AC", "#BE7E8A", "#4DBBD5B2", "#91D1C2B2","#D9C6C9","#8491B4B2","#5868AC","#6FB3D9","#56BD96","#99A9E2","#A94F64","#B0DEFA","#99EEBB","#8FD3FE")


# Open database connection, load in all saved data tables then close db connection

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
#load in metadata
metadata <- data.table(dbGetQuery(db,"select * from metadata ;"))

#load in data on SO2, NOx and CO2 
va_electricity_emissions_by_fuel <- data.table(dbGetQuery(db, "select * from va_electricity_emissions_by_fuel ;")) #units = short tons
#va_annual_emissions <- data.table(dbGetQuery(db, "select * from va_annual_emissions ;")) #units = short tons

#load in energy efficiency data: energy and CO2 per capita and per GDP
intensity_data <- data.table(dbGetQuery(db,"select * from intensity_data ;"))

# Energy efficiency data: rework this material from scratch.
virginia_annual_savings_through_2022 <- data.table(dbGetQuery(db,"select * from virginia_annual_savings_through_2022 ;"))
virginia_annual_savings_through_2020 <- data.table(dbGetQuery(db,"select * from virginia_annual_savings_through_2020 ;"))
apco_dom_VCEA_goals<-data.table(dbGetQuery(db,"select * from \"VCEA_energy_efficiency\" ;"))

#load in offshore wind projections
# This data is suspect and needs to be vetted carefully
offshore_wind_data <- data.table(dbGetQuery(db,"select * from offshore_wind ;"))
# total_mw_offshore_wind <- data.table(dbGetQuery(db,"select * from total_mw_offshore_wind ;"))
# total_production_forecast_offshore_wind <- data.table(dbGetQuery(db,"select * from total_production_forecast_offshore_wind ;"))

#load in pjm solar and wind data & apco/dominion goals
# Replace the subsequent 3 lines with 
plant_capacities = data.table(dbGetQuery(db,"select * from eia_plant_capacities ;"))

VCEA_onshore_wind_solar <- data.table(dbGetQuery(db,"select * from \"VCEA_onshore_wind_solar\" ;"))

#load in APCO and Dominion historic sales (also ROS is in there)
#apco_dom_sales<-data.table(dbGetQuery(db,"select apco_total_gwh,dom_total_gwh from elec_sales_through_2019_annual ;"))
va_utility_sales<-data.table(dbGetQuery(db,"select * from va_annual_utility_sales ;"))
setnames(va_utility_sales,"year","Year")


#load in APCO & Dom RPS
VCEA <- data.table(dbGetQuery(db,"select * from vcea_provisions ;"))
setnames(VCEA,"year","Year")

rps_mandate_schedule <- data.table(dbGetQuery(db,"select * from clean_energy_renewable_goals ;"))

###
# Load EIA annual time series table
eia_annual_data <-data.table(dbGetQuery(db,"select * from eia_annual_data ;"))

#-----------------------------#
# Unused code
#-----------------------------#
# Energy Efficiency dataset
# Not used. This data is worthless.
#investment_by_IOUs <- data.table(dbGetQuery(db,"select * from current_ee_programs ;"))

#load in capacity by fuel type data (likely will be replaced if we find better solar data)
### This data is not used. It is essentially worthless.
#whole_electric_industry_capacity <- data.table(dbGetQuery(db,"select * from whole_electric_industry_capacity ;"))

#load in VA electricity imports
# This is in the EIA_annual data table.
#va_elec_import<-data.table(dbGetQuery(db,"select * from eia_seds_elisp_va_a ;"))[,date:=NULL]
#setnames(va_elec_import,"year","Year")

#load in energy equity data
#energy_burden_county_percent_income <- data.table(dbGetQuery(db,"select * from energy_burden_county_percent_income ;"))
#energy_burden_county_expenditures <- data.table(dbGetQuery(db,"select * from energy_burden_county_expenditures ;"))
#load in utility sales data
#va_utility_sales <- data.table(dbGetQuery(db,"select * from va_utility_sales ;"))
#setnames(va_utility_sales,"year","Year")

#-----------------------------#


# End of db access for now
dbDisconnect(db)
# All data saved in the database is now loaded.

#
# Map local names to EIA data series
#
eia_name=c("ELEC_GEN_COW_VA_99_A",
           "ELEC_GEN_PEL_VA_99_A",
           "ELEC_GEN_NG_VA_99_A",
           "ELEC_GEN_NUC_VA_99_A",
           "ELEC_GEN_SUN_VA_99_A",
           "ELEC_GEN_DPV_VA_99_A",
           "ELEC_GEN_HYC_VA_99_A",
           "ELEC_GEN_HPS_VA_99_A",
           "ELEC_GEN_WND_VA_99_A",
           "ELEC_GEN_WWW_VA_99_A",
           "ELEC_GEN_WAS_VA_99_A",
           "ELEC_GEN_ALL_VA_99_A",
           "SEDS_TETCB_VA_A",
           "SEDS_TERCB_VA_A",
           "SEDS_TECCB_VA_A",
           "SEDS_TEICB_VA_A",
           "SEDS_TEACB_VA_A",
           "SEDS_ELISP_VA_A",
           "EMISS_CO2_TOTV_EC_TO_VA_A",
           "EMISS_CO2_TOTV_TT_TO_VA_A")
local_name=c("Coal",
             "Oil",
             "Gas",
             "Nuclear",
             "Solar_utility", 
             "Solar_distributed",
             "Hydropower",
             "Pumped_storage",
             "Wind",
             "Wood",
             "Other_biomass",
             "Total_gen",
             "Total_energy_cons",
             "Residential",
             "Commercial",
             "Industrial",
             "Transportation",
             "Imported_electricity",
             "Electric_sector_CO2_emissions",
             "Total_CO2_emissions")

setnames(eia_annual_data,
         eia_name, local_name)
#
# Now calculate variables that use only EIA data and store those in the EIA data table
#      For now, this is only annual data. No monthly data is used.

# Some additions and adjustments
eia_annual_data[Solar_utility == 0,Solar_utility:=NA] #random fix for visual purposes later on
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
                      Renewable = Wind+Solar_utility+Solar_distributed+Hydropower
)]
eia_annual_data[Total_gen!=0,`:=`(Percent_renewable = (Renewable/(Total_gen-Nuclear))*100, # Percent renewable generation of total generation
                                  Percent_carbon_free = (Carbon_free/Total_gen)*100,        # Percent carbon-free generation of total elec. generation
                                  Not_renewable=Total_gen-Renewable,
                                  Carbon_emitting=Total_gen-Carbon_free
)]

# Renewable and carbon free percent gen---------------------------------------------------------------------
#This code is a mess and should be redone at some point.

percent_renewable_and_carbon_free <- eia_annual_data[!is.na(Percent_renewable),.(Year,Percent_renewable,Percent_carbon_free)]
lf_percent_renewable_and_carbon_free <- melt(percent_renewable_and_carbon_free,id="Year")
lf_percent_renewable <- eia_annual_data[!is.na(Percent_renewable),.(Year,variable = as.factor("Historic"), value=Percent_renewable)]
lf_percent_carbon_free <- eia_annual_data[!is.na(Percent_renewable),.(Year,variable = as.factor("Historic"), value=Percent_carbon_free)]

# APCO and Dominion RPS schedules are in the VCEA data.table
rps_mandate_schedule = melt(VCEA[,.(Year,apco_rps,dominion_rps)], id = "Year")
rps_mandate_schedule[,variable := gsub("apco_rps","APCO",variable)]
rps_mandate_schedule[,variable := gsub("dominion_rps","Dominion",variable)]

lf_percent_renewable_and_schedule_combined_dt <- merge(lf_percent_renewable,
                                                       rps_mandate_schedule,
                                                       by=c("Year","variable","value"),all=T)
#manually creating table of overall generation goals
#creating table for facet grid 
VCEA_goal_percent_gen = data.table(Year=c(2030,2040,2050,2060),
                                   Percent_renewable_goal=c(30,30,30,30),
                                   Percent_carbon_free_goal=c(NA,NA,100,100))
lf_VCEA_goal_percent_gen    <- melt(VCEA_goal_percent_gen,id="Year")
#creating table for regular line plot 
lf_VCEA_goal_percent_gen_dt <- melt(VCEA_goal_percent_gen,id="Year")


percent_renewable_carbon_free_combined <- merge(lf_percent_renewable_and_carbon_free[,.(Year,category=variable,historic=value)],
                                                lf_VCEA_goal_percent_gen[,.(Year,category=variable,goal=value)],by=c("Year","category"),all=T)
lf_percent_renewable_carbon_free_combined <- melt(percent_renewable_carbon_free_combined,id=c("Year","category"))
lf_percent_renewable_carbon_free_combined <- lf_percent_renewable_carbon_free_combined[!is.na(value)]
setnames(lf_percent_renewable_carbon_free_combined,old=c("variable","category"),new=c("category","variable"))

lf_percent_renewable_carbon_free_combined[,variable:=gsub("Percent_renewable","Renewable",variable)]
lf_percent_renewable_carbon_free_combined[,variable:=gsub("Percent_carbon_free","Carbon free",variable)]
lf_percent_renewable_carbon_free_combined[,category:=gsub("goal","Goal",category)]
lf_percent_renewable_carbon_free_combined[,category:=gsub("historic","Historic",category)]

# This code ensures that historic data will appear first then goal data
lf_percent_renewable_carbon_free_combined = lf_percent_renewable_carbon_free_combined[,category:=as.factor(category)]
setattr(lf_percent_renewable_carbon_free_combined$category,"levels",c("Historic","Goal"))

# Calculate the percent share of Dom & Apco sales of total sales in 2019
###    This set of calculations needs to be vetted - wms 1/16/2022
recent_year = max(va_utility_sales$Year)
dom_percent_share = va_utility_sales[Year==recent_year,dom_total_gwh/va_total_sales_gwh]
apco_percent_share = va_utility_sales[Year==recent_year,apco_total_gwh/va_total_sales_gwh]

#calculating weighted average of DOM and APCO rps
### Check the math here!!
VCEA_RPS = VCEA[,
        dom_and_apco_renewable:=(dominion_rps*100*dom_percent_share)+
          (apco_rps*100*apco_percent_share)]
#VCEA_renewable_portfolio_standards <- rbind(VCEA_renewable_portfolio_standards,list(2019,NA,NA,NA)) #adding a NA historic value so plot legend label is solid instead of dashed
lf_dom_apco_rps <- melt(VCEA_RPS[Year<=2030,.(Year,dom_and_apco_renewable)],id="Year")

lf_percent_renewable_carbon_free_combined_dt <- merge(lf_percent_renewable_and_carbon_free,
                                                      lf_VCEA_goal_percent_gen_dt,by=c("Year","variable","value"),all=T)
lf_percent_renewable_carbon_free_combined_dt <- merge(lf_percent_renewable_carbon_free_combined_dt,lf_dom_apco_rps,
                                                      by=c("Year","variable","value"),all=T)

lf_percent_renewable_carbon_free_combined_dt[variable=="Percent_renewable"|variable=="Percent_renewable_goal",variable:="VA renewable"]
lf_percent_renewable_carbon_free_combined_dt[variable=="Percent_carbon_free"|variable=="Percent_carbon_free_goal",variable:="VA carbon free"]

# APCO and Dominion historic sales vs VCEA goals----------------------------------------------------------------------------
# Retrieve apco and dom sales from va_utility_sales
apco_dom_historic_sales <- va_utility_sales[apco_total_gwh!=0,.(year, apco_total_gwh,dom_total_gwh)]
lf_apco_dom_historic_sales <- melt(apco_dom_historic_sales,id="year")

#manually creating table of sales goals
VCEA_goal_sales_reduction = data.table(year=c(2022,2023,2024,2025),
                                       apco_goal=c(14720.05985,14646.0897,14572.11955,14498.1494),
                                       dom_goal=c(79655.137125,78646.84425,77638.551375,76630.2585))
lf_VCEA_goal_sales_reduction <- melt(VCEA_goal_sales_reduction,id="year")
lf_VCEA_goal_sales_reduction_dt <- melt(VCEA_goal_sales_reduction,id="year")

apco_dom_sales_combined <- merge(lf_apco_dom_historic_sales[,.(year,category=variable,historic=value)],lf_VCEA_goal_sales_reduction[,.(year,category=variable,goal=value)],by=c("year","category"),all=T)
lf_apco_dom_sales_combined <- melt(apco_dom_sales_combined,id=c("year","category"))
lf_apco_dom_sales_combined <- lf_apco_dom_sales_combined[!is.na(value)]

#setnames(lf_apco_dom_sales_combined,old=c("variable","category"),new=c("category","variable"))

lf_apco_dom_sales_combined[,variable:=gsub("apco_total_gwh","APCO, historic",variable)]
lf_apco_dom_sales_combined[,variable:=gsub("dom_total_gwh","Dominion, historic",variable)]
lf_apco_dom_sales_combined[,variable:=gsub("apco_goal","APCO, goal",variable)]
lf_apco_dom_sales_combined[,variable:=gsub("dom_goal","Dominion, goal",variable)]
lf_apco_dom_sales_combined[,category:=gsub("goal","Goal",category)]
lf_apco_dom_sales_combined[,category:=gsub("historic","Historic",category)]


lf_apco_dom_sales_combined_dt <- merge(lf_apco_dom_historic_sales,lf_VCEA_goal_sales_reduction_dt,by=c("year","variable","value"),all=T)
lf_apco_dom_sales_combined_dt[variable=="apco_total_gwh"|variable=="apco_goal",variable:="APCO"]
lf_apco_dom_sales_combined_dt[variable=="dom_total_gwh"|variable=="dom_goal",variable:="Dominion"]

# below code ensures that historic data will appear first then goal data
lf_apco_dom_sales_combined = lf_apco_dom_sales_combined[,category:=as.factor(category)]
setattr(lf_apco_dom_sales_combined$category,"levels",c("Historic","Goal"))

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
#Energy Storage
# Currently this is battery storage only. VCEA specifies battery storage amounts
# The display of storage may benefit from a little rethinking
va_storage <- plant_capacities[Prime_Mover=="BA",
                               .(id,capacity_mw = Nameplate_Capacity_MW,
                                 Plant_Name,Operating_Year)]

# the storage_capacity_projections calculations do not appear to be used
# storage_capacity_projections <- pjm_storage_working[status=="In Service",.(date=actual_in_service_date,storage=mfo)]
# storage_capacity_projections <- merge(storage_capacity_projections,pjm_storage_working[status=="Active",.(date=projected_in_service_date,storage=mfo)],by=c("date","storage"),all=T)
# 
# storage_capacity_projections <- storage_capacity_projections[,.(storage=sum(storage)),by=date]
# storage_capacity_projections <- storage_capacity_projections[,.(date,storage=cumsum(storage))]
# 
# lf_storage_capacity_projections <- melt(storage_capacity_projections,id="date")


# Need to fix how on-shore and off-shore wind are derived
# Currently there is no on-shore wind in Virginia
# The 12 MW of off-shore wind in service in 2021 is handled manually at the plot
va_wind <- plant_capacities[Prime_Mover=="WT",
                                     .(id,capacity_mw = Nameplate_Capacity_MW,
                                        Plant_Name,Operating_Year)]
# Utility target values for onshore wind and solar
VCEA_onshore_wind_solar = VCEA[,.(date=as.Date(paste0(year,"-01-01")),
                apco_onshore_wind_and_solar_mw,dominion_onshore_wind_and_solar_mw)]
VCEA_onshore_wind_solar %>% tidyr::fill(everything())
setnames(VCEA_onshore_wind_solar,old=c("apco_onshore_wind_and_solar_mw","dominion_onshore_wind_and_solar_mw"),
         new=c("target_apco_onshore_wind_and_solar","target_dom_onshore_wind_and_solar"))

# Projected Offshore Wind Capacity
total_mw_offshore_wind = offshore_wind_data[,.(Year,CVOW_Pilot,CVOW_Stage_I,CVOW_Stage_II,CVOW_Stage_III,Total=Total_mw)]  #,
total_mw_offshore_wind[Year<2022,`:=`(CVOW_Stage_I=NA,CVOW_Stage_II=NA,
                                      CVOW_Stage_III=NA)] 
total_mw_offshore_wind[Year<2020] <- NA
total_production_forecast_offshore_wind <- offshore_wind_data[,.(Year,Total_gen)]  #Stage_1_gen,Stage_2_gen,Stage_3_gen,
setnames(total_production_forecast_offshore_wind,'Total_gen','Total_Production')

total_mw_offshore_wind<-melt(total_mw_offshore_wind,id="Year")
# Remove underscores from variable names
total_mw_offshore_wind[,variable:=gsub("_"," ",variable)]

#---------------------------------------------------------------------------------------------------------

#For energy efficiency figures--------------------------------------------------------------------------------------------
#renaming columns so it can be accepted as input into piechart function
setnames(virginia_annual_savings_through_2020,old=c("Company Name","MWh"),new=c("variable","value"))
virginia_annual_savings_through_2020 = virginia_annual_savings_through_2020[,year:=2020
                                                     ][variable!="Total Needed"]

setnames(virginia_annual_savings_through_2022,old=c("Company Name","MWh"),new=c("variable","value"))
virginia_annual_savings_through_2022 = virginia_annual_savings_through_2022[ ,`:=`(year=2022,
                                                        variable = gsub("Dominion$","Dominion (Gross savings)",variable)
                                                      )][variable!="Total Needed"]

#manipulating datasets for stacked bar chart
virginia_annual_savings_2020_2022<-rbind(virginia_annual_savings_through_2020,virginia_annual_savings_through_2022)
virginia_annual_savings_2020_2022[,variable := gsub("DMME programs","Virginia Energy programs",variable)]
setattr(virginia_annual_savings_2020_2022$variable,"levels",
        c("Remaining Needed","APCO","C-PACE", "Virginia Energy programs","Dominion (Gross savings)","Energy Codes (modeled, adoption of 2015 IECC)","ESPCs  (modeled, MUSH and private)"))


#-----------------------------------------REFORMATTING DATASETS--------------------------------------------------------------------
#
# reformatting the generation dataset
cols = c("Year", "Coal", "Oil", "Gas", "Nuclear",
         "Solar_utility", "Solar_distributed", "Hydropower",
         "Wind", "Wood", "Other_biomass", "Total_gen")
va_annual_generation = eia_annual_data[,..cols]
va_gen_w_commas <- va_annual_generation[,lapply(.SD,format,big.mark=",",scientific=FALSE,trim=TRUE),
                                                .SDcols = cols[2:length(cols)],by=Year]
gen_names <- names(va_gen_w_commas)
good_gen_names <- capitalize(gsub("_"," ", gen_names))
names(va_gen_w_commas) <- good_gen_names

# reformatting the consumption dataset
cols = c("Year","Residential","Commercial","Industrial","Transportation")
va_annual_consumption <- eia_annual_data[,..cols]
va_con_w_commas <- va_annual_consumption[,lapply(.SD,format,big.mark=",",scientific=FALSE,trim=TRUE),
                                        .SDcols = cols[2:length(cols)],by=Year]
#   
# 
#reformatting carbon emissions from electricity sector
virginia_emissions_electric <- eia_annual_data[Electric_sector_CO2_emissions!=0,.(Year,Electric_sector_CO2_emissions)]
virginia_emissions_electric_commas <- virginia_emissions_electric[,
            Electric_sector_CO2_emissions:=signif(Electric_sector_CO2_emissions, digits=4)]
setnames(virginia_emissions_electric_commas,c('Year','Million Metric Tons of CO2'))

#reformatting emissions compounds data
### CHECK UNITS in display. The EIA data is in metric tons
# Not sure why data is limited to after year 2000. May want to change this.
# This data is redundant. Equivalent but more detailed data is in va_electricity_emissions_by_fuel
# va_annual_emissions = va_annual_emissions[Year>=2000,.(Year,CO2=CO2_Metric_Tons,
#                                                 SO2=SO2_Metric_Tons,
#                                                 NOx=SO2_Metric_Tons)]
#reformatting emissions compounds dataset

# Data is in metric tons (emission rates are also adjusted to be based on metric tons)
#va_electricity_emissions_by_fuel = va_electricity_emissions_by_fuel[Year >= 2000] #limit data to baseline year of 2000



