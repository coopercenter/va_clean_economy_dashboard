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

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

#load in metadata
metadata <- data.table(dbGetQuery(db,"select * from metadata ;"))

# Energy Efficiency dataset
investment_by_IOUs <- data.table(dbGetQuery(db,"select * from current_ee_programs ;"))

#load in capacity by fuel type data (likely will be replaced if we find better solar data)
whole_electric_industry_capacity <- data.table(dbGetQuery(db,"select * from whole_electric_industry_capacity ;"))

#load in data on multiple types of emission compounds 
emissions_co2_by_source_va <- data.table(dbGetQuery(db, "select * from emissions_co2_by_source_va ;")) #units = thousand metric tons
emissions_no_by_source_va <- data.table(dbGetQuery(db, "select * from emissions_no_by_source_va ;")) #units = short tons
emissions_so2_by_source_va <- data.table(dbGetQuery(db, "select * from emissions_so2_by_source_va ;")) #units = short tons
virginia_ghg <- data.table(dbGetQuery(db, "select * from virginia_ghg ;")) 

#load in energy equity data
#energy_burden_county_percent_income <- data.table(dbGetQuery(db,"select * from energy_burden_county_percent_income ;"))
#energy_burden_county_expenditures <- data.table(dbGetQuery(db,"select * from energy_burden_county_expenditures ;"))

#load in energy efficiency data
energy_consumption_per_capita_va <- data.table(dbGetQuery(db,"select * from energy_consumption_per_capita_va ;"))
energy_consumption_per_unit_gdp_va <- data.table(dbGetQuery(db,"select * from energy_consumption_per_unit_gdp_va ;"))
co2_emission_per_capita_va <- data.table(dbGetQuery(db,"select * from co2_emission_per_capita_va ;"))
co2_emission_per_thousand_dollars_of_gdp_va <- data.table(dbGetQuery(db,"select * from co2_emission_per_thousand_dollars_of_gdp_va ;"))
virginia_annual_savings_through_2022 <- data.table(dbGetQuery(db,"select * from virginia_annual_savings_through_2022 ;"))
virginia_annual_savings_through_2020 <- data.table(dbGetQuery(db,"select * from virginia_annual_savings_through_2020 ;"))
apco_dom_VCEA_goals<-data.table(dbGetQuery(db,"select * from \"VCEA_energy_efficiency\" ;"))

#load in offshore wind projections
total_mw_offshore_wind <- data.table(dbGetQuery(db,"select * from total_mw_offshore_wind ;"))
total_production_forecast_offshore_wind <- data.table(dbGetQuery(db,"select * from total_production_forecast_offshore_wind ;"))

#load in pjm solar and wind data & apco/dominion goals
pjm_solar <- data.table(dbGetQuery(db,"select * from pjm_solar ;"))
pjm_wind <- data.table(dbGetQuery(db,"select * from pjm_wind ;"))
pjm_storage <- data.table(dbGetQuery(db,"select * from pjm_storage ;"))
VCEA_onshore_wind_solar <- data.table(dbGetQuery(db,"select * from \"VCEA_onshore_wind_solar\" ;"))

#load in APCO and Dominion historic sales
apco_dom_sales<-data.table(dbGetQuery(db,"select * from elec_sales_through_2019_annual ;"))

#load in VA electricity imports
va_elec_import<-data.table(dbGetQuery(db,"select * from eia_seds_elisp_va_a ;"))[,date:=NULL]
setnames(va_elec_import,"year","Year")

#load in APCO & Dom RPS
VCEA_renewable_portfolio_standards<-data.table(dbGetQuery(db,"select * from \"VCEA_renewable_portfolio_standards\" ;"))
setnames(VCEA_renewable_portfolio_standards,"year","Year")

#load in utility sales data
va_utility_sales <- data.table(dbGetQuery(db,"select * from va_utility_sales ;"))
setnames(va_utility_sales,"year","Year")

rps_mandate_schedule <- data.table(dbGetQuery(db,"select * from clean_energy_renewable_goals ;"))

###
# Load EIA annual time series table
#load in VA electricity imports
eia_annual_data <-data.table(dbGetQuery(db,"select * from eia_annual_data ;"))

# End of db access for now
dbDisconnect(db)
#
# Name mapping
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
           "SEDS_TERCB_VA_A",
           "SEDS_TECCB_VA_A",
           "SEDS_TEICB_VA_A",
           "SEDS_TEACB_VA_A",
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
             "Residential",
             "Commercial",
             "Industrial",
             "Transportation",
             "Electric_sector_CO2_emissions",
             "Total_CO2_emissions")

setnames(eia_annual_data,
         eia_name, local_name)
# Some additions and adjustments
eia_annual_data[Solar_utility == 0,Solar_utility:=NA] #random fix for visual purposes later on
# Not sure if this should be done here or possibly closer to display time
eia_annual_data[is.na(eia_annual_data)]=0
### Check the energy math in the "other" calculation. For one thing, why is dpv in here?
### Should we net out pumped storage electricity use?
# Total annual carbon-free generation (incl. dpv)
# Note: the  definition of renewable is only wind, solar and hydro
eia_annual_data[,`:=`(Year = year(date),
                      All_solar = Solar_distributed +  Solar_utility,
                      Other = Total_gen-(Coal+Oil+Gas+Nuclear+Solar_utility+Solar_distributed+Hydropower+Wind+Wood+Other_biomass),
                      Carbon_free = Wind+Hydropower+Solar_distributed+Solar_utility+Nuclear,     
                      Renewable = Wind+Solar_distributed+Solar_utility+Hydropower                
)]
eia_annual_data[Total_gen!=0,`:=`(Percent_renewable = (Renewable/(Total_gen-Nuclear))*100, # Percent renewable generation of total generation
                                  Percent_carbon_free = (Carbon_free/Total_gen)*100,        # Percent carbon-free generation of total elec. generation
                                  Not_renewable=Total_gen-Renewable,
                                  Carbon_emitting=Total_gen-Carbon_free
)]

# Renewable and carbon free percent gen---------------------------------------------------------------------
percent_renewable_and_carbon_free <- eia_annual_data[,.(Year,Percent_renewable,Percent_carbon_free)]
lf_percent_renewable_and_carbon_free <- melt(percent_renewable_and_carbon_free,id="Year")
lf_percent_renewable <- melt(eia_annual_data[,.(Year,Percent_renewable)],id="Year")
lf_percent_carbon_free <- melt(eia_annual_data[,.(Year,Percent_carbon_free)],id="Year")

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

# below code ensures that historic data will appear first then goal data
lf_percent_renewable_carbon_free_combined = lf_percent_renewable_carbon_free_combined[,category:=as.factor(category)]
setattr(lf_percent_renewable_carbon_free_combined$category,"levels",c("Historic","Goal"))

#calculating percent share of Dom & Apco sales of total sales in 2019

recent_year = max(va_utility_sales$Year)
total_sales = va_utility_sales[Year==recent_year,sum(tot_sales_mwh)]
dom_percent_share = va_utility_sales[Year==recent_year&utility_name=="dominion",sum(tot_sales_mwh)]/total_sales
apco_percent_share = va_utility_sales[Year==recent_year&utility_name=="apco",sum(tot_sales_mwh)]/total_sales

#calculating weighted average of Dom and APCO rps
VCEA_renewable_portfolio_standards[,`:=`(apco_rps_pct=apco_rps*100,
                                         dominion_rps_pct=dominion_rps*100)]#converting to percent rather than decimal for consistency with other data
VCEA_renewable_portfolio_standards[,dom_and_apco_renewable:=dominion_rps_pct*dom_percent_share+apco_rps_pct*apco_percent_share]
#VCEA_renewable_portfolio_standards <- rbind(VCEA_renewable_portfolio_standards,list(2019,NA,NA,NA)) #adding a NA historic value so plot legend label is solid instead of dashed
lf_dom_apco_rps <- melt (VCEA_renewable_portfolio_standards[Year<=2030,.(Year,dom_and_apco_renewable)],id="Year")

lf_percent_renewable_carbon_free_combined_dt <- merge(lf_percent_renewable_and_carbon_free,lf_VCEA_goal_percent_gen_dt,by=c("Year","variable","value"),all=T)
lf_percent_renewable_carbon_free_combined_dt <- merge(lf_percent_renewable_carbon_free_combined_dt,lf_dom_apco_rps,by=c("Year","variable","value"),all=T)

lf_percent_renewable_carbon_free_combined_dt[variable=="percent_renewable"|variable=="percent_renewable_goal",variable:="VA renewable"]
lf_percent_renewable_carbon_free_combined_dt[variable=="percent_carbon_free"|variable=="percent_carbon_free_goal",variable:="VA carbon free"]

# APCO and Dominion historic sales vs VCEA goals----------------------------------------------------------------------------
apco_dom_historic_sales <- apco_dom_sales %>% 
  filter(
    as.numeric(apco_total_gwh) != 0
  )

lf_apco_dom_historic_sales <- melt(apco_dom_historic_sales[,.(year, apco_total_gwh,dom_total_gwh)],id="year")

#manually creating table of sales goals
VCEA_goal_sales_reduction = data.table(year=c(2022,2023,2024,2025),
                                       apco_goal=c(14720.05985,14646.0897,14572.11955,14498.1494),
                                       dom_goal=c(79655.137125,78646.84425,77638.551375,76630.2585))
lf_VCEA_goal_sales_reduction <- melt(VCEA_goal_sales_reduction,id="year")
lf_VCEA_goal_sales_reduction_dt <- melt(VCEA_goal_sales_reduction,id="year")

apco_dom_sales_combined <- merge(lf_apco_dom_historic_sales[,.(year,category=variable,historic=value)],lf_VCEA_goal_sales_reduction[,.(year,category=variable,goal=value)],by=c("year","category"),all=T)
lf_apco_dom_sales_combined <- melt(apco_dom_sales_combined,id=c("year","category"))
lf_apco_dom_sales_combined <- lf_apco_dom_sales_combined[!is.na(value)]

setnames(lf_apco_dom_sales_combined,old=c("variable","category"),new=c("category","variable"))

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
pjm_solar_working <- pjm_solar
pjm_solar_names <- names(pjm_solar_working)
pjm_solar_good_names <- tolower(gsub(" ","_", pjm_solar_names))
names(pjm_solar_working) <- pjm_solar_good_names 

pjm_wind_working <- pjm_wind
pjm_wind_names <- names(pjm_wind_working)
pjm_wind_good_names <- tolower(gsub(" ","_", pjm_wind_names))
names(pjm_wind_working) <- pjm_wind_good_names 

pjm_wind_working[,projected_in_service_date:=as.Date(projected_in_service_date,"%m/%d/%Y")]
pjm_solar_working[,projected_in_service_date:=as.Date(projected_in_service_date,"%m/%d/%Y")]
pjm_solar_working[,actual_in_service_date:=as.Date(actual_in_service_date,"%m/%d/%Y")]

VCEA_onshore_wind_solar[,date:=as.Date(paste0(year,"-01-01"))]
VCEA_onshore_wind_solar %>% tidyr::fill(everything())
setnames(VCEA_onshore_wind_solar,old=c("apco_onshore_wind_and_solar_mw","dominion_onshore_wind_and_solar_mw"),new=c("target_apco_onshore_wind_and_solar","target_dom_onshore_wind_and_solar"))

# apco_dom_onwind_and_solar <- pjm_wind_working[transmission_owner=="AEP",.(date=projected_in_service_date,apco_onshore_wind=mfo)] #AEP is owner of APCO
# #note: Dominion currently only has offshore wind in development
# apco_dom_onwind_and_solar <- merge(apco_dom_onwind_and_solar,pjm_solar_working[transmission_owner=="AEP",.(date=projected_in_service_date,apco_solar=mfo)],id="date",all=TRUE) #APCO has no in service solar plant
# apco_dom_onwind_and_solar <- merge(apco_dom_onwind_and_solar,pjm_solar_working[transmission_owner=="Dominion"&status=="In Service",.(date=actual_in_service_date,dom_solar=mfo)],by="date",all=TRUE,allow.cartesian=TRUE)
# apco_dom_onwind_and_solar <- merge(apco_dom_onwind_and_solar,pjm_solar_working[transmission_owner=="Dominion"&status=="Active",.(date=projected_in_service_date,dom_solar=mfo)],by=c("date","dom_solar"),all=TRUE,allow.cartesian=TRUE)
# 
# apco_dom_onwind_and_solar <- apco_dom_onwind_and_solar[,.(apco_onshore_wind=sum(apco_onshore_wind,na.rm=T),
#                                                           apco_solar=sum(apco_solar,na.rm=T),
#                                                           dom_solar=sum(dom_solar,na.rm=T)),by=date]
# 
# apco_dom_onwind_and_solar <- apco_dom_onwind_and_solar[,.(date,
#                                                           apco_onshore_wind=cumsum(apco_onshore_wind),
#                                                           apco_solar=cumsum(apco_solar),
#                                                           dom_solar=cumsum(dom_solar))]
# 
# apco_dom_onwind_and_solar[,apco_onshore_wind_and_solar:=apco_onshore_wind+apco_solar]
# 
# apco_dom_onwind_and_solar <- merge(apco_dom_onwind_and_solar,VCEA_onshore_wind_solar[,.(date,target_apco_onshore_wind_and_solar,target_dom_onshore_wind_and_solar)],id="date",all=T)
# 
# apco_dom_onwind_and_solar %>% filter(!is.na(apco_onshore_wind)) %>% fill(everything())
# 
# apco_dom_onwind_and_solar[date<='2023-12-01',`:=`(apco_solar=na.locf(apco_solar),
#                                                   apco_onshore_wind=na.locf(apco_onshore_wind),
#                                                   apco_onshore_wind_and_solar=na.locf(apco_onshore_wind_and_solar),
#                                                   dom_solar=na.locf(dom_solar))]
# 
# apco_dom_onwind_and_solar[apco_dom_onwind_and_solar==0]=NA #making 0 values NA for graphing purposes

# lf_apco_dom_onwind_and_solar <- melt(apco_dom_onwind_and_solar,id="date")

# This appears not to be used
# wind_and_solar_capacity_projections <- pjm_wind_working[fuel=="Wind",.(date=projected_in_service_date,onshore_wind=mfo)] 
# wind_and_solar_capacity_projections <- merge(wind_and_solar_capacity_projections,pjm_wind_working[fuel=="Offshore Wind",.(date=projected_in_service_date,offshore_wind=mfo)],by="date",all=T)
# wind_and_solar_capacity_projections <- merge(wind_and_solar_capacity_projections,pjm_solar_working[status=="In Service",.(date=actual_in_service_date,solar=mfo)],by="date",all=T)
# wind_and_solar_capacity_projections <- merge(wind_and_solar_capacity_projections,pjm_solar_working[status!="In Service",.(date=projected_in_service_date,solar=mfo)],by=c("date","solar"),all=T)
# 
# wind_and_solar_capacity_projections <- wind_and_solar_capacity_projections[,.(onshore_wind=sum(onshore_wind,na.rm = T),
#                                                                               # offshore_wind=sum(offshore_wind,na.rm = T),
#                                                                               # solar=sum(solar,na.rm = T)),by=date]
# 
# wind_and_solar_capacity_projections <- wind_and_solar_capacity_projections[,.(date,
#                                                                               offshore_wind=cumsum(offshore_wind),
#                                                                               onshore_wind=cumsum(onshore_wind),
#                                                                               solar=cumsum(solar))]
# 
# wind_and_solar_capacity_projections[wind_and_solar_capacity_projections==0]=NA #making 0 values NA for graphing purposes
# 
# lf_wind_and_solar_capacity_projections <-melt(wind_and_solar_capacity_projections,id="date")

# Projected Wind Capacity
total_mw_offshore_wind<-melt(total_mw_offshore_wind,id="Year")
# Remove underscores from variable names
total_mw_offshore_wind[,variable:=gsub("_"," ",variable)]


#Energy Storage
# This should be replaced with EIA capacity data
pjm_storage_working <- pjm_storage
pjm_storage_names <- names(pjm_storage_working)
pjm_storage_good_names <- tolower(gsub(" ","_", pjm_storage_names))
names(pjm_storage_working) <- pjm_storage_good_names
pjm_storage_working = pjm_storage_working[,.(projected_in_service_date,actual_in_service_date,mfo,status)]

pjm_storage_working[,projected_in_service_date:=as.Date(projected_in_service_date,"%m/%d/%Y")]
pjm_storage_working[,actual_in_service_date:=as.Date(actual_in_service_date,"%m/%d/%Y")]
# the storage_capacity_projections calculations do not appear to be used
# storage_capacity_projections <- pjm_storage_working[status=="In Service",.(date=actual_in_service_date,storage=mfo)]
# storage_capacity_projections <- merge(storage_capacity_projections,pjm_storage_working[status=="Active",.(date=projected_in_service_date,storage=mfo)],by=c("date","storage"),all=T)
# 
# storage_capacity_projections <- storage_capacity_projections[,.(storage=sum(storage)),by=date]
# storage_capacity_projections <- storage_capacity_projections[,.(date,storage=cumsum(storage))]
# 
# lf_storage_capacity_projections <- melt(storage_capacity_projections,id="date")

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
# Formatting should be done at display time, not here.
# reformatting the generation dataset
cols = c("Year",
         "Coal",
         "Oil",
         "Gas",
         "Nuclear",
         "Solar_utility",
         "Solar_distributed",
         "Hydropower",
         "Wind",
         "Wood",
         "Other_biomass",
         "Total_gen")
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

#reformatting emissions compounds dataset
va_emissions_compounds <- merge(emissions_co2_by_source_va[,.(Year=year,CO2=total/1000)],emissions_no_by_source_va[,.(Year=year,NO=total/1102311.31)],id="Year")
va_emissions_compounds <- merge(va_emissions_compounds,emissions_so2_by_source_va[,.(Year=year,SO2=total/1102311.31)],id="Year")
va_emissions_compounds <- va_emissions_compounds[Year >= 2000] #limit data to baseline year of 2000

colnames(rps_mandate_schedule) <- c('Year', 'variable', 'value')
rps_mandate_schedule <- rps_mandate_schedule[variable=="Appalachian",variable:="APCO"]

lf_percent_renewable$variable <- as.character(lf_percent_renewable$variable)
lf_percent_renewable$variable[lf_percent_renewable$variable == 'percent_renewable'] <- 'Historic'
lf_percent_renewable$variable <- as.factor(lf_percent_renewable$variable)
#lf_percent_renewable_and_schedule_combined_dt <- merge(lf_percent_renewable,rps_mandate_schedule,by=c("year","variable","value"),all=T)


