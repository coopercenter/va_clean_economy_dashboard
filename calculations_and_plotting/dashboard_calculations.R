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
energy_burden_county_percent_income <- data.table(dbGetQuery(db,"select * from energy_burden_county_percent_income ;"))
energy_burden_county_expenditures <- data.table(dbGetQuery(db,"select * from energy_burden_county_expenditures ;"))

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
va_elec_import<-data.table(dbGetQuery(db,"select * from eia_seds_elisp_va_a ;"))

#load in APCO & Dom RPS
VCEA_renewable_portfolio_standards<-data.table(dbGetQuery(db,"select * from \"VCEA_renewable_portfolio_standards\" ;"))

#load in utility sales data
va_utility_sales <- data.table(dbGetQuery(db,"select * from va_utility_sales ;"))

rps_mandate_schedule <- data.table(dbGetQuery(db,"select * from clean_energy_renewable_goals ;"))

#function to fetch data from a specified db table; return as a data table & rename 'value' column with descriptive name
fetch_time_series_from_db <- function(db_table_name, value_description, con){
  sql_script  <- paste0("select value, date from ",db_table_name," where year <= ",max_eia_annual_data_year," ;")
  print(sql_script)
  dt <- data.table(dbGetQuery(con, sql_script))
  dt <- dt[,.(year=year(date),value)]
  setnames(dt, "value", value_description)
  
  return(dt)
}

#custom color palette
# need a new color for wind.
ceps_pal <- c("#00A087B2", "#3C5488B2", "#CEA5AC", "#BE7E8A", "#4DBBD5B2", "#91D1C2B2","#D9C6C9","#8491B4B2","#5868AC","#6FB3D9","#56BD96","#99A9E2","#A94F64","#B0DEFA","#99EEBB","#8FD3FE")

# Creating a data frame called `table_list` that includes two columns:
#      * `table_name`: the list of db table names with data to be pulled from db
#      * `value_name` : associated name of 'value', e.g., "coal"
table_list = data.frame(
  table_name=c("eia_elec_gen_cow_va_99_a",
               "eia_elec_gen_pel_va_99_a",
               "eia_elec_gen_ng_va_99_a",
               "eia_elec_gen_nuc_va_99_a",
               "eia_elec_gen_sun_va_99_a",
               "eia_elec_gen_dpv_va_99_a",
               "eia_elec_gen_hyc_va_99_a",
               "eia_elec_gen_wnd_va_99_a",
               "eia_elec_gen_www_va_99_a",
               "eia_elec_gen_was_va_99_a",
               "eia_elec_gen_all_va_99_a",
               "eia_seds_tercb_va_a",
               "eia_seds_teccb_va_a",
               "eia_seds_teicb_va_a",
               "eia_seds_teacb_va_a",
               "eia_emiss_co2_totv_ec_to_va_a",
               "eia_emiss_co2_totv_tt_to_va_a"),
  value_name=c("coal",
               "oil",
               "gas",
               "nuclear",
               "solar_utility", 
               "solar_distributed",
               "hydropower",
               "wind",
               "wood",
               "other_biomass",
               "total",
               "residential",
               "commercial",
               "industrial",
               "transportation",
               "electric_sector_CO2_emissions",
               "total_CO2_emissions")
)
table_list$value_name <- as.character(table_list$value_name)
table_list$table_name <- as.character(table_list$table_name)
# sql_script  <- paste0("select value, date from eia_elec_gen_wnd_va_99_a ;")
# print(sql_script)
# dt <- data.table(dbGetQuery(con, sql_script))

#loading in data from database that is listed in table_list
for(row in 1:nrow(table_list)){
  table <- table_list[row,"table_name"]
  name_of_value <- table_list[row,"value_name"]
  
  dt <- fetch_time_series_from_db(table, name_of_value, db)
  assign(table,dt)
}

dbDisconnect(db)

eia_elec_gen_sun_va_99_a[solar_utility == 0,solar_utility:=NA] #random fix for visual purposes later on

# Isolating renewable and carbon free generation in it's own table -----------------------------------------------
renewable_and_carbon_free_list <- list(eia_elec_gen_nuc_va_99_a,
                                       eia_elec_gen_sun_va_99_a,
                                       eia_elec_gen_dpv_va_99_a,
                                       eia_elec_gen_hyc_va_99_a,
                                       eia_elec_gen_wnd_va_99_a,
                                       eia_elec_gen_all_va_99_a)

va_annual_renewable_and_carbon_free_gen <- NULL

for(table in renewable_and_carbon_free_list){
  if (is.null(va_annual_renewable_and_carbon_free_gen))
  {va_annual_renewable_and_carbon_free_gen <- table}
  else
  {va_annual_renewable_and_carbon_free_gen <- merge(va_annual_renewable_and_carbon_free_gen, table[], by = "year", all=TRUE)}
}

va_annual_renewable_and_carbon_free_gen[is.na(va_annual_renewable_and_carbon_free_gen)]=0
va_annual_renewable_and_carbon_free_gen[,all_solar:=solar_distributed+solar_utility]

# Creating 'other' generation measure by combining all by fuel type generation and total generation in a table to calculate other generation over time
gen_by_fuel_type_list <- list(eia_elec_gen_cow_va_99_a,
                              eia_elec_gen_pel_va_99_a,
                              eia_elec_gen_ng_va_99_a,
                              eia_elec_gen_nuc_va_99_a,
                              eia_elec_gen_sun_va_99_a,
                              eia_elec_gen_dpv_va_99_a,
                              eia_elec_gen_hyc_va_99_a,
                              eia_elec_gen_wnd_va_99_a,
                              eia_elec_gen_www_va_99_a,
                              eia_elec_gen_was_va_99_a,
                              eia_elec_gen_all_va_99_a)

va_annual_generation <- NULL

for(table in gen_by_fuel_type_list){
  if (is.null(va_annual_generation))
  {va_annual_generation <- table}
  else
  {va_annual_generation <- merge(va_annual_generation, table[], by = "year", all=TRUE)}
}

va_annual_generation[is.na(va_annual_generation)]=0
va_annual_generation[,other:=total-(coal+oil+gas+nuclear+solar_utility+solar_distributed+hydropower+wind+wood+other_biomass)]
other_annual_generation <- va_annual_generation[,.(year,other)]

# Finding sum of total annual renewable generation----------------------------------------------------------------
va_annual_renewable_and_carbon_free_gen[,renewable:=wind+all_solar+hydropower]

# Finding total annual renewable generation as a percent of total energy generation--------------------------------
va_annual_renewable_and_carbon_free_gen[,percent_renewable:=(renewable/(total-nuclear))*100]

# Finding sum of total annual carbon-free generation--------------------------------------------------------------
va_annual_renewable_and_carbon_free_gen[,carbon_free:=wind+hydropower+all_solar+nuclear]

# Finding total annual carbon-free generation as a percent of total energy generation--------------------------------------
va_annual_renewable_and_carbon_free_gen[,percent_carbon_free:=(carbon_free/total)*100]

# Renewable and carbon free percent gen---------------------------------------------------------------------
lf_percent_renewable_and_carbon_free <- melt(va_annual_renewable_and_carbon_free_gen[,.(year,percent_renewable,percent_carbon_free)],id="year")
lf_percent_renewable <- melt(va_annual_renewable_and_carbon_free_gen[,.(year,percent_renewable)],id="year")
lf_percent_carbon_free <- melt(va_annual_renewable_and_carbon_free_gen[,.(year,percent_carbon_free)],id="year")

#manually creating table of overall generation goals
#creating table for facet grid 
VCEA_goal_percent_gen = data.table(year=c(2030,2040,2050,2060),
                                   percent_renewable=c(30,30,30,30),
                                   percent_carbon_free=c(NA,NA,100,100))
lf_VCEA_goal_percent_gen <- melt(VCEA_goal_percent_gen,id="year")

percent_renewable_carbon_free_combined <- merge(lf_percent_renewable_and_carbon_free[,.(year,category=variable,historic=value)],lf_VCEA_goal_percent_gen[,.(year,category=variable,goal=value)],by=c("year","category"),all=T)
lf_percent_renewable_carbon_free_combined <- melt(percent_renewable_carbon_free_combined,id=c("year","category"))
lf_percent_renewable_carbon_free_combined <- lf_percent_renewable_carbon_free_combined[!is.na(value)]

setnames(lf_percent_renewable_carbon_free_combined,old=c("variable","category"),new=c("category","variable"))

lf_percent_renewable_carbon_free_combined[,variable:=gsub("percent_renewable","Renewable",variable)]
lf_percent_renewable_carbon_free_combined[,variable:=gsub("percent_carbon_free","Carbon free",variable)]
lf_percent_renewable_carbon_free_combined[,category:=gsub("goal","Goal",category)]
lf_percent_renewable_carbon_free_combined[,category:=gsub("historic","Historic",category)]

# below code ensures that historic data will appear first then goal data
lf_percent_renewable_carbon_free_combined <- lf_percent_renewable_carbon_free_combined %>% 
  arrange(desc(category)) %>%
  mutate_at(vars(category), funs(factor(., levels=unique(.))))

lf_percent_renewable_carbon_free_combined <- as.data.table(lf_percent_renewable_carbon_free_combined)

#creating table for regular line plot 
VCEA_goal_percent_gen_dt = data.table(year=c(2030,2040,2050,2060),
                                      percent_renewable_goal=c(30,30,30,30),
                                      percent_carbon_free_goal=c(NA,NA,100,100))
lf_VCEA_goal_percent_gen_dt <- melt(VCEA_goal_percent_gen_dt,id="year")

#calculating percent share of Dom & Apco sales of total sales in 2019
recent_year = va_utility_sales %>% select(year) %>% arrange(year)
recent_year = recent_year[[nrow(va_utility_sales),1]]


total_sales = va_utility_sales[year==recent_year,sum(tot_sales_mwh)]
dom_percent_share = va_utility_sales[year==recent_year&utility_name=="dominion",sum(tot_sales_mwh)]/total_sales
apco_percent_share = va_utility_sales[year==recent_year&utility_name=="apco",sum(tot_sales_mwh)]/total_sales

#calculating weighted average of Dom and APCO rps
VCEA_renewable_portfolio_standards[,`:=`(apco_rps=apco_rps*100,
                                         dominion_rps=dominion_rps*100)]#converting to percent rather than decimal for consistency with other data
VCEA_renewable_portfolio_standards[,dom_and_apco_renewable:=dominion_rps*dom_percent_share+apco_rps*apco_percent_share]
VCEA_renewable_portfolio_standards <- rbind(VCEA_renewable_portfolio_standards,list(2019,NA,NA,NA)) #adding a NA historic value so plot legend label is solid instead of dashed
lf_dom_apco_rps <- melt (VCEA_renewable_portfolio_standards[year<=2030,.(year,dom_and_apco_renewable)],id="year")

lf_percent_renewable_carbon_free_combined_dt <- merge(lf_percent_renewable_and_carbon_free,lf_VCEA_goal_percent_gen_dt,by=c("year","variable","value"),all=T)
lf_percent_renewable_carbon_free_combined_dt <- merge(lf_percent_renewable_carbon_free_combined_dt,lf_dom_apco_rps,by=c("year","variable","value"),all=T)

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

VCEA_goal_sales_reduction_dt = data.table(year=c(2022,2023,2024,2025),
                                          apco_goal=c(14720.05985,14646.0897,14572.11955,14498.1494),
                                          dom_goal=c(79655.137125,78646.84425,77638.551375,76630.2585))
lf_VCEA_goal_sales_reduction_dt <- melt(VCEA_goal_sales_reduction_dt,id="year")

lf_apco_dom_sales_combined_dt <- merge(lf_apco_dom_historic_sales,lf_VCEA_goal_sales_reduction_dt,by=c("year","variable","value"),all=T)
lf_apco_dom_sales_combined_dt[variable=="apco_total_gwh"|variable=="apco_goal",variable:="APCO"]
lf_apco_dom_sales_combined_dt[variable=="dom_total_gwh"|variable=="dom_goal",variable:="Dominion"]

# below code ensures that historic data will appear first then goal data
lf_apco_dom_sales_combined <- lf_apco_dom_sales_combined %>% 
  arrange(desc(category)) %>%
  mutate_at(vars(category), funs(factor(., levels=unique(.))))

# Renewable versus Non-renewable Generation------------------------------------------------------------------------------------------------
va_annual_renewable_and_carbon_free_gen[,not_renewable:=total-renewable]

# Carbon versus Carbon Free Generation-----------------------------------------------------------------------------------------------------
va_annual_renewable_and_carbon_free_gen[,carbon_emitting:=total-carbon_free]

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

apco_dom_onwind_and_solar <- pjm_wind_working[transmission_owner=="AEP",.(date=projected_in_service_date,apco_onshore_wind=mfo)] #AEP is owner of APCO
#note: Dominion currently only has offshore wind in development
apco_dom_onwind_and_solar <- merge(apco_dom_onwind_and_solar,pjm_solar_working[transmission_owner=="AEP",.(date=projected_in_service_date,apco_solar=mfo)],id="date",all=TRUE) #APCO has no in service solar plant
apco_dom_onwind_and_solar <- merge(apco_dom_onwind_and_solar,pjm_solar_working[transmission_owner=="Dominion"&status=="In Service",.(date=actual_in_service_date,dom_solar=mfo)],by="date",all=TRUE,allow.cartesian=TRUE)
apco_dom_onwind_and_solar <- merge(apco_dom_onwind_and_solar,pjm_solar_working[transmission_owner=="Dominion"&status=="Active",.(date=projected_in_service_date,dom_solar=mfo)],by=c("date","dom_solar"),all=TRUE,allow.cartesian=TRUE)

apco_dom_onwind_and_solar <- apco_dom_onwind_and_solar[,.(apco_onshore_wind=sum(apco_onshore_wind,na.rm=T),
                                                          apco_solar=sum(apco_solar,na.rm=T),
                                                          dom_solar=sum(dom_solar,na.rm=T)),by=date]

apco_dom_onwind_and_solar <- apco_dom_onwind_and_solar[,.(date,
                                                          apco_onshore_wind=cumsum(apco_onshore_wind),
                                                          apco_solar=cumsum(apco_solar),
                                                          dom_solar=cumsum(dom_solar))]

apco_dom_onwind_and_solar[,apco_onshore_wind_and_solar:=apco_onshore_wind+apco_solar]

apco_dom_onwind_and_solar <- merge(apco_dom_onwind_and_solar,VCEA_onshore_wind_solar[,.(date,target_apco_onshore_wind_and_solar,target_dom_onshore_wind_and_solar)],id="date",all=T)

apco_dom_onwind_and_solar %>% filter(!is.na(apco_onshore_wind)) %>% fill(everything())

apco_dom_onwind_and_solar[date<='2023-12-01',`:=`(apco_solar=na.locf(apco_solar),
                                                  apco_onshore_wind=na.locf(apco_onshore_wind),
                                                  apco_onshore_wind_and_solar=na.locf(apco_onshore_wind_and_solar),
                                                  dom_solar=na.locf(dom_solar))]

apco_dom_onwind_and_solar[apco_dom_onwind_and_solar==0]=NA #making 0 values NA for graphing purposes

lf_apco_dom_onwind_and_solar <- melt(apco_dom_onwind_and_solar,id="date")

wind_and_solar_capacity_projections <- pjm_wind_working[fuel=="Wind",.(date=projected_in_service_date,onshore_wind=mfo)] 
wind_and_solar_capacity_projections <- merge(wind_and_solar_capacity_projections,pjm_wind_working[fuel=="Offshore Wind",.(date=projected_in_service_date,offshore_wind=mfo)],by="date",all=T)
wind_and_solar_capacity_projections <- merge(wind_and_solar_capacity_projections,pjm_solar_working[status=="In Service",.(date=actual_in_service_date,solar=mfo)],by="date",all=T)
wind_and_solar_capacity_projections <- merge(wind_and_solar_capacity_projections,pjm_solar_working[status!="In Service",.(date=projected_in_service_date,solar=mfo)],by=c("date","solar"),all=T)

wind_and_solar_capacity_projections <- wind_and_solar_capacity_projections[,.(onshore_wind=sum(onshore_wind,na.rm = T),
                                                                              offshore_wind=sum(offshore_wind,na.rm = T),
                                                                              solar=sum(solar,na.rm = T)),by=date]

wind_and_solar_capacity_projections <- wind_and_solar_capacity_projections[,.(date,
                                                                              offshore_wind=cumsum(offshore_wind),
                                                                              onshore_wind=cumsum(onshore_wind),
                                                                              solar=cumsum(solar))]

wind_and_solar_capacity_projections[wind_and_solar_capacity_projections==0]=NA #making 0 values NA for graphing purposes

lf_wind_and_solar_capacity_projections <-melt(wind_and_solar_capacity_projections,id="date")

# Projected Wind Capacity
total_mw_offshore_wind<-melt(total_mw_offshore_wind,id="Year")
total_mw_offshore_wind[,variable:=gsub("CVOW_Pilot","CVOW Pilot",variable)]
total_mw_offshore_wind[,variable:=gsub("CVOW_Commercial_Stage_I","CVOW Stage I",variable)]
total_mw_offshore_wind[,variable:=gsub("CVOW_Commercial_Stage_II","CVOW Stage II",variable)]
total_mw_offshore_wind[,variable:=gsub("CVOW_Commercial_Stage_III","CVOW Stage III",variable)]


#Energy Storage
pjm_storage_working <- pjm_storage
pjm_storage_names <- names(pjm_storage_working)
pjm_storage_good_names <- tolower(gsub(" ","_", pjm_storage_names))
names(pjm_storage_working) <- pjm_storage_good_names 

pjm_storage_working[,projected_in_service_date:=as.Date(projected_in_service_date,"%m/%d/%Y")]
pjm_storage_working[,actual_in_service_date:=as.Date(actual_in_service_date,"%m/%d/%Y")]

storage_capacity_projections <- pjm_storage_working[status=="In Service",.(date=actual_in_service_date,storage=mfo)]
storage_capacity_projections <- merge(storage_capacity_projections,pjm_storage_working[status=="Active",.(date=projected_in_service_date,storage=mfo)],by=c("date","storage"),all=T)

storage_capacity_projections <- storage_capacity_projections[,.(storage=sum(storage)),by=date]
storage_capacity_projections <- storage_capacity_projections[,.(date,storage=cumsum(storage))]

lf_storage_capacity_projections <- melt(storage_capacity_projections,id="date")

# Virginia Electricity Imports
va_elec_import<-subset(va_elec_import,select=-c(date))

# For energy equity figures------------------------------------------------------------------------------------------------
#getting citation information from metadata table
expenditures_source <- metadata[db_table_name=="energy_burden_county_expenditures",data_source_full_name]
percent_income_source <- metadata[db_table_name=="energy_burden_county_percent_income",data_source_full_name]

#Below there are 3 options to get the 'va_counties' geospatial dataset ready to be merged with the energy equity data
#I have commented out options B & C, but included them for reference as none of the three options are ideal

#OPTION A - successfully shows all city and county boundaries EXCEPT Accomack and Northampton
#va_counties <- sf::st_read(dsn=here::here("VirginiaAdministrativeBoundary"),layer="VirginiaCounty")
#va_counties <- as.data.table(va_counties)
#setnames(va_counties,old="NAMELSAD",new="county")#renaming county column to match other datasets
#va_counties$county <- toTitleCase(as.character(va_counties$county))

#energy_burden_county_expenditures$county <- toTitleCase(energy_burden_county_expenditures$county)
#energy_burden_county_percent_income$county <- toTitleCase(energy_burden_county_percent_income$county)

#merging county geospatial data with energy equity data
#va_energy_equity_by_county <- merge(va_counties,energy_burden_county_expenditures,id="county")
#va_energy_equity_by_county$avg_annual_energy_cost <- as.numeric(va_energy_equity_by_county$avg_annual_energy_cost)
#va_energy_equity_by_county <- merge(va_energy_equity_by_county,energy_burden_county_percent_income,id="county")
#va_energy_equity_by_county$avg_energy_burden_as_percent_income <- as.numeric(va_energy_equity_by_county$avg_energy_burden_as_percent_income) 

#states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) #to get  state outline

#states <- as.data.table(states)
#states$ID <- as.character(states$ID)
#virginia_outline <- st_as_sf(states[ID=="virginia"])

#va_energy_equity_by_county <- as.data.table(va_energy_equity_by_county)
#strange trick to ensure hover text in map visual appears correctly
#va_energy_equity_by_county[,number1:=(1500:1632)]
#small_numbers <- seq(from=1, to=2.32, by=.01)
#va_energy_equity_by_county[,number2:=small_numbers]

#va_energy_equity_by_county <- st_as_sf(va_energy_equity_by_county)
#---------------------------------------------------------------------------------------------------

#OR
#OPTION B - does not contain most cities' geospatial data but Accomack and Northampton counties appear as they should 
counties <- st_as_sf(map("county",plot = FALSE, fill = TRUE)) #loading in county data from maps package
va_counties <- subset(counties, startsWith(as.character(counties$ID),"virginia")) #isolating VA counties
va_counties <- separate(data = va_counties, col = ID, into = c("state", "county"), sep = ",") #isolating county name
va_counties <- as.data.table(va_counties)

#adjusting county names to match format of other datasets
va_counties[,county:=paste(county,"county")]
va_counties[county=="suffolk county",county:="suffolk city"] #manually adjusting for cities
va_counties[county=="virginia beach county",county:="virginia beach city"]
va_counties[county=="newport news county",county:="newport news city"]
va_counties[county=="hampton county",county:="hampton city"]
va_counties$county <- toTitleCase(va_counties$county)

energy_burden_county_expenditures$county <- toTitleCase(energy_burden_county_expenditures$county)
energy_burden_county_percent_income$county <- toTitleCase(energy_burden_county_percent_income$county)

#merging county geospatial data with energy equity data
va_energy_equity_by_county <- merge(va_counties,energy_burden_county_expenditures,id="county")
va_energy_equity_by_county$avg_annual_energy_cost <- as.numeric(va_energy_equity_by_county$avg_annual_energy_cost)
va_energy_equity_by_county <- merge(va_energy_equity_by_county,energy_burden_county_percent_income,id="county")
va_energy_equity_by_county$avg_energy_burden_as_percent_income <- as.numeric(va_energy_equity_by_county$avg_energy_burden_as_percent_income) 

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) #to get  state outline

states <- as.data.table(states)
states$ID <- as.character(states$ID)
virginia_outline <- st_as_sf(states[ID=="virginia"])

va_energy_equity_by_county <- st_as_sf(va_energy_equity_by_county)
#---------------------------------------------------------------------------------------------------------

#For energy efficiency figures--------------------------------------------------------------------------------------------
#renaming columns so it can be accepted as input into piechart function
setnames(virginia_annual_savings_through_2020,old=c("Company Name","MWh"),new=c("variable","value"))
setnames(virginia_annual_savings_through_2022,old=c("Company Name","MWh"),new=c("variable","value"))

#manipulating datasets for stacked bar chart
virginia_annual_savings_through_2020_2 <-virginia_annual_savings_through_2020 %>%
  mutate(year=c(2020,2020,2020,2020,2020,2020,2020,2020)) %>%
  filter(variable!=c("Total Needed"))

virginia_annual_savings_through_2022_2 <-virginia_annual_savings_through_2022 %>%
  mutate(year=c(2022,2022,2022,2022,2022,2022,2022,2022)) %>%
  filter(variable!=c("Total Needed"))
virginia_annual_savings_through_2022_2[6,1]="Dominion (Gross savings)"
virginia_annual_savings_2020_2022<-rbind(virginia_annual_savings_through_2020_2,virginia_annual_savings_through_2022_2)
virginia_annual_savings_2020_2022$variable <- replace(virginia_annual_savings_2020_2022$variable,virginia_annual_savings_2020_2022$variable=="DMME programs","Virginia Energy programs")
virginia_annual_savings_2020_2022$variable <- factor(virginia_annual_savings_2020_2022$variable,levels=c("Remaining Needed","APCO","C-PACE", "Virginia Energy programs","Dominion (Gross savings)","Energy Codes (modeled, adoption of 2015 IECC)","ESPCs  (modeled, MUSH and private)"))


#-----------------------------------------REFORMATTING DATASETS--------------------------------------------------------------------

# reformatting the generation dataset
va_gen_w_commas <- va_annual_generation %>%
  select(-year) %>%
  format(big.mark=",",scientific=FALSE,trim=TRUE) %>%
  data.frame()

va_gen_w_commas<- va_annual_generation %>% select(year) %>% cbind(va_gen_w_commas)
gen_names <- names(va_gen_w_commas)
good_gen_names <- capitalize(gsub("_"," ", gen_names))
names(va_gen_w_commas) <- good_gen_names

# reformatting the consumption dataset
consumption_by_sector_list <- list(eia_seds_tercb_va_a,
                                   eia_seds_teccb_va_a,
                                   eia_seds_teicb_va_a,
                                   eia_seds_teacb_va_a)

va_annual_consumption <- NULL

for(table in consumption_by_sector_list){
  if (is.null(va_annual_consumption))
  {va_annual_consumption <- table}
  else
  {va_annual_consumption <- merge(va_annual_consumption, table[], by = "year", all=TRUE)}
}

va_con_w_commas<- va_annual_consumption %>%
  select(residential, commercial, industrial, transportation) %>%
  format(big.mark=",",scientific=FALSE,trim=TRUE) %>%
  data.frame()
  
va_con_w_commas <- va_annual_consumption %>% select(year) %>% cbind(va_con_w_commas)

#reformatting carbon emissions from electricity sector
virginia_emissions_electric <- eia_emiss_co2_totv_ec_to_va_a[,.(year,electric_sector_CO2_emissions)]
virginia_emissions_electric_commas <- data.frame(signif(select(virginia_emissions_electric, electric_sector_CO2_emissions), digits=4))
virginia_emissions_electric_commas <- cbind(virginia_emissions_electric[,1],virginia_emissions_electric_commas)
colnames(virginia_emissions_electric_commas) <- c('Year','Million Metric Tons of CO2')

#reformatting emissions compounds dataset
va_emissions_compounds <- merge(emissions_co2_by_source_va[,.(year=year,CO2=total/1000)],emissions_no_by_source_va[,.(year=year,NO=total/1102311.31)],id="year")
va_emissions_compounds <- merge(va_emissions_compounds,emissions_so2_by_source_va[,.(year=year,SO2=total/1102311.31)],id="year")
va_emissions_compounds <- va_emissions_compounds %>% filter(year >= 2000) #limit data to baseline year of 2000

colnames(rps_mandate_schedule) <- c('year', 'variable', 'value')
rps_mandate_schedule <- rps_mandate_schedule[variable=="Appalachian",variable:="APCO"]

lf_percent_renewable$variable <- as.character(lf_percent_renewable$variable)
lf_percent_renewable$variable[lf_percent_renewable$variable == 'percent_renewable'] <- 'Historic'
lf_percent_renewable$variable <- as.factor(lf_percent_renewable$variable)
lf_percent_renewable_and_schedule_combined_dt <- merge(lf_percent_renewable,rps_mandate_schedule,by=c("year","variable","value"),all=T)


