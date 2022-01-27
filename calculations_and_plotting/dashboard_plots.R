#compilation of all figures relevant to dashboard

# library(groundhog)
# groundhog.day = "2021-09-01"
# pkgs = c("lubridate", "devtools", "here")
# groundhog.library(pkgs, groundhog.day)
lbry<-c("lubridate", "devtools", "here","data.table")
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)

#install_github("coopercenter/cepsvizfunctions")
#library(cepsvizfunctions)

source(here::here("calculations_and_plotting", "dashboard_calculations.R")) #sourcing in data and reformatted data tables & calculations ready to serve as input to viz functions

# Put these in a single file to source in. Replaces most of the viz package
source(here("calculations_and_plotting","single_ring_donut_figure_p.R"))
source(here("calculations_and_plotting","build_source_list.R"))
source(here("calculations_and_plotting","stacked_area_figure.R"))
source(here("calculations_and_plotting","theme_ceps.R"))
source(here("calculations_and_plotting","ggplotly_wrapper.R"))
source(here("calculations_and_plotting","pie_chart_figure_p.R"))
source(here("calculations_and_plotting","line_figure.R"))

#----------------------------------------------PLOTTING DONUT FIGURES------------------------------------------------------------------------------

#plotting doughnut figure of progress towards renewable generation goal------------------------------------------------------------------------------

setkey(eia_annual_data,Year)
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

single_ring_renewable_donut_p <-
  single_ring_donut_figure_p(
    renewable_ring,
    "Renewable Portfolio Standard",
    paste0(recent_year,
           " Status: ",
           renewable_percent_gen_recent,
           "% of Generation from RPS Eligible Sources"
    ),
    "Goal: 100% of Generation from<br>RPS Eligible Sources by 2050",
    "label+value",
    c("#5868AC","#3C5488B2"),
    list("eia_elec_gen_sun_va_99_a","VCEA_storage")
  )
single_ring_renewable_donut_p

#plotting donut figure of progress towards carbon-free generation goal ------------------------------------------------------------------------------------------

# was `carbon_free_percent_gen_2019`
recent_year = eia_annual_data[!is.na(Percent_carbon_free),last(Year)]
carbon_free_percent_gen_recent = round(eia_annual_data[!is.na(Percent_carbon_free),last(Percent_carbon_free)], 1)
carbon_free_percent_gen_2050_goal = 100 #100% of Virginia’s electricity from carbon-free sources by 2050

carbon_free_ring = data.frame(
  category = c(
    "2019 carbon free generation (%)",
    "Additional progress necessary to reach goal (%)"
  ),
  value = c(
    carbon_free_percent_gen_recent,
    carbon_free_percent_gen_2050_goal - carbon_free_percent_gen_recent
  )
)

single_ring_carbon_free_donut_p <-
  single_ring_donut_figure_p(
    carbon_free_ring,
    "Carbon-Free Generation",
    paste0(recent_year,
           " Status: ",
           carbon_free_percent_gen_recent,
           "% of Generation from Carbon-Free Sources"
    ),
    "Goal: 100% of Generation from<br>Carbon-Free Sources by 2050",
    "label+value",
    c("#BE7E8A","#CEA5AC"),
    list("eia_elec_gen_nuc_va_99_a", "VCEA_storage")
  )
single_ring_carbon_free_donut_p

#plotting donut figure of progess towards onshore wind and solar capacity goals-----------------------------------------------------------------------------------------
recent_year = va_solar[,max(Operating_Year,na.rm=TRUE)]
solar_capacity_current_mw = va_solar[,sum(capacity_mw)] 
### This should reflect onshore wind only!! There is none as of Jan 2022
onshore_wind_capacity_current_mw = 0  #va_wind[,sum(capacity_mw)]

sw_capacity_2035_mw_goal = 16100 #16,100 MW of solar and onshore wind by January 1, 2024 (from VCEA Summary 3.0)

sw_ring = data.frame(
  category = c(paste0(recent_year, " capacity"),"Additional capacity necessary to reach goal"),
  value = c(
    solar_capacity_current_mw + onshore_wind_capacity_current_mw,
    sw_capacity_2035_mw_goal - (solar_capacity_current_mw + onshore_wind_capacity_current_mw)
  )
)

single_ring_sw_capacity_donut_p <-
  single_ring_donut_figure_p(
    sw_ring,
    "Onshore Wind & Solar Capacity",
    paste(recent_year,
          "Status:",
          format(
            solar_capacity_current_mw + onshore_wind_capacity_current_mw,
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          ),
          "MW of Onshore Wind & Solar Capacity"
    ),
    paste(
      "Goal:",
      format(
        sw_capacity_2035_mw_goal,
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      ),
      "MW of Onshore Wind & Solar<br>Capacity in Operation by 2035"
    ),
    "label+value",
    c("#00A087B2","#91D1C2B2"),
    list("pjm_solar", "pjm_wind", "VCEA_storage")
  )
single_ring_sw_capacity_donut_p

#plotting donut figure of progress towards storage capacity------------------------------------------------------------------------------------------
# There was no battery storage installed in Virginia in 2020.
recent_year = va_storage[,max(Operating_Year,na.rm=TRUE)]

# was`storage_capacity_2019_mw`
solar_capacity_current_mw = va_solar[,sum(capacity_mw)] 
storage_capacity_2035_mw_goal = 3100

storage_ring = data.frame(
  category = c(paste(recent_year, "capacity"),
               "Additional capacity necessary to reach goal"),
  value = c(
    storage_capacity_current_mw,
    storage_capacity_2035_mw_goal - storage_capacity_current_mw
  )
)

single_ring_storage_capacity_donut_p <-
  single_ring_donut_figure_p(
    storage_ring,
    "Energy Storage Capacity",
    paste(recent_year,
          "Status:",
          format(
            storage_capacity_current_mw,
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          ),
          "MW of Storage Capacity"
    ),
    paste(
      "Goal:",
      format(
        storage_capacity_2035_mw_goal,
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      ),
      "MW of Storage Capacity<br>in Operation by 2035"
    ),
    "label+value",
    c("#6FB3D9","#B0DEFA"),
    list("pjm_solar", "VCEA_storage")
  )
single_ring_storage_capacity_donut_p

#plotting donut figure of progress towards offshore wind--------------------------------------------------------------------------------------------------------------
## commented out as there are no wind actually in service. this should be check for an update in the next few years
"
recent_year <- pjm_wind_working %>% select(actual_in_service_date) %>% arrange() %>% filter(!is.na(actual_in_service_date)) 
recent_year <- recent_year[[nrow(recent_year),1]] %>% as.POSIXct() %>% year
"
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

single_ring_offshore_wind_capacity_donut_p <-
  single_ring_donut_figure_p(
    offshore_wind_ring,
    "Offshore Wind Capacity",
    paste(recent_year,
          "Status:",
          format(
            offshore_wind_current_mw,
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          ),
          "MW of Offshore Wind Capacity"
    ),
    paste(
      "Goal:",
      format(
        offshore_wind_2034_mw_goal,
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      ),
      "MW of Offshore Wind<br>Capacity in Operation by 2034"
    ),
    "label+value",
    c("#99A9E2","#8491B4B2"),
    list("pjm_solar", "VCEA_storage")
  )
single_ring_offshore_wind_capacity_donut_p

#--------------------------------------------PLOTTING GENERATION/PRODUCTION FIGURES----------------------------------------------------------------
generation_stacked_area_data = melt(eia_annual_data[Year>2000,.(Year,
                                                                Coal,Oil,Gas,Nuclear,Solar_utility,Solar_distributed,
                                                                Hydropower,Wind,Wood,Other_biomass
)],id="Year")
setnames(generation_stacked_area_data,c("Year","variable","value"),
         c("x_value","fill_variable","y_value")
)
va_annual_production_area <-
  stacked_area_figure(
    generation_stacked_area_data,
    "Generation (GWh)",
    "Virginia Electricity Generation by Fuel Type",
    list(
      "eia_elec_gen_cow_va_99_a"
    ),
    lower_limit = -1900,
    return_static = F
  )
va_annual_production_area

va_annual_production_area_p <-
  ggplotly_wrapper(va_annual_production_area)
va_annual_production_area_p

# annual production by fuel type pie chart
# just use the latest year observation of the previous plot data
latest_year = max(generation_stacked_area_data$x_value)
annual_production_pie_chart_data = generation_stacked_area_data[x_value==latest_year ][,x_value:=NULL]
setnames(annual_production_pie_chart_data,c("y_value","fill_variable"),c("value","variable"))
va_annual_production_pie_chart_p_with_legend <-
  pie_chart_figure_p(
    annual_production_pie_chart_data,
    paste0(
      "Virginia ", latest_year, " Electricity Generation by Fuel Type"
    ),
    list(
      "eia_elec_gen_cow_va_99_a"     # Source: EIA
    ),
    legend_shown = T
  )
va_annual_production_pie_chart_p_with_legend

#--------------------------------------------PLOTTING CONSUMPTION FIGURES---------------------------------------------------------------------
annual_consumption_data = melt(eia_annual_data[,.(Year,Residential,
                                                  Commercial,Transportation,Industrial)],
                               id="Year")
setnames(annual_consumption_data,c("Year","variable","value"),
         c("x_value","fill_variable","y_value")
)
va_annual_consumption_area <-
  stacked_area_figure(
    annual_consumption_data,
    "Consumption (Billion Btu)",
    "Virginia Energy Consumption by Sector",
    list(
      "eia_seds_tercb_va_a"
    ),
    return_static = F,
    modifications = scale_y_continuous(labels = comma)
  )
va_annual_consumption_area

va_annual_consumption_area_p <-
  ggplotly_wrapper(va_annual_consumption_area)
va_annual_consumption_area_p

latest_year = max(eia_annual_data[Commercial!=0,Year])
annual_consumption_pie_chart_data = annual_consumption_data[x_value==latest_year ][,x_value:=NULL]
setnames(annual_consumption_pie_chart_data,c("y_value","fill_variable"),c("value","variable"))
va_annual_consumption_pie_chart_p_with_legend <-
  pie_chart_figure_p(
    annual_consumption_pie_chart_data,
    paste0(
      "Virginia ", latest_year, " Energy Consumption by Sector"
    ),
    list(
      "eia_seds_tercb_va_a"
    ),
    legend_shown = T
  )
va_annual_consumption_pie_chart_p_with_legend

#--------------------------------PLOTTING RENEWABLE & CARBON-FREE GENERATION IN PARTICULAR-----------------------------------------------------
source(here("calculations_and_plotting","line_figure.R"))

# Graphing % of VA power generation (in GWh/yr) from renewables & carbon-free sources
lf_percent_renewable_and_carbon_free <- melt(eia_annual_data[!is.na(Percent_renewable | !is.na(Percent_carbon_free)),
                                                             .(Year,Percent_renewable,Percent_carbon_free)],id="Year")
setnames(lf_percent_renewable_and_carbon_free,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
         
percent_renewable_and_carbon_free_line <-
  line_figure(
    lf_percent_renewable_and_carbon_free,
    "Percentage of Total Generation",
    "Virginia Recent Electricity Generation",
    list(
      "eia_elec_gen_nuc_va_99_a"
    ),
    return_static = F,
    upper_limit = 100,
    subtitle_description = "Renewable and Carbon-Free"
  )
percent_renewable_and_carbon_free_line

percent_renewable_and_carbon_free_line_p <-
  ggplotly_wrapper(percent_renewable_and_carbon_free_line)
percent_renewable_and_carbon_free_line_p

#--- Use the data.table from the previous plot
lf_percent_carbon_free = lf_percent_renewable_and_carbon_free[fill_variable=="Percent_carbon_free"]
percent_carbon_free_line <-
  line_figure(
    lf_percent_carbon_free,
    "Percentage of Total Generation",
    "Virginia Carbon-Free Electricity Generation",
    list(
      "eia_elec_gen_nuc_va_99_a"
    ),
    return_static = F,
    modifications = theme(legend.position = "none"),
    upper_limit = 100
  )
percent_carbon_free_line

percent_carbon_free_line_p <-
  ggplotly_wrapper(percent_carbon_free_line)
percent_carbon_free_line_p


# Working
rps_renewable_line <-
  ggplot(rps_mandate_schedule, aes(x = Year, y = value, color = variable)) +
  geom_line(aes(
    group = variable,
    text = paste0("Year: ",Year,"\n",
                  "Variable: ",variable,"\n",
                  "Value: ", round(value, 4)
    )
  ), linetype = "dashed") +
  ylab("Percentage of Generation from RPS Eligible Sources") + xlab("Year") + ylim(0, NA) +
  labs(title = "Virginia Renewable Portfolio Standard Schedule", caption =
         paste("Source:", metadata[db_table_name == "clean_energy_renewable_goals", data_source_full_name])) +
  scale_color_manual(name = NULL, values = ceps_pal[3:4]) +
  theme_ceps()
rps_renewable_line

rps_renewable_line_p <-
  ggplotly_wrapper(
    list(
      figure = rps_renewable_line,
      x_label = "Year",
      source_description = paste("Source:", metadata[db_table_name == "clean_energy_renewable_goals", data_source_full_name]),
      title_name = "Virginia Renewable Portfolio Standard Schedule",
      subtitle_description = NULL,
      y_label = "Percentage of Generation from RPS Eligible Sources"
    )
  )
rps_renewable_line_p

#  
latest_actuals_year = lf_percent_renewable_carbon_free_combined_dt[variable=="Percent_renewable",max(Year)]
setnames(lf_percent_renewable_carbon_free_combined_dt,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
percent_renewable_and_carbon_free_goal_combined_line <-
  line_figure(
    lf_percent_renewable_carbon_free_combined_dt[!is.na(y_value)],
    "Percentage of Total Generation",
    "Virginia Electricity Generation",
    list(
      "eia_elec_gen_nuc_va_99_a",
      "VCEA_storage",
      "va_utility_sales"
    ),
    return_static = F,
    subtitle_description = "Renewable and Carbon Free",
    future_date = as.numeric(latest_actuals_year) + 1
  )
percent_renewable_and_carbon_free_goal_combined_line

setnames(lf_percent_renewable_and_schedule_combined_dt,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
percent_renewable_and_schedule_goal_combined_line <-
  line_figure(
    lf_percent_renewable_and_schedule_combined_dt[!is.na(y_value)],
    "Percentage of Generation from RPS Eligible Sources",
    "Virginia Renewable Portfolio Standard Schedule",
    list(
      "eia_elec_gen_nuc_va_99_a",
      "VCEA_storage",
      "va_utility_sales"
    ),
    return_static = F,
    future_date = as.numeric(latest_actuals_year) + 1
  )
percent_renewable_and_schedule_goal_combined_line

percent_renewable_and_carbon_free_goal_combined_line_p <-
  ggplotly_wrapper(percent_renewable_and_carbon_free_goal_combined_line)
percent_renewable_and_carbon_free_goal_combined_line_p

percent_renewable_and_schedule_goal_combined_line_p <-
  ggplotly_wrapper(percent_renewable_and_schedule_goal_combined_line)
percent_renewable_and_schedule_goal_combined_line_p


# Solar, Hydro, and Nuclear Generation over Time
carbon_free_data <- melt(eia_annual_data[Nuclear!=0,.(Year,Nuclear,Solar_utility,Solar_distributed,Hydropower,Wind)],id="Year")
setnames(carbon_free_data,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
annual_carbon_free_generation_by_type_line <-
  line_figure(
    carbon_free_data,
    "Generation (GWh)",
    "Virginia Carbon-Free Electricity Generation",
    list(
      "eia_elec_gen_nuc_va_99_a"
    ),
    return_static = F
  )
annual_carbon_free_generation_by_type_line

annual_carbon_free_generation_by_type_line_p <-
  ggplotly_wrapper(annual_carbon_free_generation_by_type_line)
annual_carbon_free_generation_by_type_line_p
rm(carbon_free_data)

# Solar (broken into distributed and utility) over time
solar_data <- melt(eia_annual_data[Year>2012,.(Year=as.factor(Year),Solar_utility,Solar_distributed)],id="Year")
setnames(solar_data,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
solar_generation_time_series_line <-
  line_figure(
    solar_data,
    "Generation (GWh)",
    "Virginia Solar Electricity Generation",
    list("eia_elec_gen_sun_va_99_a"),
    return_static = F,
    subtitle_description = "Utility Scale and Distributed"
  )
solar_generation_time_series_line
rm(solar_data)

solar_generation_time_series_line_p <-
  ggplotly_wrapper(solar_generation_time_series_line)
solar_generation_time_series_line_p

# Projected wind generation overtime
wind_data <- melt(total_production_forecast_offshore_wind[Year>2019], id = "Year")
setnames(wind_data,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
wind_projected_generation_time_series_line <-
  line_figure(
    wind_data,
    "Projected Generation (GWh)",
    "Virginia Projected Offshore Wind Electricity Generation",
    list("total_production_forecast_offshore_wind"),
    return_static = F,
    modifications =  theme(legend.position = "none"),
    subtitle_description = "Planned",
    future_date = 2021
  )
wind_projected_generation_time_series_line

### Need to fix Y-axis
#manually re-scaling y-axis so that value of 44 doesn't look like 0
wind_projected_generation_time_series_line$figure <-
  wind_projected_generation_time_series_line$figure +
  scale_y_continuous(
    breaks = c(44, 2500, 5000, 7500),
    labels = c("44", "2500", "5000", "7500")
  )
wind_projected_generation_time_series_line

wind_projected_generation_time_series_line_p <-
  ggplotly_wrapper(wind_projected_generation_time_series_line)
wind_projected_generation_time_series_line_p

# Projected wind capacity
offshore_wind = total_mw_offshore_wind
setnames(offshore_wind,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
wind_projected_capacity_line <-
  line_figure(
    offshore_wind,
    "Projected Capacity (MW)",
    "Coastal Virginia Offshore Wind (CVOW) Capacity",
    list("total_mw_offshore_wind"),
    return_static = F,
    subtitle_description = "Planned",
    future_date = as.numeric(format(Sys.Date(), "%Y")) + 1
  )
wind_projected_capacity_line

wind_projected_capacity_line_p <-
  ggplotly_wrapper(wind_projected_capacity_line)
wind_projected_capacity_line_p

#Stacked Annual Carbon Free Generation Broken Out by Type
carbon_free_data <- melt(eia_annual_data[Nuclear!=0,.(Year,Nuclear,Solar_utility,Solar_distributed,Hydropower,Wind)],id="Year")
setnames(carbon_free_data,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
carbon_free_generation_by_type_stacked <-
  stacked_area_figure(
    carbon_free_data,
    "Generation (GWh)",
    "Virginia Carbon-Free Electricity Generation by Source",
    list("eia_elec_gen_nuc_va_99_a"),
    return_static = F
  )
carbon_free_generation_by_type_stacked
rm(carbon_free_data)

carbon_free_generation_by_type_stacked_p <-
  ggplotly_wrapper(carbon_free_generation_by_type_stacked)
carbon_free_generation_by_type_stacked_p

# Stacked Annual Renewable Generation Broken Out by Type (hydro, utility solar, distributed solar)
renewables_data <- melt(eia_annual_data[Nuclear!=0,.(Year,Solar_utility,Solar_distributed,Hydropower,Wind)],id="Year")
setnames(renewables_data,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
renewable_generation_by_type_stacked <-
  stacked_area_figure(
    renewables_data,
    "Generation (GWh)",
    "Virginia Renewable Electricity Generation",
    list(
      "eia_elec_gen_sun_va_99_a"
    ),
    return_static = F
  )
renewable_generation_by_type_stacked
rm(renewables_data)

renewable_generation_by_type_stacked_p <-
  ggplotly_wrapper(renewable_generation_by_type_stacked)
renewable_generation_by_type_stacked_p


# VA Electricity Net Imports
electricity_imports <- eia_annual_data[Imported_electricity!=0,
                                       .(x_value=Year,y_value=Imported_electricity,
                                         fill_variable="Imports")]
va_elec_net_imports_line <-
  line_figure(
    electricity_imports,
    "Interstate Electricity Flow (GWh)",
    "Virginia Net Interstate Electricity Flow",
    return_static = F,
    source_citation = "Source: U.S. Energy Information Administration",
    subtitle_description = "Positive Values = Imports",
    modifications = theme(legend.position = "none")
  )
va_elec_net_imports_line

va_elec_net_imports_line_p <-
  ggplotly_wrapper(va_elec_net_imports_line)
va_elec_net_imports_line_p

#--------------------------------PLOTTING EMISSIONS FIGURES--------------------------------------------------------

# CO2 total emissions & CO2 emissions from electric sector on same figure
# The SEDS data is greatly delayed. Maybe we can think of a better presentation here.
# Two more years of data is available beyond what SEDS has. I don't think that includes
#    total energy related CO2 emissions.
electricity_CO2_emissions = va_electricity_emissions_by_fuel[CO2_Total!=0,.(Year,
                                      Electricity_sector = CO2_Total/1000)]
all_CO2_emissions = eia_annual_data[Total_CO2_emissions!=0,
                                          .(Year=year(date),
                                            All_sectors = Total_CO2_emissions) ]
CO2_emissions = merge(electricity_CO2_emissions,all_CO2_emissions,by="Year",all=TRUE)
CO2_emissions <- melt(CO2_emissions,id="Year")
CO2_emissions[,variable := gsub("_"," ",variable)]
setnames(CO2_emissions,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
co2_combined_emissions_line <-
  line_figure(
    CO2_emissions,
    "Emissions (million metric tons CO2)",
    "Virginia CO2 Emissions from Electricity Production",
    list(
      "eia_emiss_co2_totv_ec_to_va_a"
    ),
    return_static = F
  )
co2_combined_emissions_line
rm(CO2_emissions)

co2_combined_emissions_line_p <-
  ggplotly_wrapper(co2_combined_emissions_line)
co2_combined_emissions_line_p


# CO2 emissions by fuel type
### Check the units here
emission_data_by_fuel = melt(va_electricity_emissions_by_fuel[, .(
  Year = Year,
  Coal = CO2_Coal / 1000,
  Natural_gas = CO2_Natural_gas / 1000,
  Petroleum = CO2_Petroleum / 1000,
  Other = CO2_Other / 1000
)], id = "Year")
setnames(emission_data_by_fuel,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
carbon_by_fuel_emissions_stacked <-
  stacked_area_figure(
    emission_data_by_fuel,
    "Emissions (million metric tons)",
    "Virginia CO2 Emissions From Electricity Production By Fuel Type",
    list("emissions_co2_by_source_va"),
    return_static = F
  )
carbon_by_fuel_emissions_stacked
rm(emission_data_by_fuel)

carbon_by_fuel_emissions_stacked_p <-
  ggplotly_wrapper(carbon_by_fuel_emissions_stacked)
carbon_by_fuel_emissions_stacked_p

#-------------------------------------PLOTTING ENERGY EFFICIENCY FIGURES--------------------------------------
energy_per_gdp <- intensity_data[!is.na(energy_consumption_per_gdp),
                               .(x_value=as.factor(year(date)),y_value=energy_consumption_per_gdp,
                                 fill_variable="Cons_per_gdp")]
consumption_per_gdp_line <-
  line_figure(
    energy_per_gdp,
    "Consumption per GDP (Btu/$)",
    "Virginia Electricity Consumption per Dollar of GDP",
    list("fred_vangsp", "eia_seds_tetcb_va_a"),
    #for now, may change to derived values table name at some point
    return_static = F,
    modifications = theme(legend.position = "none")
  )
consumption_per_gdp_line
rm(energy_per_gdp)

consumption_per_gdp_line_p <-
  ggplotly_wrapper(consumption_per_gdp_line)
consumption_per_gdp_line_p

energy_cons_per_capita <- intensity_data[!is.na(energy_consumption_per_capita),
                                 .(x_value=as.factor(year(date)),y_value=energy_consumption_per_capita,
                                   fill_variable="Cons_per_gdp")]
consumption_per_capita_line <-
  line_figure(
    energy_cons_per_capita,
    "Consumption per Capita (Billion Btu/Person)",
    "Virginia Electricity Consumption per Person",
    list("fred_vapop", "eia_seds_tetcb_va_a"),
    #for now, may change to derived values table names at some point
    return_static = F,
    modifications = theme(legend.position = "none")
  )
consumption_per_capita_line
rm(energy_cons_per_capita)

consumption_per_capita_line_p <-
  ggplotly_wrapper(consumption_per_capita_line)
consumption_per_capita_line_p

co2_per_GDP <- intensity_data[!is.na(co2_per_GDP),
                              .(x_value=as.factor(year(date)),y_value=co2_per_GDP,
                              fill_variable="co2_per_gdp")]
emissions_per_gdp_line <-
  line_figure(
    co2_per_GDP,
    "Emissions/GDP (Metric Tons/Thousand $)",
    "Virginia CO2 Emissions per Dollar of GDP",
    list("fred_vangsp", "eia_emiss_co2_totv_tt_to_va_a"),
    return_static = F,
    modifications = theme(legend.position = "none")
  )
emissions_per_gdp_line
rm(co2_per_GDP)

emissions_per_gdp_line_p <- ggplotly_wrapper(emissions_per_gdp_line)
emissions_per_gdp_line_p

co2_per_capita <- intensity_data[!is.na(co2_per_GDP),
                        .(x_value=as.factor(year(date)),y_value=co2_per_capita,
                                fill_variable="co2_per_capita")]
emissions_per_capita_line <-
  line_figure(
    co2_per_capita,
    "Emissions per Capita (Metric Tons/Person)",
    "Virginia CO2 Emissions per Capita",
    list("fred_vangsp", "eia_emiss_co2_totv_tt_to_va_a"),
    return_static = F,
    modifications = theme(legend.position = "none")
  )
emissions_per_capita_line
rm(co2_per_capita)

emissions_per_capita_line_p <-
  ggplotly_wrapper(emissions_per_capita_line)
emissions_per_capita_line_p

annual_savings_2020_2022_stacked_bar_chart <-
  ggplot(virginia_annual_savings_2020_2022,
         mapping = aes(x = year, y = value, fill = variable)) +
  geom_bar(
    position = "stack",
    stat = "identity",
    color = "black",
    size = .2,
    aes(
      group = variable,
      text = paste0(
        "Year: ",
        year,
        "\n",
        "Value: ",
        round(value, 4),
        "\n",
        "Variable: ",
        variable
      )
    )
  ) +
  scale_x_continuous(breaks = c(2020, 2022)) +
  scale_y_continuous(labels = comma) +
  labs(
    x = "Year",
    y = "Projected energy savings (MWh)",
    title = "Projected Future Savings From Energy Efficiency Programs",
    caption = "Source: The American Council for an Energy-Efficient Economy"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "#D3D3D3","#00A087B2", "#3C5488B2","#CEA5AC","#BE7E8A","#4DBBD5B2","#91D1C2B2"
    )
  ) +
  theme_ceps()
annual_savings_2020_2022_stacked_bar_chart

annual_savings_2020_2022_stacked_bar_chart_p <-
  ggplotly_wrapper(
    list(
      figure = annual_savings_2020_2022_stacked_bar_chart,
      x_label = "Year",
      source_description = "Source: The American Council for an Energy-Efficient Economy",
      title_name = "Virginia Energy Savings through 2020 and 2022",
      subtitle_description = NULL,
      y_label = "Savings (MWh)"
    )
  ) %>%
  layout(legend = list(x = 1, y = 0.5))
annual_savings_2020_2022_stacked_bar_chart_p


#creating reference figures
#data not currently in db, so assigning values manually
#2018 energy expenditure per capita from EIA data (https://www.eia.gov/state/seds/data.php?incfile=/state/seds/sep_sum/html/rank_pr.html&sid=US)
#median income from Census data (https://www.census.gov/search-results.html?q=2018+income&page=1&stateGeo=none&searchtype=web&cssp=SERP&_charset_=UTF-8)
virginia_2018_energy_expenditures_per_capita = 3601
us_2018_energy_expenditures_per_capita = 3891

virginia_median_income = 71564
us_median_income = 60293

dollar_reference_table <- data.table(
  category = c("Virginia", "U.S."),
  value = c(
    virginia_2018_energy_expenditures_per_capita,
    us_2018_energy_expenditures_per_capita
  )
)

percentage_of_income_reference_table <-
  data.table(
    category = c("Virginia", "U.S."),
    value = c(
      virginia_2018_energy_expenditures_per_capita / virginia_median_income *
        100,
      us_2018_energy_expenditures_per_capita / us_median_income * 100
    )
  )

dollar_reference_figure <-
  ggplot(dollar_reference_table,
         mapping = aes(x = category, y = value, fill = category)) +
  geom_bar(position = "dodge",
           stat = "identity",
           aes(
             group = category,
             text = paste0("Value: ", value, "\n", "Variable: ", category)
           )) +
  xlab(NULL) + ylab("Energy Cost (Dollars)") +
  labs(title = "2018 Energy Expenditures per Capita in Dollars",
       caption = "Source: U.S. Energy Information Administration",
       subtitle = "U.S. vs Virginia") +
  scale_fill_manual(values = ceps_pal[1:2]) +
  theme_ceps() +
  theme(legend.position = "none")
dollar_reference_figure

dollar_reference_figure_p <-
  ggplotly_wrapper(
    list(
      figure = dollar_reference_figure,
      x_label = NULL,
      source_description = "Source: U.S. Energy Information Administration",
      title_name = "2018 Energy Expenditures per Capita in Dollars",
      subtitle_description = "U.S. vs Virginia",
      y_label = "Energy Cost (Dollars)"
    )
  )
dollar_reference_figure_p

percent_income_reference_figure <-
  ggplot(
    percentage_of_income_reference_table,
    mapping = aes(x = category, y = value, fill = category)
  ) +
  geom_bar(position = "dodge",
           stat = "identity",
           aes(
             group = category,
             text = paste0("Value: ", round(value, 3), "\n", "Variable: ", category)
           )) +
  xlab(NULL) + ylab("Energy Cost (Percentage of Income)") +
  labs(title = "2018 Energy Expenditures per Capita as a Percentage of Median Income",
       caption = "Source: U.S. Census Bureau, U.S. Energy Information Administration",
       subtitle = "U.S. vs Virginia") +
  scale_fill_manual(values = ceps_pal[1:2]) +
  theme_ceps() +
  theme(legend.position = "none")
percent_income_reference_figure

percent_income_reference_figure_p <-
  ggplotly_wrapper(
    list(
      figure = percent_income_reference_figure,
      x_label = NULL,
      source_description = "Source: U.S. Census Bureau, U.S. Energy Information Administration",
      title_name = "2018 Energy Expenditures per Capita as a Percentage of Median Income",
      subtitle_description = "U.S. vs Virginia",
      y_label = "Energy Cost (Percentage of Income)"
    )
  )
percent_income_reference_figure_p

apco_dom_sales_data = melt(lf_apco_dom_sales_combined_dt, id = "year")
setnames(lf_apco_dom_sales_combined_dt,c("year","variable","value"),
         c("x_value","fill_variable","y_value"))
apco_dom_historic_goal_sales_combined_line <-
  line_figure(
    lf_apco_dom_sales_combined_dt,
    "Total Retail Sales (GWh/year)",
    "Mandated Electricity Reduction Under Virginia Clean Economy Act",
    return_static = F,
    source_citation = "Source: U.S. Energy Information Administration, Dominion Energy Inc.",
    future_date = as.numeric(format(Sys.Date(), "%Y")) + 1
  )
apco_dom_historic_goal_sales_combined_line
rm(apco_dom_sales_data)

apco_dom_historic_goal_sales_combined_line_p <-
  ggplotly_wrapper(apco_dom_historic_goal_sales_combined_line)
apco_dom_historic_goal_sales_combined_line_p


#------------------------------------------------------------------------------------------------------------------------------------------------------
#Saving only the plots that the dashboard uses. This will save R image file into cep-viz folder. Move that R image file into the dashboard file and
#open those objects into the global environment in the dashboard project.
save(
  single_ring_renewable_donut_p,
  single_ring_carbon_free_donut_p,
  single_ring_renewable_donut_p,
  single_ring_carbon_free_donut_p,
  va_annual_production_area_p,
  va_annual_production_pie_chart_p_with_legend,
  co2_combined_emissions_line_p,
  co2_combined_emissions_line_p,
  carbon_by_fuel_emissions_stacked_p,
  va_annual_consumption_area_p,
  va_annual_consumption_area_p,
  va_annual_consumption_pie_chart_p_with_legend,
  percent_renewable_and_carbon_free_line_p,
  percent_carbon_free_line_p,
  percent_renewable_and_schedule_goal_combined_line_p,
  percent_renewable_and_carbon_free_goal_combined_line_p,
  va_gen_w_commas,
  va_con_w_commas,
  virginia_emissions_electric_commas,
  single_ring_sw_capacity_donut_p,
  annual_carbon_free_generation_by_type_line_p,
  solar_generation_time_series_line_p,
  wind_projected_generation_time_series_line_p,
  wind_projected_capacity_line_p,
  single_ring_offshore_wind_capacity_donut_p,
  pjm_solar,
  investment_by_IOUs,
  pjm_storage,
  single_ring_storage_capacity_donut_p,
  consumption_per_capita_line_p,
  consumption_per_gdp_line_p,
  emissions_per_capita_line_p,
  emissions_per_gdp_line_p,
  pjm_wind,
  va_elec_net_imports_line_p,
  rps_renewable_line_p,
  
  #va_avg_annual_energy_cost_p,
  #va_avg_annual_energy_percent_exp_p,
  #va_avg_annual_energy_cost,
  #va_avg_annual_energy_percent_exp,
  
  # dollar_reference_figure,
  # dollar_reference_figure_p,
  # percent_income_reference_figure,
  # percent_income_reference_figure_p,
  
  annual_savings_2020_2022_stacked_bar_chart,
  annual_savings_2020_2022_stacked_bar_chart_p,
  apco_dom_historic_goal_sales_combined_line_p,
  file = "dashboard_output.RData"
)

