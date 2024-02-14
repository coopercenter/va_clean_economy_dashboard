#PREP--------------------------------------------------------------------------------------------------------------------------------
lbry<-c("lubridate", "devtools", "here","data.table",'plotly','ggplot2')
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)

#read in the existing RData file so that not every graph needs to be remade at every update, instead we can resave the existing, unchanged graph objects at the end after we update the desired plots
load(here('dashboard_output.RData'))

#sourcing in data and reformatted data tables & calculations ready to serve as input to viz functions
source(here::here("calculations_and_plotting", "dashboard_calculations_new.R"))

#source the plot functions
source(here("calculations_and_plotting/plot_functions.R"))

#custom color palette
theme_colors <- c("#00A087B2", "#3C5488B2", "#CEA5AC", "#BE7E8A", "#4DBBD5B2", "#91D1C2B2","#D9C6C9","#8491B4B2","#5868AC","#6FB3D9","#56BD96","#99A9E2","#A94F64","#B0DEFA","#99EEBB","#8FD3FE")

#SUMMARY PAGE---------------------------------------------------------------------------------------------------------------------------------------------------

#single_ring_renewable_donut_p
renewable_ring <- renewable_ring_data()
single_ring_renewable_donut_p <-
  single_ring_donut_figure_p(
    data_table=renewable_ring,
    description_of_goal="Renewable Portfolio Standard",
    top_description=paste0(eia_annual_data[!is.na(Percent_renewable),last(Year)],
           " Status: ",
           renewable_ring$value[1],
           "% of Generation from RPS Eligible Sources"
    ),
    bottom_description="Goal: 100% of Generation from<br>RPS Eligible Sources by 2050",
    "label+value",
    colors_list=c("#5868AC","#3C5488B2"),
    character_list=list("eia_elec_gen_sun_va_99_a","VCEA_storage")
  )
single_ring_renewable_donut_p

#single_ring_carbon_free_donut_p
carbon_free_ring <- carbon_free_ring_data()
single_ring_carbon_free_donut_p <-
  single_ring_donut_figure_p(
    carbon_free_ring,
    "Carbon-Free Generation",
    paste0(eia_annual_data[!is.na(Percent_carbon_free),last(Year)],
           " Status: ",
           carbon_free_ring$value[1],
           "% of Generation from Carbon-Free Sources"
    ),
    "Goal: 100% of Generation from<br>Carbon-Free Sources by 2050",
    "label+value",
    c("#BE7E8A","#CEA5AC"),
    list("eia_elec_gen_nuc_va_99_a", "VCEA_storage")
  )
single_ring_carbon_free_donut_p

#single_ring_storage_capacity_donut_p
storage_ring <- storage_capacity_ring_data()
single_ring_storage_capacity_donut_p <-
  single_ring_donut_figure_p(
    storage_ring,
    "Energy Storage Capacity",
    paste(2021, #recent_year,
          "Status:",
          format(
            storage_ring$value[1],
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          ),
          "MW of Storage Capacity"
    ),
    paste(
      "Goal:",
      format(
        3100,
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

#va_annual_production_area_p
generation_stacked_area_data <- annual_production_data()
va_annual_production_area <-
  stacked_area_figure(
    generation_stacked_area_data,
    "Total Generation (GWh)",
    "Virginia Electricity Generation by Fuel Type",
    list(
      "eia_elec_gen_cow_va_99_a"
    ),
    lower_limit = -1900,
    return_static = F,
    modifications = scale_y_continuous(labels = comma)
  )
va_annual_production_area

va_annual_production_area_p <-
  ggplotly_wrapper(va_annual_production_area)
va_annual_production_area_p

#va_annual_production_pie_chart_p_with_legend
annual_production_pie_chart_data <- annual_production_pie_data()
va_annual_production_pie_chart_p_with_legend <-
  pie_chart_figure_p(
    annual_production_pie_chart_data,
    paste0(
      "Virginia ", max(generation_stacked_area_data$x_value), " Electricity Generation by Fuel Type"
    ),
    list(
      "eia_elec_gen_cow_va_99_a"     # Source: EIA
    ),
    legend_shown = T
  )
va_annual_production_pie_chart_p_with_legend

#va_annual_consumption_area_p
annual_consumption_data <- consumption_data()
va_annual_consumption_area <-
  stacked_area_figure(
    annual_consumption_data,
    "Total Consumption (Billion Btu)",
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

#va_annual_consumption_pie_chart_p_with_legend
annual_consumption_pie_chart_data <- annual_consumption_pie_data()
va_annual_consumption_pie_chart_p_with_legend <-
  pie_chart_figure_p(
    annual_consumption_pie_chart_data,
    paste0(
      "Virginia ", max(eia_annual_data[Commercial!=0,Year]), " Energy Consumption by Sector"
    ),
    list(
      "eia_seds_tercb_va_a"
    ),
    legend_shown = T
  )
va_annual_consumption_pie_chart_p_with_legend

#GENERATION AND CAPACITY PAGE-------------------------------------------------------------------------------------------------------------------------------

#single_ring_sw_capacity_donut_p
sw_ring <- single_ring_capacity_donut_data()
single_ring_sw_capacity_donut_p <-
  single_ring_donut_figure_p(
    sw_ring,
    "Onshore Wind & Solar Capacity",
    paste(va_solar[,max(Operating_Year,na.rm=TRUE)],
          "Status:",
          format(
            sw_ring$value[1],
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          ),
          "MW of Onshore Wind & Solar Capacity"
    ),
    paste(
      "Goal:",
      format(
        16100,
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

#single_ring_renewable_donut_p
#renewable_ring <- renewable_ring_data()
#single_ring_renewable_donut_p <-
  #single_ring_donut_figure_p(
    #renewable_ring,
    #"Renewable Portfolio Standard",
   # paste0(recent_year,
        #   " Status: ",
          # renewable_percent_gen_recent,
          # "% of Generation from RPS Eligible Sources"
   # ),
  #  "Goal: 100% of Generation from<br>RPS Eligible Sources by 2050",
  #  "label+value",
  #  c("#5868AC","#3C5488B2"),
   # list("eia_elec_gen_sun_va_99_a","VCEA_storage")
#  )
#single_ring_renewable_donut_p

#single_ring_offshore_wind_capacity_donut_p
offshore_wind_ring <- offshore_wind_capacity_data()
single_ring_offshore_wind_capacity_donut_p <-
  single_ring_donut_figure_p(
    offshore_wind_ring,
    "Offshore Wind Capacity",
    paste(2021, #set manually for the time being
          "Status:",
          format(
            offshore_wind_ring$value[1],
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          ),
          "MW of Offshore Wind Capacity"
    ),
    paste(
      "Goal:",
      format(
        5200,
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

#percent_renewable_and_carbon_free_line_p
# Graphing % of VA power generation (in GWh/yr) from renewables & carbon-free sources
percent_renewable_and_carbon_free <- renewable_and_carbon_free_plot_data()
percent_renewable_and_carbon_free_line <-
  line_figure(
    percent_renewable_and_carbon_free,
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

#rps_renewable_line_p
rps_mandate_schedule <- renewable_line_plot_data()
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
  scale_color_manual(name = NULL, values = theme_colors[3:4]) +
  theme_ceps()
#put a plotly wrapper on it
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

#va_elec_net_imports_line_p
electricity_imports <- import_data()
va_elec_net_imports_line <-
  line_figure(
    electricity_imports,
    "Interstate Electricity Flow (GWh)",
    "Virginia Net Interstate Electricity Flow",
    return_static = F,
    source_citation = "Source: U.S. Energy Information Administration",
    subtitle_description = "Positive Values = Imports",
    modifications = theme(legend.position = "none"),
    modifications2 = scale_y_continuous(label=comma,limits=c(0,54000))
  )
va_elec_net_imports_line_p <-
  ggplotly_wrapper(va_elec_net_imports_line) 
va_elec_net_imports_line_p

#annual_carbon_free_generation_by_type_line_p
carbon_free_data <- annual_carbon_free_data()
annual_carbon_free_generation_by_type_line <-
  line_figure(
    carbon_free_data,
    "Generation (GWh)",
    "Virginia Carbon-Free Electricity Generation",
    list(
      "eia_elec_gen_nuc_va_99_a"
    ),
    return_static = F,
    modifications = scale_y_continuous(label=comma,limits=c(0,32000))
  )
annual_carbon_free_generation_by_type_line

annual_carbon_free_generation_by_type_line_p <-
  ggplotly_wrapper(annual_carbon_free_generation_by_type_line)
annual_carbon_free_generation_by_type_line_p

#solar_generation_time_series_line_p
solar_data <- solar_gen_data()
solar_generation_time_series_line <-
  line_figure(
    solar_data,
    "Generation (GWh)",
    "Virginia Solar Electricity Generation",
    list("eia_elec_gen_sun_va_99_a"),
    return_static = F,
    subtitle_description = "Utility Scale and Distributed",
    modifications = scale_y_continuous(label=comma,limits=c(0,5000))
  )
solar_generation_time_series_line
solar_generation_time_series_line_p <-
  ggplotly_wrapper(solar_generation_time_series_line)
solar_generation_time_series_line_p
rm(solar_data)

#wind_projected_generation_time_series_line_p
#hovertext needs commas
wind_data <- wind_projected_generation_data()
wind_data <- wind_data %>%
  mutate(y_value=y_value/1000) #divide by 1000 to convert MWh to GWh
wind_projected_generation_time_series_line <-
  line_figure(
    wind_data,
    "Projected Generation (GWh)",
    "Virginia Projected Offshore Wind Electricity Generation",
    list("total_production_forecast_offshore_wind"),
    return_static = F,
    modifications =  theme(legend.position = "none"),
    subtitle_description = "Planned",
    future_date = 2021,
    modifications2 = scale_y_continuous(label=comma,limits=c(0,10000))
  )
wind_projected_generation_time_series_line

wind_projected_generation_time_series_line_p <-
  ggplotly_wrapper(wind_projected_generation_time_series_line)
wind_projected_generation_time_series_line_p

### Need to fix Y-axis
#manually re-scaling y-axis so that value of 44 doesn't look like 0
#wind_projected_generation_time_series_line$figure <-
 # wind_projected_generation_time_series_line$figure +
  #scale_y_continuous(
   # breaks = c(44, 2500, 5000, 7500),
    #labels = c("44", "2500", "5000", "7500")
#  )
#wind_projected_generation_time_series_line

#wind_projected_capacity_line_p
total_mw_offshore_wind <- wind_projected_capacity_data()
wind_projected_capacity_line <-
  line_figure(
    total_mw_offshore_wind,
    "Projected Capacity (MW)",
    "Coastal Virginia Offshore Wind (CVOW) Capacity",
    list("total_mw_offshore_wind"),
    return_static = F,
    subtitle_description = "Planned",
    future_date = as.numeric(format(Sys.Date(), "%Y")) + 1,
    modifications = scale_y_continuous(label=comma,limits=c(0,3000))
    
  )
wind_projected_capacity_line

wind_projected_capacity_line_p <-
  ggplotly_wrapper(wind_projected_capacity_line)
wind_projected_capacity_line_p
rm(total_mw_offshore_wind)

#single_ring_carbon_free_donut_p
#carbon_free_ring <- carbon_free_ring_data()
#single_ring_carbon_free_donut_p <-
  #single_ring_donut_figure_p(
    #carbon_free_ring,
    #"Carbon-Free Generation",
   # paste0(recent_year,
          # " Status: ",
         #  carbon_free_percent_gen_recent,
          # "% of Generation from Carbon-Free Sources"
    #),
   # "Goal: 100% of Generation from<br>Carbon-Free Sources by 2050",
   # "label+value",
   # c("#BE7E8A","#CEA5AC"),
    #list("eia_elec_gen_nuc_va_99_a", "VCEA_storage")
 # )
#single_ring_carbon_free_donut_p

#ENERGY EFFICIENCY PAGE---------------------------------------------------------------------------------------------------------------------------------

#annual_kwh_by_square_feet
year_by_building_size <- energycap_by_building_size_data()
yearly_values_by_size <- plot_ly(year_by_building_size, x = ~year, y = ~kWh/sqft, type = 'bar', width="500px",
                                 color = ~size_range, colors = theme_colors,
                                 text= ~paste("Kilowatt Hours per Square Foot: ",round(kWh/sqft,digits=2),
                                              "<br> Cost per Square Foot: $",round(cost/sqft,digits=2),
                                              "<br> Cost per Kilowatt Hour: $", round(cost/kWh,digits=2),
                                              "<br> Number of Buildings: ",buildings,
                                              "<br> Annual Kilowatt Hour Savings Per Square Foot: ",round(savings/kWh,digits=2)),
                                 hoverinfo="text") %>% 
  layout(title="Annual Power Use by Facility Size",xaxis = list(title = "Year", tickangle = -0),yaxis = list(title = "Kilowatt Hours per Square Foot"),
         margin = list(b = 100),
         barmode = 'group',
         legend = list(title=list(text='<b> Square Footage Range </b>')),
         paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0")
yearly_values_by_size

#buildings_tracked
by_agency_category <- building_tracking_by_agency_category_data()
agency_category_progress <- ggplot(by_agency_category) + 
  geom_col(aes(agency_category,percent_done,
               text=paste("Facilities Over 5,000 Square Feet: ",facilities_over_5000_sqft,
                          "<br> Facilities Over 5,000 Square Feet Being Tracked: ", facilities_over_5000_sqft_tracked,
                          "<br> Percent Complete: ", percent(percent_done))),fill="#56B4E9") + 
  geom_hline(aes(yintercept=yearly_goal, color=yearly_goal_label))+
  theme(axis.text.x = element_text(angle=-0, vjust=1, hjust=1))+
  labs(x="",y='Percent of Facilties Tracked',color='Facility Tracking Goals',title='Tracking Progress by Agency Category')+
  scale_y_continuous(labels=percent)+
  scale_color_manual(values=theme_colors#c(paletteer_d("ggthemes::colorblind"))
  ) +
  coord_flip() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),legend.background = element_rect(fill="#F0F0F0"))

agency_category_progress_plot <- ggplotly(agency_category_progress,tooltip='text')
agency_category_progress_plot

#apco_ee_spending
apco_ee<- ggplot(vcea_standard_projections) + 
  geom_col(aes(date,apco_ee_percent_projection,
               text=paste('Projected Energy Savings Achievement :',apco_ee_percent_projection*100,'%'))
           ,fill="#56B4E9") + 
  geom_hline(aes(yintercept=apco_ee_mandated_min, color=c('2022 Mandated Minimum','2023 Mandated Minimum',
                                                          '2024 Mandated Minimum','2025 Mandated Minimum')))+
  labs(x="Year",y='Energy Savings as Percent of Annual Jurisdiction Sales',color="",title='Appalachian Power Company Projected Annual Savings')+
  scale_y_continuous(labels=percent)+
  scale_color_manual(values=theme_colors) + coord_flip() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),legend.background = element_rect(fill="#F0F0F0"))
apco_ee_spending <- ggplotly(apco_ee,tooltip='text')
apco_ee_spending

#apco_mandates_and_progress
apco_reshaped <- reshape_spending_data('apco')
apco_mandates_and_progress <- plot_ly(apco_reshaped,x=~spending_to_date,y=~spending_goal, type='bar',orientation='h',
                     name='Spending To Date',color=as.factor('Spending To Date'),colors=theme_colors) %>%
  add_trace(apco_reshaped,x=~spending_requirements,y=~spending_goal,type='bar',orientation='h',
            name='Spending Requirements by 2028',color=as.factor('Spending Requirements by 2028'),colors=theme_colors) %>%
  layout(title='Appalachian Power Company Spending Targets and Progress',xaxis=list(title='Million Dollars',tickprefix='$',tickangle=-0),
         yaxis=list(title=''),
         paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0")
apco_mandates_and_progress

#dominion_ee_spending
dom_ee <- ggplot(vcea_standard_projections) + 
  geom_col(aes(date,dominion_ee_percent_projection,
               text=paste('Projected Energy Savings Achievement :',dominion_ee_percent_projection*100,'%'))
           ,fill="#56B4E9") + 
  geom_hline(aes(yintercept=dominion_ee_mandated_min, color=c('2022 Mandated Minimum','2023 Mandated Minimum',
                                                              '2024 Mandated Minimum','2025 Mandated Minimum')))+
  labs(x="Year",y='Energy Savings as Percent of Annual Jurisdiction Sales',color="",title='Dominion Energy Projected Annual Savings')+
  scale_y_continuous(labels=percent)+
  scale_color_manual(values=theme_colors) + coord_flip() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),legend.background = element_rect(fill="#F0F0F0"))
dom_ee_spending <- ggplotly(dom_ee,tooltip='text')
dom_ee_spending

#dominion_mandates_and_progress
dom_reshaped <- reshape_spending_data('dominion')
dominion_mandates_and_progress <- plot_ly(dom_reshaped,x=~spending_to_date,y=~spending_goal, type='bar', orientation='h',
                         name='Spending To Date',color=as.factor('Spending To Date'),colors=theme_colors) %>%
  add_trace(dom_reshaped,x=~spending_requirements,y=~spending_goal,type='bar', orientation='h',
            name='Spending Requirements by 2028',color=as.factor('Spending Requirements by 2028'),colors=theme_colors) %>%
  layout(title='Dominion Energy Spending Targets and Progress',xaxis=list(title='Million Dollars',tickprefix='$',tickangle=-0),
         yaxis=list(title=''),
         paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0")
dominion_mandates_and_progress

#odp_ee_spending
odp_ee <- ggplot(vcea_standard_projections) + 
  geom_col(aes(date,odp_ee_percent_projection,
               text=paste('Projected Energy Savings Achievement :',odp_ee_percent_projection*100,'%'))
           ,fill="#56B4E9") + 
  geom_hline(aes(yintercept=odp_ee_mandated_min, color=c('2022 Mandated Minimum','2023 Mandated Minimum',
                                                         '2024 Mandated Minimum','2025 Mandated Minimum')))+
  labs(x="Year",y='Energy Savings as Percent of Annual Jurisdiction Sales',color="",title='Old Dominion Power Projected Annual Savings')+
  scale_y_continuous(labels=percent)+
  scale_color_manual(values=theme_colors) + coord_flip() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),legend.background = element_rect(fill="#F0F0F0"))
odp_ee_spending <- ggplotly(odp_ee,tooltip='text')
odp_ee_spending

#odp_mandates_and_progress
odp_reshaped <- reshape_spending_data('odp')
odp_mandates_and_progress <- plot_ly(odp_reshaped,x=~spending_to_date,y=~spending_goal, type='bar', orientation='h',
                    name='Spending To Date',color=as.factor('Spending To Date'),colors=theme_colors) %>%
  add_trace(odp_reshaped,x=~spending_requirements,y=~spending_goal,type='bar',orientation='h',
            name='Spending Requirements by 2028',color=as.factor('Spending Requirements by 2028'),colors=theme_colors) %>%
  layout(title='Old Dominion Power Spending Targets and Progress',xaxis=list(title='Million Dollars',tickprefix='$',tickangle=-0),
         yaxis=list(title=''),
         paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0")
odp_mandates_and_progress

#EMISSIONS PAGE-----------------------------------------------------------------------------------------------------------------------------------------------------------

#co2_combined_emissions_line_p
CO2_emissions <- co2_combined_emissions_data()
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
co2_combined_emissions_line_p <-
  ggplotly_wrapper(co2_combined_emissions_line)
co2_combined_emissions_line_p
rm(CO2_emissions)

#carbon_by_fuel_emissions_stacked_p
emission_data_by_fuel <- co2_by_fuel_data()
carbon_by_fuel_emissions_stacked <-
  stacked_area_figure(
    emission_data_by_fuel,
    "Emissions (million metric tons)",
    "Virginia CO2 Emissions From Electricity Production By Fuel Type",
    list("emissions_co2_by_source_va"),
    return_static = F
  )
carbon_by_fuel_emissions_stacked
carbon_by_fuel_emissions_stacked_p <-
  ggplotly_wrapper(carbon_by_fuel_emissions_stacked)
carbon_by_fuel_emissions_stacked_p
rm(emission_data_by_fuel)

#emissions_per_capita_line_p
co2_per_capita <- co2_per_capita_data()
emissions_per_capita_line <-
  line_figure(
    co2_per_capita,
    "Emissions per Capita (Metric Tons/Person)",
    "Virginia CO2 Emissions per Capita",
    list("fred_vangsp", "eia_emiss_co2_totv_tt_to_va_a"),
    return_static = F,
    modifications = theme(legend.position = "none"),
    modifications2 = scale_x_discrete(breaks=c(1980,1990,2000,2010,2020))
  )
emissions_per_capita_line
emissions_per_capita_line_p <-
  ggplotly_wrapper(emissions_per_capita_line)
emissions_per_capita_line_p
rm(co2_per_capita)

#emissions_per_gdp_line_p
co2_per_GDP <- co2_per_gdp_data()
emissions_per_gdp_line <-
  line_figure(
    co2_per_GDP,
    "Emissions/GDP (Metric Tons/Thousand $)",
    "Virginia CO2 Emissions per Dollar of GDP",
    list("fred_vangsp", "eia_emiss_co2_totv_tt_to_va_a"),
    return_static = F,
    modifications = theme(legend.position = "none"),
    modifications2=scale_x_discrete(breaks=c(1995,2000,2005,2010,2015,2020))
  )
emissions_per_gdp_line
emissions_per_gdp_line_p <- ggplotly_wrapper(emissions_per_gdp_line)
emissions_per_gdp_line_p
rm(co2_per_GDP)

#format the data input for the reactive graphs on the energy efficiency page
#group the building energy use (lead_by_example_data) by primary use for each size
size_1_use <- group_by_place_use('5,000 - 50,000')
size_2_use <- group_by_place_use('50,001 - 100,000')
size_3_use <- group_by_place_use('100,001 - 250,000')
size_4_use <- group_by_place_use('250,001 - 500,000')
size_5_use <- group_by_place_use('500,001 - 990,000')

#format the data input for the reactive building tracking progress by agency category graph
education <- filter_by_agency_categories('Education')
health_and_human_svs <- filter_by_agency_categories('Health and Human Services')
transportation <- filter_by_agency_categories('Transportation')
natural_resources <- filter_by_agency_categories('Natural Resources')
agriculture_and_forestry <- filter_by_agency_categories('Agriculture and Forestry')
culture <- filter_by_agency_categories('Culture')
administration <- filter_by_agency_categories('Administration')
public_safety_and_homeland_security <- filter_by_agency_categories('Public Safety and Homeland Security')
independent_agencies <- filter_by_agency_categories('Independent Agencies')
commerce_and_trade <- filter_by_agency_categories('Commerce and Trade')
veterans_and_defense_affairs <-  filter_by_agency_categories('Veterans and Defense Affairs')
other <- filter_by_agency_categories('Other Services or Category Not Known')


#SAVE PLOTS TO RDATA--------------------------------------------------------------------------------------------------------

#save plot objects to dashboard_output.Rdata
save(
  single_ring_renewable_donut_p,
  single_ring_carbon_free_donut_p,
  single_ring_storage_capacity_donut_p,
  va_annual_production_pie_chart_p_with_legend,
  va_annual_production_area_p,
  va_annual_consumption_pie_chart_p_with_legend,
  va_annual_consumption_area_p,
  single_ring_sw_capacity_donut_p,
  single_ring_offshore_wind_capacity_donut_p,
  percent_renewable_and_carbon_free_line_p,
  rps_renewable_line_p,
  va_elec_net_imports_line_p,
  annual_carbon_free_generation_by_type_line_p,
  solar_generation_time_series_line_p,
  wind_projected_generation_time_series_line_p,
  wind_projected_capacity_line_p,
  single_ring_carbon_free_donut_p,
  yearly_values_by_size,
  agency_category_progress_plot,
  apco_ee_spending,
  apco_mandates_and_progress,
  dom_ee_spending,
  dominion_mandates_and_progress,
  odp_ee_spending,
  odp_mandates_and_progress,
  co2_combined_emissions_line_p,
  carbon_by_fuel_emissions_stacked_p,
  emissions_per_capita_line_p,
  emissions_per_gdp_line_p,
  #energy efficiency data for the reactive visualizations
  size_1_use,
  size_2_use,
  size_3_use,
  size_4_use,
  size_5_use,
  culture,
  health_and_human_svs,
  transportation,
  natural_resources,
  agriculture_and_forestry,
  education,
  administration,
  public_safety_and_homeland_security,
  independent_agencies,
  commerce_and_trade,
  veterans_and_defense_affairs,
  other,
  theme_colors,
  file = "dashboard_output.RData"
)

