#compilation of all figures relevant to dashboard

library(groundhog)
groundhog.day = "2021-09-01"
pkgs = c("lubridate", "devtools", "here")
groundhog.library(pkgs, groundhog.day)

install_github("coopercenter/cepsvizfunctions")
library(cepsvizfunctions)

source(here::here("calculations_and_plotting", "dashboard_calculations.R")) #sourcing in data and reformatted data tables & calculations ready to serve as input to viz functions

#----------------------------------------------PLOTTING DONUT FIGURES------------------------------------------------------------------------------

#plotting donut figure of progress towards renewable generation goal------------------------------------------------------------------------------

# was `renewable_percent_gen_2019`
recent_year = va_annual_renewable_and_carbon_free_gen %>% select(year) %>% arrange(year)
recent_year = recent_year[[nrow(va_annual_renewable_and_carbon_free_gen),1]]

renewable_percent_gen_recent = round(va_annual_renewable_and_carbon_free_gen[year ==
                                                                               recent_year, (all_solar + hydropower) / (total - nuclear)] * 100, 1)
renewable_percent_gen_2050_goal = 100

renewable_ring = data.frame(
  category = c(
    paste(recent_year, "RPS generation progress (%)"),
    "additional progress necessary to reach goal (%)"
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
recent_year = va_annual_renewable_and_carbon_free_gen %>% select(year) %>% arrange(year)
recent_year = recent_year[[nrow(va_annual_renewable_and_carbon_free_gen),1]]

# was `carbon_free_percent_gen_2019`
carbon_free_percent_gen_recent = round(va_annual_renewable_and_carbon_free_gen[year ==
                                                                               recent_year, (all_solar + hydropower + nuclear) / total] * 100, 1)
carbon_free_percent_gen_2050_goal = 100 #100% of Virginia’s electricity from carbon-free sources by 2050

carbon_free_ring = data.frame(
  category = c(
    "2019 carbon free generation (%)",
    "additional progress necessary to reach goal (%)"
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
recent_year <- pjm_solar_working %>% select(actual_in_service_date) %>% arrange(actual_in_service_date) %>% filter(!is.na(actual_in_service_date)) 
recent_year <- recent_year[[nrow(recent_year),1]] %>% as.POSIXct() %>% year()

# was `solar_capacity_2019_mw`
solar_capacity_current_mw = pjm_solar_working[status == "In Service", sum(mfo)] #currently only solar in service, no wind

# was `onshore_wind_capacity_2019_mw`
onshore_wind_capacity_current_mw = pjm_wind_working[fuel == "Wind" &
                                                   status == "In Service", sum(mfo)]
sw_capacity_2035_mw_goal = 16100 #16,100 MW of solar and onshore wind by January 1, 2024 (from VCEA Summary 3.0)

sw_ring = data.frame(
  category = c(paste0(recent_year, " capacity"),"additional capacity necessary to reach goal"),
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
recent_year <- pjm_storage_working %>% select(actual_in_service_date) %>% arrange(actual_in_service_date) %>% filter(!is.na(actual_in_service_date)) 
recent_year <- recent_year[[nrow(recent_year),1]] %>% as.POSIXct() %>% year

# was`storage_capacity_2019_mw`
storage_capacity_current_mw = pjm_storage_working[status == "In Service", sum(mfo)]
storage_capacity_2035_mw_goal = 3100



storage_ring = data.frame(
  category = c(paste(recent_year, "capacity"),"additional capacity necessary to reach goal"),
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

offshore_wind_current_mw = pjm_wind_working[fuel == "Offshore Wind" &
                                              status == "In Service", sum(mfo)]
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

va_annual_production_area <-
  stacked_area_figure(
    list(
      eia_elec_gen_cow_va_99_a,
      eia_elec_gen_pel_va_99_a,
      eia_elec_gen_ng_va_99_a,
      eia_elec_gen_nuc_va_99_a,
      eia_elec_gen_sun_va_99_a,
      eia_elec_gen_dpv_va_99_a,
      eia_elec_gen_hyc_va_99_a,
      eia_elec_gen_www_va_99_a,
      eia_elec_gen_was_va_99_a,
      other_annual_generation
    ),
    "year",
    "Generation (GWh)",
    "Virginia Electricity Generation by Fuel Type",
    list(
      "eia_elec_gen_cow_va_99_a",
      "eia_elec_gen_pel_va_99_a",
      "eia_elec_gen_ng_va_99_a",
      "eia_elec_gen_nuc_va_99_a",
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a",
      "eia_elec_gen_www_va_99_a",
      "eia_elec_gen_was_va_99_a"
    ),
    lower_limit = -1900,
    return_static = F
  )
va_annual_production_area

va_annual_production_area_p <-
  ggplotly_wrapper(va_annual_production_area)
va_annual_production_area_p

recent_year <- eia_elec_gen_nuc_va_99_a %>% select(year) %>% arrange(year) %>% filter(!is.na(year)) 
recent_year <- recent_year[[nrow(recent_year),1]]

va_annual_production_2019_pie_chart_p <-
  pie_chart_figure_p(
    list(
      eia_elec_gen_cow_va_99_a[year == recent_year],
      eia_elec_gen_pel_va_99_a[year == recent_year],
      eia_elec_gen_ng_va_99_a[year == recent_year],
      eia_elec_gen_nuc_va_99_a[year == recent_year],
      eia_elec_gen_sun_va_99_a[year == recent_year],
      eia_elec_gen_dpv_va_99_a[year == recent_year],
      eia_elec_gen_hyc_va_99_a[year == recent_year],
      eia_elec_gen_www_va_99_a[year == recent_year],
      eia_elec_gen_was_va_99_a[year == recent_year],
      other_annual_generation[year == recent_year]
    ),
    "year", 
    paste0(
    "Virginia ", recent_year, " Electricity Generation by Fuel Type"
    ),
    list(
      "eia_elec_gen_cow_va_99_a",
      "eia_elec_gen_pel_va_99_a",
      "eia_elec_gen_ng_va_99_a",
      "eia_elec_gen_nuc_va_99_a",
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a",
      "eia_elec_gen_www_va_99_a",
      "eia_elec_gen_was_va_99_a"
    )
  )
va_annual_production_2019_pie_chart_p

va_annual_production_2019_pie_chart_p_with_legend <-
  pie_chart_figure_p(
    list(
      eia_elec_gen_cow_va_99_a[year == recent_year],
      eia_elec_gen_pel_va_99_a[year == recent_year],
      eia_elec_gen_ng_va_99_a[year == recent_year],
      eia_elec_gen_nuc_va_99_a[year == recent_year],
      eia_elec_gen_sun_va_99_a[year == recent_year],
      eia_elec_gen_dpv_va_99_a[year == recent_year],
      eia_elec_gen_hyc_va_99_a[year == recent_year],
      eia_elec_gen_www_va_99_a[year == recent_year],
      eia_elec_gen_was_va_99_a[year == recent_year],
      other_annual_generation[year == recent_year]
    ),
    "year",
    paste0(
      "Virginia ", recent_year, " Electricity Generation by Fuel Type"
    ),
    list(
      "eia_elec_gen_cow_va_99_a",
      "eia_elec_gen_pel_va_99_a",
      "eia_elec_gen_ng_va_99_a",
      "eia_elec_gen_nuc_va_99_a",
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a",
      "eia_elec_gen_www_va_99_a",
      "eia_elec_gen_was_va_99_a"
    ),
    legend_shown = T
  )
va_annual_production_2019_pie_chart_p_with_legend

#--------------------------------------------PLOTTING CONSUMPTION FIGURES---------------------------------------------------------------------
va_annual_consumption_area <-
  stacked_area_figure(
    list(
      eia_seds_tercb_va_a,
      eia_seds_teccb_va_a,
      eia_seds_teicb_va_a,
      eia_seds_teacb_va_a
    ),
    "year",
    "Consumption (Billion Btu)",
    "Virginia Energy Consumption by Sector",
    list(
      "eia_seds_tercb_va_a",
      "eia_seds_teccb_va_a",
      "eia_seds_teicb_va_a",
      "eia_seds_teacb_va_a"
    ),
    return_static = F,
    modifications = scale_y_continuous(labels = comma)
  )
va_annual_consumption_area

va_annual_consumption_area_p <-
  ggplotly_wrapper(va_annual_consumption_area)
va_annual_consumption_area_p


recent_year <- eia_seds_tercb_va_a %>% select(year) %>% arrange(year) %>% filter(!is.na(year)) 
recent_year <- recent_year[[nrow(recent_year),1]]

va_annual_consumption_2018_pie_chart_p <-
  pie_chart_figure_p(
    list(
      eia_seds_tercb_va_a[year == recent_year],
      eia_seds_teccb_va_a[year == recent_year],
      eia_seds_teicb_va_a[year == recent_year],
      eia_seds_teacb_va_a[year == recent_year]
    ),
    "year",
    paste0(
      "Virginia ", recent_year, " Energy Consumption by Sector"
    ),
    list(
      "eia_seds_tercb_va_a",
      "eia_seds_teccb_va_a",
      "eia_seds_teicb_va_a",
      "eia_seds_teacb_va_a"
    )
  )
va_annual_consumption_2018_pie_chart_p

va_annual_consumption_2018_pie_chart_p_with_legend <-
  pie_chart_figure_p(
    list(
      eia_seds_tercb_va_a[year == recent_year],
      eia_seds_teccb_va_a[year == recent_year],
      eia_seds_teicb_va_a[year == recent_year],
      eia_seds_teacb_va_a[year == recent_year]
    ),
    "year",
    paste0(
      "Virginia ", recent_year, " Energy Consumption by Sector"
    ),
    list(
      "eia_seds_tercb_va_a",
      "eia_seds_teccb_va_a",
      "eia_seds_teicb_va_a",
      "eia_seds_teacb_va_a"
    ),
    legend_shown = T
  )
va_annual_consumption_2018_pie_chart_p_with_legend

#--------------------------------PLOTTING RENEWABLE & CARBON-FREE GENERATION IN PARTICULAR-----------------------------------------------------

# Graphing % of VA power generation (in GWh/yr) from renewables & carbon-free sources
percent_renewable_and_carbon_free_line <-
  line_figure(
    list(lf_percent_renewable_carbon_free_combined[category == "Historic"]),
    "year",
    "Percentage of Total Generation",
    "Virginia Recent Electricity Generation",
    list(
      "eia_elec_gen_nuc_va_99_a",
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a"
    ),
    return_static = F,
    upper_limit = 100,
    subtitle_description = "Renewable and Carbon-Free"
  )
percent_renewable_and_carbon_free_line

percent_renewable_and_carbon_free_line_p <-
  ggplotly_wrapper(percent_renewable_and_carbon_free_line)
percent_renewable_and_carbon_free_line_p

percent_carbon_free_line <-
  line_figure(
    list(lf_percent_carbon_free),
    "year",
    "Percentage of Total Generation",
    "Virginia Carbon-Free Electricity Generation",
    list(
      "eia_elec_gen_nuc_va_99_a",
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a"
    ),
    return_static = F,
    modifications = theme(legend.position = "none"),
    upper_limit = 100
  )

percent_carbon_free_line

percent_carbon_free_line_p <-
  ggplotly_wrapper(percent_carbon_free_line)
percent_carbon_free_line_p

percent_renewable_and_carbon_free_goal_line <-
  line_figure(
    list(lf_VCEA_goal_percent_gen),
    "year",
    "Percentage of Total Generation",
    "Virginia Electricity Generation Goals",
    list("VCEA_storage"),
    return_static = F,
    subtitle_description = "Renewable and Carbon-Free"
  )
percent_renewable_and_carbon_free_goal_line

rps_renewable_line <-
  ggplot(rps_mandate_schedule, aes(x = year, y = value, color = variable)) +
  geom_line(aes(
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

percent_renewable_and_carbon_free_goal_line_p <-
  ggplotly_wrapper(percent_renewable_and_carbon_free_goal_line)
percent_renewable_and_carbon_free_goal_line_p

recent_year <- eia_elec_gen_nuc_va_99_a %>% select(year) %>% arrange(year) %>% filter(!is.na(year)) 
recent_year <- recent_year[[nrow(recent_year),1]]

percent_renewable_and_carbon_free_goal_combined_line <-
  line_figure(
    list(lf_percent_renewable_carbon_free_combined_dt),
    "year",
    "Percentage of Total Generation",
    "Virginia Electricity Generation",
    list(
      "eia_elec_gen_nuc_va_99_a",
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a",
      "VCEA_storage",
      "va_utility_sales"
    ),
    return_static = F,
    subtitle_description = "Renewable and Carbon Free",
    future_date = as.numeric(recent_year) + 1
  )
percent_renewable_and_carbon_free_goal_combined_line

percent_renewable_and_schedule_goal_combined_line <-
  line_figure(
    list(lf_percent_renewable_and_schedule_combined_dt),
    "year",
    "Percentage of Generation from RPS Eligible Sources",
    "Virginia Renewable Portfolio Standard Schedule",
    list(
      "eia_elec_gen_nuc_va_99_a",
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a",
      "VCEA_storage",
      "va_utility_sales"
    ),
    return_static = F,
    future_date = as.numeric(recent_year) + 1
  )
percent_renewable_and_schedule_goal_combined_line

percent_renewable_and_carbon_free_goal_combined_line_p <-
  ggplotly_wrapper(percent_renewable_and_carbon_free_goal_combined_line)
percent_renewable_and_carbon_free_goal_combined_line_p

percent_renewable_and_schedule_goal_combined_line_p <-
  ggplotly_wrapper(percent_renewable_and_schedule_goal_combined_line)
percent_renewable_and_schedule_goal_combined_line_p

#facet grid
renewable_and_carbon_free_facet_graph <-
  ggplot(data = lf_percent_renewable_carbon_free_combined,
         mapping = aes(x = year, y = value, color = variable)) +
  geom_line(aes(
    group = variable,
    text = paste0("Year: ", year, "\n", "Value: ", value, "\n", "Variable: ", variable)
  )) +
  facet_grid(. ~ category, scales = "free") +
  scale_color_manual(name = NULL, values = ceps_pal[1:2]) +
  xlab("Year") + ylab("Percentage of Total Generation") + ylim(0, NA) +
  labs(title = "Virginia Electricity Generation, Renewable and Carbon-Free",
       subtitle = "Historic vs Goal",
       caption = "Source: U.S. Energy Information Administration, Virginia Clean Economy Act") +
  theme_ceps()
renewable_and_carbon_free_facet_graph

renewable_and_carbon_free_facet_graph_p <-
  subplot(
    percent_renewable_and_carbon_free_line_p,
    percent_renewable_and_carbon_free_goal_line_p,
    shareY = T
  ) %>%
  add_annotations(
    x = 0.5,
    y = -0.13,
    yref = 'paper',
    xref = 'paper',
    text = paste0(
      "<sub><i>Source: U.S. Energy Information Administration, Virginia Clean Economy Act"
    ),
    showarrow = F,
    font = list(size = 14)
  )
renewable_and_carbon_free_facet_graph_p

# Solar, Hydro, and Nuclear Generation over Time
annual_carbon_free_generation_by_type_line <-
  line_figure(
    list(
      eia_elec_gen_nuc_va_99_a,
      eia_elec_gen_sun_va_99_a,
      eia_elec_gen_dpv_va_99_a,
      eia_elec_gen_hyc_va_99_a
    ),
    "year",
    "Generation (GWh)",
    "Virginia Carbon-Free Electricity Generation",
    list(
      "eia_elec_gen_nuc_va_99_a",
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a"
    ),
    return_static = F
  )
annual_carbon_free_generation_by_type_line

annual_carbon_free_generation_by_type_line_p <-
  ggplotly_wrapper(annual_carbon_free_generation_by_type_line)
annual_carbon_free_generation_by_type_line_p

# Solar (broken into distributed and utility) over time
solar_generation_time_series_line <-
  line_figure(
    list(eia_elec_gen_sun_va_99_a[solar_utility != 0], eia_elec_gen_dpv_va_99_a),
    "year",
    "Generation (GWh)",
    "Virginia Solar Electricity Generation",
    list("eia_elec_gen_sun_va_99_a", "eia_elec_gen_dpv_va_99_a"),
    return_static = F,
    subtitle_description = "Utility Scale and Distributed"
  )
solar_generation_time_series_line

solar_generation_time_series_line_p <-
  ggplotly_wrapper(solar_generation_time_series_line)
solar_generation_time_series_line_p

# Wood generation over time
wood_generation_time_series_line <-
  line_figure(
    list(melt(eia_elec_gen_www_va_99_a, id = "year")),
    "year",
    "Generation (GWh)",
    "Virginia Electricity Generation from Wood",
    list("eia_elec_gen_www_va_99_a"),
    return_static = F,
    modifications = theme(legend.position = "none")
  )
wood_generation_time_series_line

wood_generation_time_series_line_p <-
  ggplotly_wrapper(wood_generation_time_series_line)
wood_generation_time_series_line_p

# Projected wind generation overtime
wind_projected_generation_time_series_line <-
  line_figure(
    list(melt(
      total_production_forecast_offshore_wind, id = "Year"
    )),
    "Year",
    "Projected Generation (GWh)",
    "Virginia Projected Offshore Wind Electricity Generation",
    list("total_production_forecast_offshore_wind"),
    return_static = F,
    modifications =  theme(legend.position = "none"),
    subtitle_description = "Planned",
    future_date = 2020
  )
wind_projected_generation_time_series_line

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
wind_projected_capacity_line <-
  line_figure(
    list(total_mw_offshore_wind),
    "Year",
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
carbon_free_generation_by_type_stacked <-
  stacked_area_figure(
    list(
      eia_elec_gen_nuc_va_99_a,
      eia_elec_gen_sun_va_99_a,
      eia_elec_gen_dpv_va_99_a,
      eia_elec_gen_hyc_va_99_a
    ),
    "year",
    "Generation (GWh)",
    "Virginia Carbon-Free Electricity Generation by Source",
    list(
      "eia_elec_gen_nuc_va_99_a",
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a"
    ),
    return_static = F
  )
carbon_free_generation_by_type_stacked

carbon_free_generation_by_type_stacked_p <-
  ggplotly_wrapper(carbon_free_generation_by_type_stacked)
carbon_free_generation_by_type_stacked_p

# Stacked Annual Renewable Generation Broken Out by Type (hydro, utility solar, distributed solar)
renewable_generation_by_type_stacked <-
  stacked_area_figure(
    list(
      eia_elec_gen_sun_va_99_a,
      eia_elec_gen_dpv_va_99_a,
      eia_elec_gen_hyc_va_99_a
    ),
    "year",
    "Generation (GWh)",
    "Virginia Renewable Electricity Generation",
    list(
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a"
    ),
    return_static = F
  )
renewable_generation_by_type_stacked

renewable_generation_by_type_stacked_p <-
  ggplotly_wrapper(renewable_generation_by_type_stacked)
renewable_generation_by_type_stacked_p

# Stacked Renewable versus Non-renewable Generation
renewable_vs_non_renewable <-
  melt(va_annual_renewable_and_carbon_free_gen[, .(year, renewable, not_renewable)], id =
         "year")

renewable_versus_non_renewable_stacked <-
  stacked_area_figure(
    list(renewable_vs_non_renewable),
    "year",
    "Generation (GWh)",
    "Virginia Renewable and Non-Renewable Electricity Generation",
    list(
      "eia_elec_gen_cow_va_99_a",
      "eia_elec_gen_pel_va_99_a",
      "eia_elec_gen_ng_va_99_a",
      "eia_elec_gen_nuc_va_99_a",
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a",
      "eia_elec_gen_www_va_99_a",
      "eia_elec_gen_was_va_99_a"
    ),
    return_static = F
  )
renewable_versus_non_renewable_stacked

renewable_versus_non_renewable_stacked_p <-
  ggplotly_wrapper(renewable_versus_non_renewable_stacked)
renewable_versus_non_renewable_stacked_p

# Stacked Carbon versus Carbon Free Generation
carbon_vs_carbon_free <-
  melt(va_annual_renewable_and_carbon_free_gen[, .(year, carbon_free, carbon_emitting)], id =
         "year")

carbon_versus_carbon_free_stacked <-
  stacked_area_figure(
    list(carbon_vs_carbon_free),
    "year",
    "Generation (GWh)",
    "Virginia Carbon Emitting and Carbon-Free Electricity Generation",
    list(
      "eia_elec_gen_cow_va_99_a",
      "eia_elec_gen_pel_va_99_a",
      "eia_elec_gen_ng_va_99_a",
      "eia_elec_gen_nuc_va_99_a",
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a",
      "eia_elec_gen_www_va_99_a",
      "eia_elec_gen_was_va_99_a"
    ),
    return_static = F
  )
carbon_versus_carbon_free_stacked

carbon_versus_carbon_free_stacked_p <-
  ggplotly_wrapper(carbon_versus_carbon_free_stacked)
carbon_versus_carbon_free_stacked_p

# Total Renewable and Total Carbon Free Generation Over Time
renewable_and_carbon_free <-
  melt(va_annual_renewable_and_carbon_free_gen[, .(year, carbon_free, renewable, total)], id =
         "year")

renewable_and_carbon_free_line <-
  line_figure(
    list(renewable_and_carbon_free),
    "year",
    "Generation (GWh)",
    "Virginia Renewable and Carbon-Free Electricity Generation",
    list(
      "eia_elec_gen_cow_va_99_a",
      "eia_elec_gen_pel_va_99_a",
      "eia_elec_gen_ng_va_99_a",
      "eia_elec_gen_nuc_va_99_a",
      "eia_elec_gen_sun_va_99_a",
      "eia_elec_gen_dpv_va_99_a",
      "eia_elec_gen_hyc_va_99_a",
      "eia_elec_gen_www_va_99_a",
      "eia_elec_gen_was_va_99_a"
    ),
    return_static = F
  )
renewable_and_carbon_free_line

renewable_and_carbon_free_line_p <-
  ggplotly_wrapper(renewable_and_carbon_free_line)
renewable_and_carbon_free_line_p

# VA Electricity Net Imports
va_elec_net_imports_line <-
  line_figure(
    list(melt(va_elec_import, id = "year")),
    "year",
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

#--------------------------PLOTTING WIND AND SOLAR PROJECTED CAPACITY ADDITIONS (AND STORAGE)----------------------------------

apco_dom_target_vs_projected_capacity <-
  line_figure(
    list(lf_apco_dom_onwind_and_solar[date <= '2040-01-01']),
    "date",
    "Capacity (MW)",
    "Appalachian Power & Dominion Utility Onshore Wind and Solar",
    list("pjm_solar", "pjm_wind", "VCEA_onshore_wind_solar"),
    return_static = F,
    subtitle_description = "Current and Projected Capacity vs VCEA Target Capacity",
    x_label = "Date"
  )
apco_dom_target_vs_projected_capacity

apco_dom_target_vs_projected_capacity_p <-
  ggplotly_wrapper(apco_dom_target_vs_projected_capacity)
apco_dom_target_vs_projected_capacity_p

future_date <- format(Sys.Date(), "%Y-%m-%d") %>% as.Date() %m+% years(5)

apco_dom_projected_capacity <-
  line_figure(
    list(lf_apco_dom_onwind_and_solar[variable != "target_apco_onshore_wind_and_solar" &
                                        variable != "target_dom_onshore_wind_and_solar" & date < "2024-01-01"]),
    "date",
    "Capacity (MW)",
    "Appalachian Power & Dominion Utility Onshore Wind and Solar",
    list("pjm_solar", "pjm_wind"),
    return_static = F,
    subtitle_description = "Current and Projected Capacity",
    x_label = "Date"
  )
apco_dom_projected_capacity

apco_dom_projected_capacity_p <-
  ggplotly_wrapper(apco_dom_projected_capacity)
apco_dom_projected_capacity_p

apco_dom_target_capacity <-
  line_figure(
    list(melt(VCEA_onshore_wind_solar[, .(year,
                                          target_apco_onshore_wind_and_solar,
                                          target_dom_onshore_wind_and_solar)], id = "year")),
    "year",
    "Capacity (MW)",
    "Appalachian Power & Dominion Utility Onshore Wind and Solar",
    list("VCEA_onshore_wind_solar"),
    return_static = F,
    subtitle_description = "VCEA Target Capacity",
    x_label = "Year"
  )
apco_dom_target_capacity

apco_dom_target_capacity_p <-
  ggplotly_wrapper(apco_dom_target_capacity)
apco_dom_target_capacity_p

on_off_wind_solar_line <-
  line_figure(
    list(lf_wind_and_solar_capacity_projections),
    "date",
    "Capacity (MW)",
    "Virginia Current and Projected Utility Wind and Solar Capacity",
    list("pjm_wind", "pjm_solar"),
    return_static = F,
    x_label = "Date"
  )
on_off_wind_solar_line

on_off_wind_solar_line_p <- ggplotly_wrapper(on_off_wind_solar_line)
on_off_wind_solar_line_p

on_off_wind_solar_area <-
  stacked_area_figure(
    list(lf_wind_and_solar_capacity_projections),
    "date",
    "Capacity (MW)",
    "Virginia Current and Projected Utility Wind and Solar Capacity",
    list("pjm_wind", "pjm_solar"),
    return_static = F,
    x_label = "Date"
  )
on_off_wind_solar_area

on_off_wind_solar_area_p <- ggplotly_wrapper(on_off_wind_solar_area)
on_off_wind_solar_area_p

dominion_offshore_wind_projected_capacity <-
  line_figure(
    list(lf_wind_and_solar_capacity_projections %>% filter(variable =="offshore_wind") %>% filter(!is.na(value))),
    "date",
    "Capacity (MW)",
    "Dominion Projected Offshore Wind Capacity",
    list("pjm_wind", "VCEA_onshore_wind_solar"),
    return_static = F,
    x_label = "Date",
    subtitle_description = "Note: VCEA requires Dominion to develop 5,200 MW of offshore wind by 2034",
    modifications = theme(legend.position = "none")
  )
dominion_offshore_wind_projected_capacity

dominion_offshore_wind_projected_capacity_p <-
  ggplotly_wrapper(dominion_offshore_wind_projected_capacity)
dominion_offshore_wind_projected_capacity_p

storage_projected_capacity_line <-
  line_figure(
    list(lf_storage_capacity_projections),
    "date",
    "Capacity (MW)",
    "Virginia Current and Projected Utility Storage Capacity",
    list("pjm_solar"),
    #using pjm_solar instead of pjm_storage because its not yet in metadata
    return_static = F,
    modifications = theme(legend.position = "none"),
    x_label = "Date"
  )
storage_projected_capacity_line

storage_projected_capacity_line_p <-
  ggplotly_wrapper(storage_projected_capacity_line)
storage_projected_capacity_line_p

#--------------------------------PLOTTING EMISSIONS FIGURES--------------------------------------------------------

# CO2 total emissions & CO2 emissions from electric sector on same figure
co2_combined_emissions_line <-
  line_figure(
    list(
      eia_emiss_co2_totv_ec_to_va_a,
      eia_emiss_co2_totv_tt_to_va_a
    ),
    "year",
    "Emissions (million metric tons CO2)",
    "Virginia CO2 Emissions from Electricity Production",
    list(
      "eia_emiss_co2_totv_ec_to_va_a",
      "eia_emiss_co2_totv_tt_to_va_a"
    ),
    return_static = F
  )
co2_combined_emissions_line

co2_combined_emissions_line_p <-
  ggplotly_wrapper(co2_combined_emissions_line)
co2_combined_emissions_line_p

# Emissions by compound
# note: to convert from short tons to million metric tons, divide short tons by 1102311.31 & to convert from thousand metric tons to million metric tons, divide by 1000
emissions_line <-
  line_figure(
    list(
      emissions_co2_by_source_va[, .(year = year, CO2 = total / 1000)],
      emissions_no_by_source_va[, .(year = year, NO = total / 1102311.31)],
      emissions_so2_by_source_va[, .(year = year, SO2 = total / 1102311.31)]
    ),
    "year",
    "Emissions (million metric tons)",
    "Virginia Emissions",
    list(
      "emissions_co2_by_source_va",
      "emissions_no_by_source_va",
      "emissions_so2_by_source_va"
    ),
    return_static = F
  )
emissions_line

emissions_line_p <- ggplotly_wrapper(emissions_line)
emissions_line_p

# CO2 emissions by fuel type
carbon_by_fuel_emissions_stacked <-
  stacked_area_figure(
    list(melt(emissions_co2_by_source_va[, .(
      year,
      coal = coal / 1000,
      natural_gas = natural_gas / 1000,
      petroleum = petroleum / 1000,
      other = other / 1000
    )], id = "year")),
    "year",
    "Emissions (million metric tons)",
    "Virginia CO2 Emissions From Electricity Production By Fuel Type",
    list("emissions_co2_by_source_va"),
    return_static = F
  )
carbon_by_fuel_emissions_stacked

carbon_by_fuel_emissions_stacked_p <-
  ggplotly_wrapper(carbon_by_fuel_emissions_stacked)
carbon_by_fuel_emissions_stacked_p

#-------------------------------------PLOTTING ENERGY EFFICIENCY FIGURES--------------------------------------

consumption_per_gdp_line <-
  line_figure(
    list(melt(energy_consumption_per_unit_gdp_va, id = "year")),
    "year",
    "Consumption per GDP (Btu/$)",
    "Virginia Electricity Consumption per GDP",
    list("fred_vangsp", "eia_seds_tetcb_va_a"),
    #for now, may change to derived values table name at some point
    return_static = F,
    modifications = theme(legend.position = "none")
  )
consumption_per_gdp_line

consumption_per_gdp_line_p <-
  ggplotly_wrapper(consumption_per_gdp_line)
consumption_per_gdp_line_p

consumption_per_capita_line <-
  line_figure(
    list(melt(energy_consumption_per_capita_va, id = "year")),
    "year",
    "Consumption per Capita (Billion Btu/Person)",
    "Virginia Electricity Consumption per Capita",
    list("fred_vapop", "eia_seds_tetcb_va_a"),
    #for now, may change to derived values table names at some point
    return_static = F,
    modifications = theme(legend.position = "none")
  )
consumption_per_capita_line

consumption_per_capita_line_p <-
  ggplotly_wrapper(consumption_per_capita_line)
consumption_per_capita_line_p

emissions_per_gdp_line <-
  line_figure(
    list(
      melt(co2_emission_per_thousand_dollars_of_gdp_va, id = "year")
    ),
    "year",
    "Emissions/GDP (Metric Tons/Thousand $)",
    "Virginia CO2 Emissions per unit of GDP",
    list("fred_vangsp", "eia_emiss_co2_totv_tt_to_va_a"),
    return_static = F,
    modifications = theme(legend.position = "none")
  )
emissions_per_gdp_line

emissions_per_gdp_line_p <- ggplotly_wrapper(emissions_per_gdp_line)
emissions_per_gdp_line_p

emissions_per_capita_line <-
  line_figure(
    list(melt(co2_emission_per_capita_va, id = "year")),
    "year",
    "Emissions per Capita (Metric Tons/Person)",
    "Virginia CO2 Emissions per Capita",
    list("fred_vangsp", "eia_emiss_co2_totv_tt_to_va_a"),
    return_static = F,
    modifications = theme(legend.position = "none")
  )
emissions_per_capita_line

emissions_per_capita_line_p <-
  ggplotly_wrapper(emissions_per_capita_line)
emissions_per_capita_line_p

#APCO and Dominion historic sales vs goals
apco_dom_historic_sales_line <-
  line_figure(
    list(lf_apco_dom_historic_sales),
    "year",
    "Sales (GWh)",
    "APCO and Dominion Historic Sales",
    return_static = F,
    source_citation = "Source: U.S. Energy Information Administration, Virgina Clean Economy Act"
  )
apco_dom_historic_sales_line

apco_dom_historic_sales_line_p <-
  ggplotly_wrapper(apco_dom_historic_sales_line)
apco_dom_historic_sales_line_p

apco_dom_sales_goal_line <-
  line_figure(
    list(lf_VCEA_goal_sales_reduction),
    "year",
    "Sales (GWh)",
    "APCO and Dominion VCEA Sales Goals, 2022-2025",
    return_static = F,
    source_citation = "Source: U.S. Energy Information Administration, Virgina Clean Economy Act",
    modifications = geom_line(linetype =
                                "dashed", size = 1.3)
  )
apco_dom_sales_goal_line

apco_dom_sales_goal_line_p <-
  ggplotly_wrapper(apco_dom_sales_goal_line)
apco_dom_sales_goal_line_p

apco_dom_historic_goal_sales_combined_line <-
  line_figure(
    list(lf_apco_dom_sales_combined_dt),
    "year",
    "Total Retail Sales (GWh/year)",
    "Mandated Electricity Reduction Under Virginia Clean Economy Act",
    return_static = F,
    source_citation = "Source: U.S. Energy Information Administration, Dominion Energy Inc.",
    future_date = as.numeric(format(Sys.Date(), "%Y")) + 1
  )
apco_dom_historic_goal_sales_combined_line

apco_dom_historic_goal_sales_combined_line_p <-
  ggplotly_wrapper(apco_dom_historic_goal_sales_combined_line)
apco_dom_historic_goal_sales_combined_line_p

#facet grid
apco_dom_sales_facet_graph <-
  ggplot(data = lf_apco_dom_sales_combined,
         mapping = aes(x = year, y = value, color = variable)) +
  geom_line(aes(
    group = variable,
    text = paste0("Year: ", year, "\n", "Value: ", value, "\n", "Variable: ", variable)
  )) +
  facet_grid(. ~ category, scales = "free") +
  scale_color_manual(name = NULL, values = ceps_pal[1:4]) +
  xlab("Year") + ylab("Sales (GWh)") + ylim(0, NA) +
  labs(title = "Electricty Sales",
       subtitle = "Historic vs Goal",
       caption = "Source:  U.S. Energy Information Administration, Virginia Clean Economy Act") +
  theme_ceps()
apco_dom_sales_facet_graph


#Note: below figures come from ACEEE data - no metadata entries yet so manual citations are used
annual_savings_2020_pie_chart_p <-
  pie_chart_figure_p(
    list(virginia_annual_savings_through_2020[variable != "Total Needed"]),
    title_name = "Virginia Savings through 2020 (MWh)",
    character_list = list("virginia_annual_savings_through_2020"),
    source_citation = "Source: The American Council for an Energy-Efficient Economy"
  )
annual_savings_2020_pie_chart_p

annual_savings_2020_pie_chart_p_with_legend <-
  pie_chart_figure_p(
    list(virginia_annual_savings_through_2020[variable != "Total Needed"]),
    title_name = "Virginia Savings through 2020 (MWh)",
    character_list = list("virginia_annual_savings_through_2020"),
    legend_shown = T,
    source_citation = "Source: The American Council for an Energy-Efficient Economy"
  )
annual_savings_2020_pie_chart_p_with_legend

annual_savings_2022_pie_chart_p <-
  pie_chart_figure_p(
    list(virginia_annual_savings_through_2022[variable != "Total Needed"]),
    title_name = "Virginia Savings through 2022 (MWh)",
    character_list = list("virginia_annual_savings_through_2020"),
    source_citation = "Source: The American Council for an Energy-Efficient Economy"
  )
annual_savings_2022_pie_chart_p

annual_savings_2022_pie_chart_p_with_legend <-
  pie_chart_figure_p(
    list(virginia_annual_savings_through_2022[variable != "Total Needed"]),
    title_name = "Virginia Savings through 2022 (MWh)",
    character_list = list("virginia_annual_savings_through_2020"),
    legend_shown = T,
    source_citation = "Source: The American Council for an Energy-Efficient Economy"
  )
annual_savings_2022_pie_chart_p_with_legend

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
      "#D3D3D3",
      "#00A087B2",
      "#3C5488B2",
      "#CEA5AC",
      "#BE7E8A",
      "#4DBBD5B2",
      "#91D1C2B2"
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

#----------------------------------------PLOTTING GEOSPATIAL DATA----------------------------------------------------------
#energy equity figures

#OPTION A--------------------------------------------------------------------------------------------------------------
#energy burden map showing average energy expenditures by county
#va_avg_annual_energy_cost <- ggplot() +
#  geom_sf(data = virginia_outline, fill = NA,color="dimgrey") +
#  geom_sf(data = va_energy_equity_by_county, aes(fill = avg_annual_energy_cost)) +
#  geom_sf(data = va_energy_equity_by_county, aes(fill = number1,text=paste0(county,"\nEnergy Expenditures: $",avg_annual_energy_cost)),alpha=0) +
#  scale_fill_gradientn(name="Average Annual Energy Cost \nin Dollars\n",colors=ceps_pal[1:5]) +
#  coord_sf(xlim = c(-84, -75), ylim = c(36, 40), expand = FALSE)+
#  labs(title = "Average Annual Energy Cost ($) for Counties in Virginia", caption = "Source: U.S. Department of Energy") +
#  theme(panel.background = element_rect(fill = "#F0F0F0"),
#        panel.grid = element_line(color="#F0F0F0"),
#        axis.text = element_blank(),
#        axis.ticks = element_blank(),
#        plot.background=element_rect(fill="#F0F0F0"),
#        plot.caption=element_text(hjust = 0.5,face="italic"),
#        plot.subtitle = element_text(family="Helvetica",hjust=0.5,color="dimgrey"),
#        plot.title =element_text(family="Helvetica",hjust=0.5,color="dimgrey"),
#        legend.title=element_text(size=7),
#        legend.text=element_text(size=7),
#        legend.justification = "center",
#        legend.background = element_rect(fill = "#F0F0F0"),
#        text = element_text(family = "Helvetica",color = "dimgrey"))
#va_avg_annual_energy_cost

#energy burden map showing average energy expenditures as percent of income by county
#va_avg_annual_energy_percent_exp <-  ggplot() +
#  geom_sf(data = virginia_outline, fill = NA,color="dimgrey") +
#  geom_sf(data = va_energy_equity_by_county, aes(fill = avg_energy_burden_as_percent_income)) +
#  geom_sf(data = va_energy_equity_by_county, aes(fill = number2,text=paste0(county,"\nEnergy Expenditures: ",avg_energy_burden_as_percent_income,"%")),alpha=0) +
#  scale_fill_gradientn(name="Average Annual Energy Cost \nas Percentage of Income\n",colors=ceps_pal[1:5]) +
#  coord_sf(xlim = c(-84, -75), ylim = c(36, 40), expand = FALSE) +
#  labs(title = "Average Energy Burden (% income) for Counties in Virginia", caption = "Source: U.S. Department of Energy") +
#  theme(panel.background = element_rect(fill = "#F0F0F0"),
#        panel.grid = element_line(color="#F0F0F0"),
#        axis.text = element_blank(),
#        axis.ticks = element_blank(),
#        plot.background=element_rect(fill="#F0F0F0"),
#        plot.caption=element_text(hjust = 0.5,face="italic"),
#        plot.subtitle = element_text(family="Helvetica",hjust=0.5,color="dimgrey"),
#        plot.title =element_text(family="Helvetica",hjust=0.5,color="dimgrey"),
#        legend.title=element_text(size=7),
#        legend.text=element_text(size=7),
#        legend.justification = "center",
#        legend.background = element_rect(fill = "#F0F0F0"),
#        text = element_text(family = "Helvetica",color = "dimgrey"))
#va_avg_annual_energy_percent_exp

#va_avg_annual_energy_cost_p <- ggplotly(va_avg_annual_energy_cost,tooltip =c("text")) %>%
#  layout(title = list(text=paste0("Average Annual Energy Cost ($) for Counties in Virginia"),titlefont=list(size=15)),
#         xaxis=list(title = paste0("<i>","<sub>","Source: U.S. Department of Energy","<sub>","<i>"),titlefont=list(size=14)))%>%
#  config(displaylogo = FALSE,
#         modeBarButtonsToRemove = c("pan2d","select2d","lasso2d","zoom2d","autoScale2d","resetScale2d","toggleSpikelines"))%>%
#  style(hoveron = "fills")
#va_avg_annual_energy_cost_p

#va_avg_annual_energy_percent_exp_p <- ggplotly(va_avg_annual_energy_percent_exp,tooltip = c("text")) %>%
#  layout(title = list(text=paste0("Average Energy Burden (% income) for Counties in Virginia"),titlefont=list(size=15)),
#         xaxis=list(title = paste0("<i>","<sub>","Source: U.S. Department of Energy","<sub>","<i>"),titlefont=list(size=14)))%>%
#  config(displaylogo = FALSE,
#         modeBarButtonsToRemove = c("pan2d","select2d","lasso2d","zoom2d","autoScale2d","resetScale2d","toggleSpikelines"))%>%
#  style(hoveron = "fills")
#va_avg_annual_energy_percent_exp_p
#-------------------------------------------------------------------------------------
#OR
#OPTION B---------------------------------------------------------------------------
#energy burden map showing average energy expenditures by county
va_avg_annual_energy_cost <- ggplot() +
  geom_sf(data = virginia_outline, fill = NA, color = "dimgrey") +
  geom_sf(data = va_energy_equity_by_county, aes(
    fill = avg_annual_energy_cost,
    text = paste0(county, "\nEnergy Expenditures: $", avg_annual_energy_cost)
  )) +
  scale_fill_gradientn(name = "Average Annual Energy Cost \nin Dollars\n", colors =
                         ceps_pal[1:5]) + #setting alpha adds some transparency
  coord_sf(xlim = c(-84,-75),
           ylim = c(36, 40),
           expand = FALSE) +
  labs(title = "Average Annual Energy Cost ($) for Counties in Virginia", caption = "Source: U.S. Department of Energy") +
  theme(
    panel.background = element_rect(fill = "#F0F0F0"),
    panel.grid = element_line(color = "#F0F0F0"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#F0F0F0"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    plot.subtitle = element_text(
      family = "Helvetica",
      hjust = 0.5,
      color = "dimgrey"
    ),
    plot.title = element_text(
      family = "Helvetica",
      hjust = 0.5,
      color = "dimgrey"
    ),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.justification = "center",
    legend.background = element_rect(fill = "#F0F0F0"),
    text = element_text(family = "Helvetica", color = "dimgrey")
  )
va_avg_annual_energy_cost

#energy burden map showing average energy expenditures as percent of income by county
va_avg_annual_energy_percent_exp <- ggplot() +
  geom_sf(data = virginia_outline, fill = NA, color = "dimgrey") +
  geom_sf(data = va_energy_equity_by_county,
          aes(
            fill = avg_energy_burden_as_percent_income,
            text = paste0(
              county,
              "\nEnergy Expenditures: ",
              avg_energy_burden_as_percent_income,
              "%"
            )
          )) +
  scale_fill_gradientn(name = "Average Annual Energy Cost \nas Percentage of Income\n", colors =
                         ceps_pal[1:5]) + #setting alpha adds some transparency
  coord_sf(xlim = c(-84,-75),
           ylim = c(36, 40),
           expand = FALSE) +
  labs(title = "Average Energy Burden (% income) for Counties in Virginia", caption = "Source: U.S. Department of Energy") +
  theme(
    panel.background = element_rect(fill = "#F0F0F0"),
    panel.grid = element_line(color = "#F0F0F0"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#F0F0F0"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    plot.subtitle = element_text(
      family = "Helvetica",
      hjust = 0.5,
      color = "dimgrey"
    ),
    plot.title = element_text(
      family = "Helvetica",
      hjust = 0.5,
      color = "dimgrey"
    ),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.justification = "center",
    legend.background = element_rect(fill = "#F0F0F0"),
    text = element_text(family = "Helvetica", color = "dimgrey")
  )
va_avg_annual_energy_percent_exp

va_avg_annual_energy_cost_p <-
  ggplotly(va_avg_annual_energy_cost, tooltip = "text") %>%
  layout(
    title = list(
      text = paste0("Average Annual Energy Cost ($) for Counties in Virginia"),
      titlefont = list(size = 15)
    ),
    xaxis = list(
      title = paste0(
        "<br>",
        "<i>",
        "<sub>",
        "Source: U.S. Department of Energy",
        "<sub>",
        "<i>"
      ),
      titlefont = list(size = 14)
    )
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "pan2d",
      "select2d",
      "lasso2d",
      "zoom2d",
      "autoScale2d",
      "resetScale2d",
      "toggleSpikelines"
    )
  )
va_avg_annual_energy_cost_p

va_avg_annual_energy_percent_exp_p <-
  ggplotly(va_avg_annual_energy_percent_exp, tooltip = "text") %>%
  layout(
    title = list(
      text = paste0("Average Energy Burden (% income) for Counties in Virginia"),
      titlefont = list(size = 15)
    ),
    xaxis = list(
      title = paste0(
        "<br>",
        "<i>",
        "<sub>",
        "Source: U.S. Department of Energy",
        "<sub>",
        "<i>"
      ),
      titlefont = list(size = 14)
    )
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "pan2d",
      "select2d",
      "lasso2d",
      "zoom2d",
      "autoScale2d",
      "resetScale2d",
      "toggleSpikelines"
    )
  )
va_avg_annual_energy_percent_exp_p
#----------------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------------------------------------------------------------------------------
#Saving only the plots that the dashboard uses. This will save R image file into cep-viz folder. Move that R image file into the dashboard file and
#open those objects into the global environment in the dashboard project.
save(
  single_ring_renewable_donut_p,
  single_ring_carbon_free_donut_p,
  single_ring_renewable_donut_p,
  single_ring_carbon_free_donut_p,
  va_annual_production_area_p,
  va_annual_production_2019_pie_chart_p_with_legend,
  co2_combined_emissions_line_p,
  co2_combined_emissions_line_p,
  carbon_by_fuel_emissions_stacked_p,
  va_annual_consumption_area_p,
  va_annual_consumption_area_p,
  va_annual_consumption_2018_pie_chart_p_with_legend,
  percent_renewable_and_carbon_free_line_p,
  percent_carbon_free_line_p,
  percent_renewable_and_schedule_goal_combined_line_p,
  va_gen_w_commas,
  va_con_w_commas,
  virginia_emissions_electric_commas,
  single_ring_sw_capacity_donut_p,
  percent_renewable_and_carbon_free_goal_combined_line_p,
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
  va_avg_annual_energy_cost,
  va_avg_annual_energy_percent_exp,
  
  dollar_reference_figure,
  dollar_reference_figure_p,
  percent_income_reference_figure,
  percent_income_reference_figure_p,
  
  annual_savings_2020_2022_stacked_bar_chart,
  annual_savings_2020_2022_stacked_bar_chart_p,
  apco_dom_historic_goal_sales_combined_line_p,
  file = "dashboard_output.RData"
)

