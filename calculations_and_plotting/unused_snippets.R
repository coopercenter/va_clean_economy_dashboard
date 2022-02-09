
#facet grid
# apco_dom_sales_facet_graph <-
#   ggplot(data = lf_apco_dom_sales_combined,
#          mapping = aes(x = year, y = value, color = variable)) +
#   geom_line(aes(
#     group = variable,
#     text = paste0("Year: ", year, "\n", "Value: ", value, "\n", "Variable: ", variable)
#   )) +
#   facet_grid(. ~ category, scales = "free") +
#   scale_color_manual(name = NULL, values = ceps_pal[1:4]) +
#   xlab("Year") + ylab("Sales (GWh)") + ylim(0, NA) +
#   labs(title = "Electricty Sales",
#        subtitle = "Historic vs Goal",
#        caption = "Source:  U.S. Energy Information Administration, Virginia Clean Economy Act") +
#   theme_ceps()
# apco_dom_sales_facet_graph


#Note: below figures come from ACEEE data - no metadata entries yet so manual citations are used
# annual_savings_2020_pie_chart_p <-
#   pie_chart_figure_p(
#     list(virginia_annual_savings_through_2020[variable != "Total Needed"]),
#     title_name = "Virginia Savings through 2020 (MWh)",
#     character_list = list("virginia_annual_savings_through_2020"),
#     source_citation = "Source: The American Council for an Energy-Efficient Economy"
#   )
# annual_savings_2020_pie_chart_p
# 
# annual_savings_2020_pie_chart_p_with_legend <-
#   pie_chart_figure_p(
#     list(virginia_annual_savings_through_2020[variable != "Total Needed"]),
#     title_name = "Virginia Savings through 2020 (MWh)",
#     character_list = list("virginia_annual_savings_through_2020"),
#     legend_shown = T,
#     source_citation = "Source: The American Council for an Energy-Efficient Economy"
#   )
# annual_savings_2020_pie_chart_p_with_legend
# 
# annual_savings_2022_pie_chart_p <-
#   pie_chart_figure_p(
#     list(virginia_annual_savings_through_2022[variable != "Total Needed"]),
#     title_name = "Virginia Savings through 2022 (MWh)",
#     character_list = list("virginia_annual_savings_through_2020"),
#     source_citation = "Source: The American Council for an Energy-Efficient Economy"
#   )
# annual_savings_2022_pie_chart_p
# 
# annual_savings_2022_pie_chart_p_with_legend <-
#   pie_chart_figure_p(
#     list(virginia_annual_savings_through_2022[variable != "Total Needed"]),
#     title_name = "Virginia Savings through 2022 (MWh)",
#     character_list = list("virginia_annual_savings_through_2020"),
#     legend_shown = T,
#     source_citation = "Source: The American Council for an Energy-Efficient Economy"
#   )
# annual_savings_2022_pie_chart_p_with_legend

#APCO and Dominion historic sales vs goals
# apco_dom_historic_sales_line <-
#   line_figure(
#     list(lf_apco_dom_historic_sales),
#     "year",
#     "Sales (GWh)",
#     "APCO and Dominion Historic Sales",
#     return_static = F,
#     source_citation = "Source: U.S. Energy Information Administration, Virgina Clean Economy Act"
#   )
# apco_dom_historic_sales_line
# 
# apco_dom_historic_sales_line_p <-
#   ggplotly_wrapper(apco_dom_historic_sales_line)
# apco_dom_historic_sales_line_p

# apco_dom_sales_goal_line <-
#   line_figure(
#     list(lf_VCEA_goal_sales_reduction),
#     "year",
#     "Sales (GWh)",
#     "APCO and Dominion VCEA Sales Goals, 2022-2025",
#     return_static = F,
#     source_citation = "Source: U.S. Energy Information Administration, Virgina Clean Economy Act",
#     modifications = geom_line(linetype =
#                                 "dashed", size = 1.3)
#   )
# apco_dom_sales_goal_line
# 
# apco_dom_sales_goal_line_p <-
#   ggplotly_wrapper(apco_dom_sales_goal_line)
# apco_dom_sales_goal_line_p


# Emissions by compound
# note: to convert from short tons to million metric tons, divide short tons by 1102311.31 & to convert from thousand metric tons to million metric tons, divide by 1000
# emissions_line <-
#   line_figure(
#     list(
#       emissions_co2_by_source_va[, .(year = year, CO2 = total / 1000)],
#       emissions_no_by_source_va[, .(year = year, NO = total / 1102311.31)],
#       emissions_so2_by_source_va[, .(year = year, SO2 = total / 1102311.31)]
#     ),
#     "year",
#     "Emissions (million metric tons)",
#     "Virginia Emissions",
#     list(
#       "emissions_co2_by_source_va",
#       "emissions_no_by_source_va",
#       "emissions_so2_by_source_va"
#     ),
#     return_static = F
#   )
# emissions_line
# 
# emissions_line_p <- ggplotly_wrapper(emissions_line)
# emissions_line_p




# apco_dom_target_vs_projected_capacity <-
#   line_figure(
#     list(lf_apco_dom_onwind_and_solar[date <= '2040-01-01']),
#     "date",
#     "Capacity (MW)",
#     "Appalachian Power & Dominion Utility Onshore Wind and Solar",
#     list("pjm_solar", "pjm_wind", "VCEA_onshore_wind_solar"),
#     return_static = F,
#     subtitle_description = "Current and Projected Capacity vs VCEA Target Capacity",
#     x_label = "Date"
#   )
# apco_dom_target_vs_projected_capacity
# 
# apco_dom_target_vs_projected_capacity_p <-
#   ggplotly_wrapper(apco_dom_target_vs_projected_capacity)
# apco_dom_target_vs_projected_capacity_p

future_date <- format(Sys.Date(), "%Y-%m-%d") %>% as.Date() %m+% years(5)

# apco_dom_projected_capacity <-
#   line_figure(
#     list(lf_apco_dom_onwind_and_solar[variable != "target_apco_onshore_wind_and_solar" &
#                                         variable != "target_dom_onshore_wind_and_solar" & date < "2024-01-01"]),
#     "date",
#     "Capacity (MW)",
#     "Appalachian Power & Dominion Utility Onshore Wind and Solar",
#     list("pjm_solar", "pjm_wind"),
#     return_static = F,
#     subtitle_description = "Current and Projected Capacity",
#     x_label = "Date"
#   )
# apco_dom_projected_capacity
# 
# apco_dom_projected_capacity_p <-
#   ggplotly_wrapper(apco_dom_projected_capacity)
# apco_dom_projected_capacity_p

# apco_dom_target_capacity <-
#   line_figure(
#     list(melt(VCEA_onshore_wind_solar[, .(year,
#                                           target_apco_onshore_wind_and_solar,
#                                           target_dom_onshore_wind_and_solar)], id = "year")),
#     "year",
#     "Capacity (MW)",
#     "Appalachian Power & Dominion Utility Onshore Wind and Solar",
#     list("VCEA_onshore_wind_solar"),
#     return_static = F,
#     subtitle_description = "VCEA Target Capacity",
#     x_label = "Year"
#   )
# apco_dom_target_capacity
# 
# apco_dom_target_capacity_p <-
#   ggplotly_wrapper(apco_dom_target_capacity)
# apco_dom_target_capacity_p

# on_off_wind_solar_line <-
#   line_figure(
#     list(lf_wind_and_solar_capacity_projections),
#     "date",
#     "Capacity (MW)",
#     "Virginia Current and Projected Utility Wind and Solar Capacity",
#     list("pjm_wind", "pjm_solar"),
#     return_static = F,
#     x_label = "Date"
#   )
# on_off_wind_solar_line
# 
# on_off_wind_solar_line_p <- ggplotly_wrapper(on_off_wind_solar_line)
# on_off_wind_solar_line_p

# on_off_wind_solar_area <-
#   stacked_area_figure(
#     list(lf_wind_and_solar_capacity_projections),
#     "date",
#     "Capacity (MW)",
#     "Virginia Current and Projected Utility Wind and Solar Capacity",
#     list("pjm_wind", "pjm_solar"),
#     return_static = F,
#     x_label = "Date"
#   )
# on_off_wind_solar_area
# 
# on_off_wind_solar_area_p <- ggplotly_wrapper(on_off_wind_solar_area)
# on_off_wind_solar_area_p






# Stacked Renewable versus Non-renewable Generation
# renewable_vs_non_renewable <-
#   melt(va_annual_renewable_and_carbon_free_gen[, .(year, renewable, not_renewable)], id =
#          "year")
# 
# renewable_versus_non_renewable_stacked <-
#   stacked_area_figure(
#     list(renewable_vs_non_renewable),
#     "year",
#     "Generation (GWh)",
#     "Virginia Renewable and Non-Renewable Electricity Generation",
#     list(
#       "eia_elec_gen_cow_va_99_a",
#       "eia_elec_gen_pel_va_99_a"
#     ),
#     return_static = F
#   )
# renewable_versus_non_renewable_stacked
# 
# renewable_versus_non_renewable_stacked_p <-
#   ggplotly_wrapper(renewable_versus_non_renewable_stacked)
# renewable_versus_non_renewable_stacked_p

# Stacked Carbon versus Carbon Free Generation
# carbon_vs_carbon_free <-
#   melt(va_annual_renewable_and_carbon_free_gen[, .(year, carbon_free, carbon_emitting)], id =
#          "year")
# 
# carbon_versus_carbon_free_stacked <-
#   stacked_area_figure(
#     list(carbon_vs_carbon_free),
#     "year",
#     "Generation (GWh)",
#     "Virginia Carbon Emitting and Carbon-Free Electricity Generation",
#     list(
#       "eia_elec_gen_cow_va_99_a",
#       "eia_elec_gen_pel_va_99_a"
#     ),
#     return_static = F
#   )
# carbon_versus_carbon_free_stacked
# 
# carbon_versus_carbon_free_stacked_p <-
#   ggplotly_wrapper(carbon_versus_carbon_free_stacked)
# carbon_versus_carbon_free_stacked_p

# Total Renewable and Total Carbon Free Generation Over Time
# renewable_and_carbon_free <-
#   melt(va_annual_renewable_and_carbon_free_gen[, .(year, carbon_free, renewable, total)], id =
#          "year")
# 
# renewable_and_carbon_free_line <-
#   line_figure(
#     list(renewable_and_carbon_free),
#     "year",
#     "Generation (GWh)",
#     "Virginia Renewable and Carbon-Free Electricity Generation",
#     list(
#       "eia_elec_gen_cow_va_99_a",
#       "eia_elec_gen_pel_va_99_a"
#     return_static = F
#   )
# renewable_and_carbon_free_line
# 
# renewable_and_carbon_free_line_p <-
#   ggplotly_wrapper(renewable_and_carbon_free_line)
# renewable_and_carbon_free_line_p

#facet grid  <- not used in dashboard
# renewable_and_carbon_free_facet_graph <-
#   ggplot(data = lf_percent_renewable_carbon_free_combined,
#          mapping = aes(x = year, y = value, color = variable)) +
#   geom_line(aes(
#     group = variable,
#     text = paste0("Year: ", year, "\n", "Value: ", value, "\n", "Variable: ", variable)
#   )) +
#   facet_grid(. ~ category, scales = "free") +
#   scale_color_manual(name = NULL, values = ceps_pal[1:2]) +
#   xlab("Year") + ylab("Percentage of Total Generation") + ylim(0, NA) +
#   labs(title = "Virginia Electricity Generation, Renewable and Carbon-Free",
#        subtitle = "Historic vs Goal",
#        caption = "Source: U.S. Energy Information Administration, Virginia Clean Economy Act") +
#   theme_ceps()
# renewable_and_carbon_free_facet_graph
# 
# renewable_and_carbon_free_facet_graph_p <-
#   subplot(
#     percent_renewable_and_carbon_free_line_p,
#     percent_renewable_and_carbon_free_goal_line_p,
#     shareY = T
#   ) %>%
#   add_annotations(
#     x = 0.5,
#     y = -0.13,
#     yref = 'paper',
#     xref = 'paper',
#     text = paste0(
#       "<sub><i>Source: U.S. Energy Information Administration, Virginia Clean Economy Act"
#     ),
#     showarrow = F,
#     font = list(size = 14)
#   )
# renewable_and_carbon_free_facet_graph_p




# VCEA_goal_percent_gen    <- melt(VCEA_goal_percent_gen,id="Year")
# setnames(VCEA_goal_percent_gen,c("Year","variable","value"),
#          c("x_value","fill_variable","y_value"))
# percent_renewable_and_carbon_free_goal_line <-
#   line_figure(
#     lf_VCEA_goal_percent_gen,
#     "Percentage of Total Generation",
#     "Virginia Electricity Generation Goals",
#     list("VCEA_storage"),
#     return_static = F,
#     subtitle_description = "Renewable and Carbon-Free"
#   )
# percent_renewable_and_carbon_free_goal_line
# 
# percent_renewable_and_carbon_free_goal_line_p <-
#   ggplotly_wrapper(percent_renewable_and_carbon_free_goal_line)
# percent_renewable_and_carbon_free_goal_line_p






# For energy equity figures------------------------------------------------------------------------------------------------
#getting citation information from metadata table
expenditures_source <- metadata[db_table_name=="energy_burden_county_expenditures",data_source_full_name]
percent_income_source <- metadata[db_table_name=="energy_burden_county_percent_income",data_source_full_name]


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

