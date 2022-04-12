#PREP--------------------------------------------------------------------------------------------------------------------------------

lbry<-c("lubridate", "devtools", "here","data.table",'plotly','ggplot2')
test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,lbry)

source(here::here("calculations_and_plotting", "dashboard_calculations.R")) #sourcing in data and reformatted data tables & calculations ready to serve as input to viz functions

# Put these in a single file to source in. Replaces most of the viz package
source(here("calculations_and_plotting","single_ring_donut_figure_p.R"))
source(here("calculations_and_plotting","build_source_list.R"))
source(here("calculations_and_plotting","stacked_area_figure.R"))
source(here("calculations_and_plotting","theme_ceps.R"))
source(here("calculations_and_plotting","ggplotly_wrapper.R"))
source(here("calculations_and_plotting","pie_chart_figure_p.R"))
source(here("calculations_and_plotting","line_figure.R"))

#theme colours, not a permanent solution
theme_colors <- c("#00A087B2", "#3C5488B2", "#CEA5AC", "#BE7E8A", "#4DBBD5B2", "#91D1C2B2","#D9C6C9","#8491B4B2","#5868AC","#6FB3D9","#56BD96","#99A9E2","#A94F64","#B0DEFA","#99EEBB","#8FD3FE")

#custom color palette
# need a new color for wind.
#should just replace the instances of this with theme_colours for consistency
ceps_pal <- c("#00A087B2", "#3C5488B2", "#CEA5AC", "#BE7E8A", "#4DBBD5B2", "#91D1C2B2","#D9C6C9","#8491B4B2","#5868AC","#6FB3D9","#56BD96","#99A9E2","#A94F64","#B0DEFA","#99EEBB","#8FD3FE")

#SUMMARY PAGE---------------------------------------------------------------------------------------------------------------------------------------------------

#The renewable_progress_donut
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

#actually creating the plot
single_ring_renewable_donut_p <-
  single_ring_donut_figure_p(
    data_table=renewable_ring,
    description_of_goal="Renewable Portfolio Standard",
    top_description=paste0(recent_year,
           " Status: ",
           renewable_percent_gen_recent,
           "% of Generation from RPS Eligible Sources"
    ),
    bottom_description="Goal: 100% of Generation from<br>RPS Eligible Sources by 2050",
    "label+value",
    colors_list=c("#5868AC","#3C5488B2"),
    character_list=list("eia_elec_gen_sun_va_99_a","VCEA_storage")
  )
single_ring_renewable_donut_p

#carbon_free_donut
#setting up the plot variables
recent_year = eia_annual_data[!is.na(Percent_carbon_free),last(Year)]
carbon_free_percent_gen_recent = round(eia_annual_data[!is.na(Percent_carbon_free),last(Percent_carbon_free)], 1)
carbon_free_percent_gen_2050_goal = 100 #100% of Virginia’s electricity from carbon-free sources by 2050

carbon_free_ring = data.frame(
  category = c(
    paste0(recent_year, " carbon free generation (%)"),
    "Additional progress necessary to reach goal (%)"
  ),
  value = c(
    carbon_free_percent_gen_recent,
    carbon_free_percent_gen_2050_goal - carbon_free_percent_gen_recent
  )
)
#actually generating the plot
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

#energy_storage_donut
#prepping the plot data/variables
storage_capacity_current_mw = va_storage[,sum(capacity_mw)] 
## Temporary fix until storage data retrieval can be fixed
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

#generating the plot
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

#gen_area
#set up the data for this plot and the next one
generation_stacked_area_data = melt(eia_annual_data[Year>2000,.(Year,
                                                                Coal,Oil,Gas,Nuclear,Solar_utility,Solar_distributed,
                                                                Hydropower,Wind,Wood,Other_biomass
)],id="Year")
generation_stacked_area_data[,variable:=gsub('_',' ',variable)]
generation_stacked_area_data[,variable:=gsub('Solar utility','Solar, utility',variable)]
generation_stacked_area_data[,variable:=gsub('Solar distributed','Solar, distributed',variable)]
setnames(generation_stacked_area_data,c("Year","variable","value"),
         c("x_value","fill_variable","y_value")
)
#generate the visualization
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

#gen_pie
#some additional variable/data setup
latest_year = max(generation_stacked_area_data$x_value)
annual_production_pie_chart_data = generation_stacked_area_data[x_value==latest_year ][,x_value:=NULL]
setnames(annual_production_pie_chart_data,c("y_value","fill_variable"),c("value","variable"))

#generate the plot
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

#con_area
#set up the data
annual_consumption_data = melt(eia_annual_data[,.(Year,Residential,
                                                  Commercial ,Transportation,Industrial)],
                               id="Year")
setnames(annual_consumption_data,c("Year","variable","value"),
         c("x_value","fill_variable","y_value")
) 

annual_consumption_data <- annual_consumption_data %>% filter(y_value !=0)

#generate the plot
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

#con_pie
#set data/variables

latest_year = max(eia_annual_data[Commercial!=0,Year])
annual_consumption_pie_chart_data = annual_consumption_data[x_value==latest_year ][,x_value:=NULL]
setnames(annual_consumption_pie_chart_data,c("y_value","fill_variable"),c("value","variable"))

#generate plot
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

#GENERATION AND CAPACITY PAGE-------------------------------------------------------------------------------------------------------------------------------

#sw_donut
#setting up the data and variables
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

#generating the plot
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

#gen_goal1
#set up the plot data
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

#generate the visualization
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

#offshore_wind_progress
#set up the data
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

#generate the plot
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

#renewable_timeline_plot
#data setup
# Graphing % of VA power generation (in GWh/yr) from renewables & carbon-free sources
lf_percent_renewable_and_carbon_free.plot <- melt(eia_annual_data[!is.na(Percent_renewable | !is.na(Percent_carbon_free)),
                                                                  .(Year,Percent_renewable,Percent_carbon_free)],id="Year")
lf_percent_renewable_and_carbon_free.plot[,variable:=gsub('_',' ',variable)]
lf_percent_renewable_and_carbon_free.plot[,variable:=gsub('Percent carbon free','Percent carbon-free',variable)]
setnames(lf_percent_renewable_and_carbon_free.plot,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))

#make the graph
percent_renewable_and_carbon_free_line <-
  line_figure(
    lf_percent_renewable_and_carbon_free.plot,
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
#set up the plot data
# VA Electricity Net Imports
electricity_imports <- eia_annual_data[Imported_electricity!=0,
                                       .(x_value=Year,y_value=Imported_electricity,
                                         fill_variable="Imports")]
#generate the plot
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
va_elec_net_imports_line_p <-
  ggplotly_wrapper(va_elec_net_imports_line)
va_elec_net_imports_line_p

#rc_break_line
#set plot data/variables
# Solar, Hydro, and Nuclear Generation over Time
carbon_free_data <- melt(eia_annual_data[Nuclear!=0,.(Year,Nuclear,Solar_utility,Solar_distributed,Hydropower,Wind)],id="Year")
# Don't display long lines at zero.
carbon_free_data[Year<2016 & variable=='Solar_utility',value := ifelse(value==0,NA,value)]
carbon_free_data[Year<2013 & variable=='Solar_distributed',value := ifelse(value==0,NA,value)]
carbon_free_data[Year<2021 & variable=='Wind',value := ifelse(value==0,NA,value)]
#carbon_free_data[,Year:=as.factor(Year)]
# Fix legend label text
carbon_free_data[,variable:=gsub('_',', ',variable)]
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

#solar_gen
#data edits
# Solar (broken into distributed and utility) over time
solar_data <- melt(eia_annual_data[Year>2012,.(Year=Year,Solar_utility,Solar_distributed)],id="Year")
# Don't display long lines at zero.
solar_data[Year<2016 & variable=='Solar_utility',value := ifelse(value==0,NA,value)]
solar_data[,Year:=as.factor(Year)]
# Fix legend label text
solar_data[,variable:=gsub('_',', ',variable)]
setnames(solar_data,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))

#create graph
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
solar_generation_time_series_line_p <-
  ggplotly_wrapper(solar_generation_time_series_line)
solar_generation_time_series_line_p
rm(solar_data)

#wind_projected_gen
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
#wind_projected_generation_time_series_line$figure <-
 # wind_projected_generation_time_series_line$figure +
  #scale_y_continuous(
   # breaks = c(44, 2500, 5000, 7500),
    #labels = c("44", "2500", "5000", "7500")
#  )
#wind_projected_generation_time_series_line

wind_projected_generation_time_series_line_p <-
  ggplotly_wrapper(wind_projected_generation_time_series_line)
wind_projected_generation_time_series_line_p

#wind_projected_capacity
#data tweaking
offshore_wind = total_mw_offshore_wind
setnames(offshore_wind,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
#plot creation
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
rm(offshore_wind)

#gen_goal2
#set up data
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

#create the plot
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

#ENERGY EFFICIENCY PAGE---------------------------------------------------------------------------------------------------------------------------------

#annual_kwh_by_square_feet
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
agency_category_progress <- ggplot(sqft_over_5000) + 
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
apco_ee <- ggplot(eia_standard_projections) + 
  geom_col(aes(date,apco_ee_percent_projection,
               text=paste('Projected Energy Savings Achievement :',apco_ee_percent_projection*100,'%'))
           ,fill="#56B4E9") + 
  geom_hline(aes(yintercept=apco_ee_mandated_min, color=c('2022 Mandated Minimum','2023 Mandated Minimum',
                                                          '2024 Mandated Minimum','2025 Mandated Minimum')))+
  labs(x="Year",y='Energy Savings as Percent of Annual Jurisdiction Sales',color="",title='Appalachian Power Company Projected Annual Savings')+
  scale_y_continuous(labels=percent)+
  scale_color_manual(values=theme_colors) + coord_flip() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),legend.background = element_rect(fill="#F0F0F0"))
apco_ee_plot <- ggplotly(apco_ee,tooltip='text')
apco_ee_plot

#apco_mandates_and_progress
apco_plot <- plot_ly(apco_reshaped,x=~spending_to_date,y=~spending_goal, type='bar',orientation='h',
                     name='Spending To Date',color=as.factor('Spending To Date'),colors=theme_colors) %>%
  add_trace(apco_reshaped,x=~spending_requirements,y=~spending_goal,type='bar',orientation='h',
            name='Spending Requirements by 2028',color=as.factor('Spending Requirements by 2028'),colors=theme_colors) %>%
  layout(title='Appalachian Power Company Spending Targets and Progress',xaxis=list(title='Million Dollars',tickprefix='$',tickangle=-0),
         yaxis=list(title=''),
         paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0")
apco_plot

#dominion_ee_spending
dom_ee <- ggplot(eia_standard_projections) + 
  geom_col(aes(date,dominion_ee_percent_projection,
               text=paste('Projected Energy Savings Achievement :',dominion_ee_percent_projection*100,'%'))
           ,fill="#56B4E9") + 
  geom_hline(aes(yintercept=dominion_ee_mandated_min, color=c('2022 Mandated Minimum','2023 Mandated Minimum',
                                                              '2024 Mandated Minimum','2025 Mandated Minimum')))+
  labs(x="Year",y='Energy Savings as Percent of Annual Jurisdiction Sales',color="",title='Dominion Energy Projected Annual Savings')+
  scale_y_continuous(labels=percent)+
  scale_color_manual(values=theme_colors) + coord_flip() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),legend.background = element_rect(fill="#F0F0F0"))
dom_ee_plot <- ggplotly(dom_ee,tooltip='text')
dom_ee_plot

#dominion_mandates_and_progress
dominion_plot <- plot_ly(dom_reshaped,x=~spending_to_date,y=~spending_goal, type='bar', orientation='h',
                         name='Spending To Date',color=as.factor('Spending To Date'),colors=theme_colors) %>%
  add_trace(apco_plot,x=~spending_requirements,y=~spending_goal,type='bar', orientation='h',
            name='Spending Requirements by 2028',color=as.factor('Spending Requirements by 2028'),colors=theme_colors) %>%
  layout(title='Dominion Energy Spending Targets and Progress',xaxis=list(title='Million Dollars',tickprefix='$',tickangle=-0),
         yaxis=list(title=''),
         paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0")
dominion_plot

#odp_ee_spending
odp_ee <- ggplot(eia_standard_projections) + 
  geom_col(aes(date,odp_ee_percent_projection,
               text=paste('Projected Energy Savings Achievement :',odp_ee_percent_projection*100,'%'))
           ,fill="#56B4E9") + 
  geom_hline(aes(yintercept=odp_ee_mandated_min, color=c('2022 Mandated Minimum','2023 Mandated Minimum',
                                                         '2024 Mandated Minimum','2025 Mandated Minimum')))+
  labs(x="Year",y='Energy Savings as Percent of Annual Jurisdiction Sales',color="",title='Old Dominion Power Projected Annual Savings')+
  scale_y_continuous(labels=percent)+
  scale_color_manual(values=theme_colors) + coord_flip() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),legend.background = element_rect(fill="#F0F0F0"))
odp_ee_plot <- ggplotly(odp_ee,tooltip='text')
odp_ee_plot

#odp_mandates_and_progress
odp_plot <- plot_ly(odp_reshaped,x=~spending_to_date,y=~spending_goal, type='bar', orientation='h',
                    name='Spending To Date',color=as.factor('Spending To Date'),colors=theme_colors) %>%
  add_trace(apco_plot,x=~spending_requirements,y=~spending_goal,type='bar',orientation='h',
            name='Spending Requirements by 2028',color=as.factor('Spending Requirements by 2028'),colors=theme_colors) %>%
  layout(title='Old Dominion Power Spending Targets and Progress',xaxis=list(title='Million Dollars',tickprefix='$',tickangle=-0),
         yaxis=list(title=''),
         paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0")
odp_plot

#EMISSIONS PAGE-----------------------------------------------------------------------------------------------------------------------------------------------------------

#electric_emissions_plot2
# CO2 total emissions & CO2 emissions from electric sector on same figure
# The SEDS data is greatly delayed. Maybe we can think of a better presentation here.
# Two more years of data is available beyond what SEDS has. I don't think that includes
#    total energy related CO2 emissions.
all_CO2_emissions = eia_annual_data[Total_CO2_emissions!=0,
                                    .(Year=year(date),
                                      All_sectors = Total_CO2_emissions,
                                      Electricity_sector = Electric_sector_CO2_emissions)]
#CO2_emissions = merge(electricity_CO2_emissions,all_CO2_emissions,by="Year",all=TRUE)
CO2_emissions <- melt(all_CO2_emissions,id="Year")
CO2_emissions[,variable := gsub("_"," ",variable)]
setnames(CO2_emissions,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
#generate the plot
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

#co2_emissions_by_fuel
#set the parameter data
emission_data_by_fuel = melt(va_electricity_emissions_by_fuel[, .(
  Year = Year,
  Coal = CO2_Coal / 1000,
  Natural_gas = CO2_Natural_gas / 1000,
  Petroleum = CO2_Petroleum / 1000,
  Other = CO2_Other / 1000
)], id = "Year")

emission_data_by_fuel[,variable:=gsub('_',' ',variable)]
setnames(emission_data_by_fuel,c("Year","variable","value"),
         c("x_value","fill_variable","y_value"))
#create the graph
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

#emissions_per_capita
co2_per_capita <- intensity_data[!is.na(co2_per_capita),
                                 .(x_value=as.factor(year(date)),y_value=co2_per_capita,
                                   fill_variable="co2_per_capita")]
#create plot
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

#emissions_per_gdp
co2_per_GDP <- intensity_data[!is.na(co2_per_gdp),
                              .(x_value=as.factor(year(date)),y_value=co2_per_gdp,
                                fill_variable="co2_per_gdp")]
#create plot
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
  single_ring_renewable_donut_p,
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
  apco_ee_plot,
  apco_plot,
  dom_ee_plot,
  dominion_plot,
  odp_ee_plot,
  odp_plot,
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

