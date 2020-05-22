#compilation of all figures relevant to dashboard

library(eia)
library(here)
library(tidyverse)
library(stringr)
library(jsonlite)
library(data.table)
library(ggplot2)
library(dplyr)
library(RPostgreSQL)
library(scales)

db_driver = dbDriver("PostgreSQL")
source(here("ggplot2", "my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

table="whole_electric_industry_capacity"
script  <- paste0("select * from ",table," ;")
capacity <- data.table(dbGetQuery(db,script))

#load in dataframe from database on multiple types of emission compounds
va_emissions_compounds <- dbGetQuery(db, "SELECT * from emission") 

dbDisconnect(db)

co2_emissions_by_fuel <- va_emissions_compounds%>%select(Year, Coal2,`Natural gas2`,Petroleum2, Other2)
  
#get relevant features to analyze, in this case the total emissions for each compound
va_emissions_compounds <- select(va_emissions_compounds, Year, Total, Total1, Total2)
#rename columns
colnames(va_emissions_compounds) <- c("Year", "SO2", "NO", "CO2")
#convert to numeric
va_emissions_compounds <- (sapply(va_emissions_compounds, as.numeric))
#make it a dataframe
va_emissions_compounds <- data.frame(va_emissions_compounds)
#limit data to baseline year of 2000
va_emissions_compounds <- va_emissions_compounds[1:19,]
#convert it into a graphable format for line_graph function
va_emissions_compounds <- va_emissions_compounds %>%
  select(Year, SO2, NO, CO2) %>%
  gather(key = "Compound", value = "emissions_in_million_metric_tons", -Year)

colnames(co2_emissions_by_fuel) <- c('year', "coal","natural_gas", "petroleum", 'other')
co2_emissions_by_fuel <- (sapply(co2_emissions_by_fuel, as.numeric))
#make it a dataframe
co2_emissions_by_fuel <- data.frame(co2_emissions_by_fuel)
co2_emissions_by_fuel <- co2_emissions_by_fuel[1:19,]
co2_emissions_by_fuel <- co2_emissions_by_fuel %>%
  select(year, coal, natural_gas, petroleum, other) %>%
  gather(key = "Fuel Type", value = "emissions_in_million_metric_tons", -year)


source(here::here("my_eia_api_key.R"))

get_EIA_series <- function(eiaKey,series_id) {
  require(jsonlite)
  require(data.table)
  # This function retrieves one EIA time-series with metadata
  # The function returns a list of parts of the series:
  #     seriesID,name,units,frequency,data (as data table)
  
  # eiaKey is your EIA API key
  eiaBase = paste0("http://api.eia.gov/series/?api_key=",eiaKey,"&series_id=") 
  
  vv = paste0(eiaBase,series_id)
  temp = readLines(vv, warn = "F")
  rd <- fromJSON(temp)
  print(paste0("Retrieving: ",rd$series$series_id))
  print(paste0(rd$series$name))
  
  # Now take the 'data' element from the list and make a data frame
  rd2 = data.frame(rd$series$data,stringsAsFactors = F)
  rd2 = data.table(rd2)
  
  setnames(rd2,1,"year"); setnames(rd2,2,'value')
  rd2[,value:=as.numeric(value)]
  rd2[,year:=as.numeric(year)]
  returnList = list(
    series_id = rd$series$series_id,
    name = rd$series$name,
    units = rd$series$units,
    frequency = rd$series$f,
    data = rd2
  )
  return(rd2) 
}

series_list_gen = data.frame(
  series_id=c("ELEC.GEN.COW-VA-99.A",
              "ELEC.GEN.PEL-VA-99.A",
              "ELEC.GEN.NG-VA-99.A",
              "ELEC.GEN.NUC-VA-99.A",
              "ELEC.GEN.SUN-VA-99.A",
              "ELEC.GEN.DPV-VA-99.A",
              "ELEC.GEN.HYC-VA-99.A",
              "ELEC.GEN.WWW-VA-99.A",
              "ELEC.GEN.WAS-VA-99.A",
              "ELEC.GEN.ALL-VA-99.A"),
  fuel=c("coal",
         "oil",
         "gas",
         "nuclear",
         "utility_solar", #note utility_solar currently includes all utility-scale solar but this can be changed to just utility-scale PV if needed
         "distributed_solar",
         "hydropower",
         "wood",
         "other_biomass",
         "total"))

series_list_gen$fuel<-as.character(series_list_gen$fuel)

#building data table `va_annual_generation` by merging data on generation by several fuel types
va_annual_generation <- NULL

for(row in 1:nrow(series_list_gen)){
  table <- series_list_gen[row,"series_id"]
  fuel <- series_list_gen[row,"fuel"]
  
  dt <- get_EIA_series(eiaKey,table)
  setnames(dt,old="value",new=fuel)
  
  if (is.null(va_annual_generation))
  {va_annual_generation <- dt}
  else
  {va_annual_generation <-  merge(va_annual_generation, dt[], by ="year", all=TRUE)}
}

va_annual_generation[is.na(va_annual_generation)]=0
va_annual_generation[,other:=total-(coal+oil+gas+nuclear+utility_solar+distributed_solar+hydropower+wood+other_biomass)]

series_list_con = data.frame(
  series_id=c("SEDS.TERCB.VA.A",
              "SEDS.TECCB.VA.A",
              "SEDS.TEICB.VA.A",
              "SEDS.TEACB.VA.A"),
  sector=c("residential",
           "commercial",
           "industrial",
           "transportation")
)

series_list_con$sector<-as.character(series_list_con$sector)

#building data table `va_annual_consumption` by merging data on consumption by sector
va_annual_consumption <- NULL

for(row in 1:nrow(series_list_con)){
  table <- series_list_con[row,"series_id"]
  sector <- series_list_con[row,"sector"]
  
  dt <- get_EIA_series(eiaKey,table)
  setnames(dt,old="value",new=sector)
  
  if (is.null(va_annual_consumption))
  {va_annual_consumption <- dt}
  else
  {va_annual_consumption <-  merge(va_annual_consumption, dt[], by ="year", all=TRUE)}
}

#isolating renewable and carbon free generation in it's own table
va_annual_renewable_and_carbon_free_gen <- va_annual_generation[,.(year,nuclear,utility_solar,distributed_solar,hydropower,total)]
va_annual_renewable_and_carbon_free_gen[,all_solar:=distributed_solar+utility_solar]

#series ID for CO2 emissions from electric sector in VA
series_id = ("EMISS.CO2-TOTV-EC-TO-VA.A")
virginia_emissions_electric <- get_EIA_series(eiaKey,series_id)
virginia_emissions_electric <- virginia_emissions_electric[1:18,]

#series ID for CO2 emissions in VA
series_id = ("EMISS.CO2-TOTV-TT-TO-VA.A")
virginia_emissions <- get_EIA_series(eiaKey,series_id)
virginia_emissions <- virginia_emissions[1:18,]

source(here::here("ggplot2","viz_functions.R"))

#PLOTTING DONUT FIGURES:

#plotting donut figure of progress towards renewable generation goal
renewable_percent_gen_2019 = va_annual_renewable_and_carbon_free_gen[year==2019,(all_solar+hydropower)/total]
renewable_percent_gen_2030_goal = .3 #30% of Virginia’s electricity from renewables by 2030

renewable_donut <- donut_figure(renewable_percent_gen_2019,"2019","2.6%",renewable_percent_gen_2030_goal,"2030","30%","Renewable Generation","slateblue2","slateblue4")
renewable_donut 

renewable_donut_p <- donut_figure_p(renewable_percent_gen_2019,"2019","2.6%",renewable_percent_gen_2030_goal,"2030","30%","Renewable Generation","skyblue","steelblue")
renewable_donut_p

single_ring_renewable_donut_p <- single_ring_donut_figure_p(renewable_percent_gen_2019,"2019","2.6%",renewable_percent_gen_2030_goal,"2030","30%","Renewable Generation","skyblue","steelblue")
single_ring_renewable_donut_p

#plotting donut figure of progress towards carbon-free generation goal
carbon_free_percent_gen_2019 = va_annual_renewable_and_carbon_free_gen[year==2019,(all_solar+hydropower+nuclear)/total]
carbon_free_percent_gen_2050_goal = 1 #100% of Virginia’s electricity from carbon-free sources by 2050

carbon_free_donut <- donut_figure(carbon_free_percent_gen_2019,"2019","32.9%",carbon_free_percent_gen_2050_goal,"2050","100%","Carbon-Free Generation","palegreen3","palegreen4")
carbon_free_donut

carbon_free_donut_p <- donut_figure_p(carbon_free_percent_gen_2019,"2019","32.9%",carbon_free_percent_gen_2050_goal,"2050","100%","Carbon-Free Generation","mediumseagreen","seagreen")
carbon_free_donut_p

single_ring_carbon_free_donut_p <- single_ring_donut_figure_p(carbon_free_percent_gen_2019,"2019","32.9%",carbon_free_percent_gen_2050_goal,"2050","100%","Carbon-Free Generation","mediumseagreen","seagreen")
single_ring_carbon_free_donut_p

#plotting donut figure of progess towards wind and solar capacity goals
solar_capacity_2018_mw = capacity[Year==2018,as.numeric(Solar)]
sw_capacity_2028_goal_mw = 5500 #5,500 MW of onshore wind and solar energy total [in operation] by 2028
sw_capacity_2030_goal_mw = 13600 #13,600 MW of onshore wind and solar energy total by 2030 (from 'Virginia Clean Economy progress dashboard -- UPDATED DRAFT')

solar_capacity_percent_2018 = solar_capacity_2018_mw/sw_capacity_2030_goal_mw
sw_capacity_percent_goal_2028 = sw_capacity_2028_goal_mw/sw_capacity_2030_goal_mw
sw_capacity_percent_goal_2030 = sw_capacity_2030_goal_mw/sw_capacity_2030_goal_mw

sw_capacity_donut <- donut_figure(solar_capacity_percent_2018,"2018","392.5 MW",sw_capacity_percent_goal_2028,"2028","5,500 MW in Operation","Wind & Solar Energy","indianred2","indianred3",sw_capacity_percent_goal_2030,"2030","13,600 MW Total","indianred4")
sw_capacity_donut

sw_capacity_donut_p = donut_figure_p(solar_capacity_percent_2018,"2018","392.5 MW",sw_capacity_percent_goal_2028,"2028","5,500 MW in Operation","Wind & Solar Energy","lightcoral","indianred",sw_capacity_percent_goal_2030,"2030","13,600 MW Total","maroon")
sw_capacity_donut_p

single_ring_sw_capacity_donut_p <- single_ring_donut_figure_p(solar_capacity_percent_2018,"2018","392.5 MW",sw_capacity_percent_goal_2028,"2028","5,500 MW in Operation","Wind & Solar Energy","lightcoral","indianred",sw_capacity_percent_goal_2030,"2030","13,600 MW Total","maroon")
single_ring_sw_capacity_donut_p

#PLOTTING GENERATION/PRODUCTION FIGURES:

lf_va_annual_generation <- melt(va_annual_generation[,.(year,coal,oil,gas,nuclear,utility_solar,distributed_solar,hydropower,wood,other_biomass,other)],id="year")

va_annual_production_area = stacked_area_figure(lf_va_annual_generation,"GWh","VA Annual Generation",subtitle_name = "By Fuel Type",lower_limit = -1900)
va_annual_production_area

va_annual_production_2019_pie_chart = pie_chart_figure(lf_va_annual_generation[year==2019&variable!="other"],"VA 2019 Generation",percent_label_size = 0) #setting percent_label_size = 0 to remove percent labels because slivers are so small
#other is excluded because the pie chart function does not take negative values

#finding location of labels and percent label for each fuel type so that labels for the larger pie slices (gas and nuclear) can be manually added
va_2019_gen = lf_va_annual_generation[year==2019&variable!="other"]

va_2019_gen <- va_2019_gen %>% 
  arrange(desc(variable)) %>%
  mutate(prop = value / sum(va_2019_gen$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
va_2019_gen=data.table(va_2019_gen)

va_annual_production_2019_pie_chart = va_annual_production_2019_pie_chart +
  geom_text(aes(y=va_2019_gen[variable=="gas",ypos],label=paste0(as.character(va_2019_gen[variable=="gas",round(prop,1)]),"%")),color="white",size=4)+
  geom_text(aes(y=va_2019_gen[variable=="nuclear",ypos],label=paste0(as.character(va_2019_gen[variable=="nuclear",round(prop,1)]),"%")),color="white",size=4) 
va_annual_production_2019_pie_chart

va_annual_production_2019_pie_chart_p = pie_chart_figure_p(va_2019_gen,"VA 2019 Generation")
va_annual_production_2019_pie_chart_p

va_annual_production_2019_pie_chart_p_with_legend = pie_chart_figure_p(va_2019_gen,"VA 2019 Generation",legend_shown = TRUE)
va_annual_production_2019_pie_chart_p_with_legend

#PLOTTING CONSUMPTION FIGURES:

lf_va_annual_consumption <- melt(va_annual_consumption,id="year")

va_annual_consumption_area = stacked_area_figure(lf_va_annual_consumption,"Billion Btu","VA Annual Consumption",subtitle_name = "By Sector") + scale_y_continuous(labels = comma)
va_annual_consumption_area

va_annual_consumption_2017_pie_chart = pie_chart_figure(lf_va_annual_consumption[year==2017],"VA 2017 Consumption")
va_annual_consumption_2017_pie_chart

va_annual_consumption_2017_pie_chart_p = pie_chart_figure_p(lf_va_annual_consumption[year==2017],"VA 2017 Consumption")
va_annual_consumption_2017_pie_chart_p

va_annual_consumption_2017_pie_chart_p_with_legend = pie_chart_figure_p(lf_va_annual_consumption[year==2017],"VA 2017 Consumption",legend_shown = TRUE)
va_annual_consumption_2017_pie_chart_p_with_legend

#PLOTTING RENEWABLE & CARBON-FREE GENERATION IN PARTICULAR:

# Finding sum of total annual renewable generation
va_annual_renewable_and_carbon_free_gen[,renewable:=all_solar+hydropower]

# Finding total annual renewable generation as a percent of total energy generation
va_annual_renewable_and_carbon_free_gen[,percent_renewable:=(renewable/total)*100]

# Finding sum of total annual carbon-free generation
va_annual_renewable_and_carbon_free_gen[,carbon_free:=hydropower+all_solar+nuclear]

# Finding total annual carbon-free generation as a percent of total energy generation
va_annual_renewable_and_carbon_free_gen[,percent_carbon_free:=(carbon_free/total)*100]

# Graphing % of VA power generation (in GWh/yr) from renewables & carbon-free sources 
melted_percent_renewable_and_carbon_free<-melt(va_annual_renewable_and_carbon_free_gen[,.(year,percent_renewable,percent_carbon_free)],id="year")

percent_renewable_and_carbon_free_line<-line_figure(melted_percent_renewable_and_carbon_free,"Percentage","Percentage of Total Annual VA Energy Generation")
percent_renewable_and_carbon_free_line

# Solar, Hydro, and Nuclear Generation over Time
melted_generation <- melt(va_annual_renewable_and_carbon_free_gen[,.(year,all_solar,hydropower,nuclear,total)],id="year")

annual_carbon_free_generation_by_type_line<-line_figure(melted_generation,"GWh","Annual VA Generation by Carbon-Free Sources")
annual_carbon_free_generation_by_type_line

# Solar (broken into distributed and utility), Hydro, and Nuclear Generation over Time
melted_generation2 <- melt(va_annual_renewable_and_carbon_free_gen[,.(year,utility_solar,distributed_solar,hydropower,nuclear,total)],id="year")

annual_carbon_free_generation_by_type_line2<-line_figure(melted_generation2,"GWh","Annual VA Generation by Carbon-Free Sources")
annual_carbon_free_generation_by_type_line2

# Solar (broken into distributed and utility) over time
solar_generation_time_series_line<-line_figure(melted_generation2[variable!="total"&variable!="nuclear"&variable!="hydropower"],"GWh","Annual VA Generation of Solar Energy")
solar_generation_time_series_line

# Wood generation over time
wood_generation_time_series_line<-line_figure(lf_va_annual_generation[variable=="wood"],"GWh","Annual VA Energy Generation from Wood")
wood_generation_time_series_line

#Stacked Annual Carbon Free Generation Broken Out by Type
carbon_free_generation_by_type_stacked<-stacked_area_figure(melted_generation2[variable!="total"],"GWh","Annual VA Generation by Carbon-Free Sources")
carbon_free_generation_by_type_stacked

# Stacked Annual Renewable Generation Broken Out by Type (hydro, utility solar, distributed solar)
renewable_generation_by_type_stacked<-stacked_area_figure(melted_generation2[variable!="total"&variable!="nuclear"],"GWh","Annual VA Generation by Renewable Sources")
renewable_generation_by_type_stacked

# Stacked Renewable versus Non-renewable Generation
va_annual_renewable_and_carbon_free_gen[,not_renewable:=total-renewable]
melted_renewable_and_non_renewable<-melt(va_annual_renewable_and_carbon_free_gen[,.(year,renewable,not_renewable)],id="year")

renewable_versus_non_renewable_stacked<-stacked_area_figure(melted_renewable_and_non_renewable,"GWh","Annual VA Renewable and Non-Renewable Generation")
renewable_versus_non_renewable_stacked

# Stacked Carbon versus Carbon Free Generation
va_annual_renewable_and_carbon_free_gen[,carbon_emitting:=total-carbon_free]
melted_carbon_and_carbon_free<-melt(va_annual_renewable_and_carbon_free_gen[,.(year,carbon_free,carbon_emitting)],id="year")

carbon_versus_carbon_free_stacked<-stacked_area_figure(melted_carbon_and_carbon_free,"GWh","Annual VA Carbon Emitting and Carbon-Free Generation")
carbon_versus_carbon_free_stacked

# Total Renewable and Total Carbon Free Generation Over Time
melted_renewable_and_carbon_free<-melt(va_annual_renewable_and_carbon_free_gen[,.(year,carbon_free,renewable,total)],id="year")

renewable_and_carbon_free_line<-line_figure(melted_renewable_and_carbon_free,"GWh","Annual VA Generation by Type",annual=TRUE,x_label="Year",subtitle_name=NULL,lower_limit=0)
renewable_and_carbon_free_line

#PLOTTING EMISSIONS FIGURES:
virginia_emissions[,variable:="co2_emissions"] #adding variable column so that line_figure function can be utilized
co2_emissions_line <- line_figure(virginia_emissions,"emissions (million metric tons CO2)","Virginia Annual CO2 Emissions") + 
  theme(legend.position = "none") #removing legend as only one line is plotted
co2_emissions_line

virginia_emissions_electric[,variable:="co2_emissions_electric"]
co2_electric_emissions_line<-line_figure(virginia_emissions_electric,"emissions (million metric tons CO2)","Virginia Annual CO2 Emissions from Electric Sector") +
  theme(legend.position = "none") #removing legend as only one line is plotted
co2_electric_emissions_line

setnames(va_emissions_compounds,old=c("Compound","emissions_in_million_metric_tons","Year"),new=c("variable","value","year")) #changing names to fit function inputs
emissions_line <- line_figure(va_emissions_compounds,"emissions (million metric tons)","Virginia Annual Emissions") +
  scale_y_continuous(labels = comma)
emissions_line

setnames(co2_emissions_by_fuel,old=c("Fuel Type","emissions_in_million_metric_tons","year"),new=c("variable","value","year")) #changing names to fit function inputs
carbon_by_fuel_emissions_line <- line_figure(co2_emissions_by_fuel,"emissions (million metric tons)","Virginia CO2 Emissions By Fuel Type") +
  scale_y_continuous(labels = comma)
carbon_by_fuel_emissions_line



#--------------------------------------------------------------------------------
# reformatting the generation dataset
va_gen_w_commas<-data.frame(format(va_annual_generation[,2:12],big.mark=",",scientific=FALSE,trim=TRUE))
va_gen_w_commas<-cbind(va_annual_generation[,1],va_gen_w_commas)

# reformatting the consumption dataset
va_con_w_commas<-data.frame(format(va_annual_consumption[,2:5],big.mark=",",scientific=FALSE,trim=TRUE))
va_con_w_commas<-cbind(va_annual_consumption[,1],va_con_w_commas)

#reformatting carbon emissions
virginia_emissions_electric <- virginia_emissions_electric[,1:2]
virginia_emissions_electric_commas <- data.frame(signif(virginia_emissions_electric[,2], digits=4))
virginia_emissions_electric_commas <- cbind(virginia_emissions_electric[,1],virginia_emissions_electric_commas)
colnames(virginia_emissions_electric_commas) <- c('Year','Million Metric Tons of CO2')

#reformatting emissions compounds dataset
db_driver = dbDriver("PostgreSQL")
source(here("ggplot2", "my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
va_emissions_compounds <- dbGetQuery(db, "SELECT * from emission") 
#get relevant features to analyze, in this case the total emissions for each compound
va_emissions_compounds <- select(va_emissions_compounds, Year, Total, Total1, Total2)
#rename columns
colnames(va_emissions_compounds) <- c("Year", "SO2", "NO", "CO2")
#convert to numeric
va_emissions_compounds <- (sapply(va_emissions_compounds, as.numeric))
#make it a dataframe
va_emissions_compounds <- data.frame(va_emissions_compounds)
#limit data to baseline year of 2000
va_emissions_compounds <- va_emissions_compounds[1:19,]
dbDisconnect(db)


