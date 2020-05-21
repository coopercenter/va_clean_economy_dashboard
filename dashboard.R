library(here)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr, warn.conflicts = FALSE)
library(DT)

source(here("R Shiny","generation_va.R"))
source(here("R Shiny","emissions_va.R"))
source(here("R Shiny","consumption_va.R"))
source(here("R Shiny","renewable_progress_goals_visualization.R"))
source(here("ggplot2","donut_figures.R"))
source(here("ggplot2", "gen_by_source_type.R"))
source(here("ggplot2", "viz_functions.R"))
source(here("ggplot2", "dashboard_plots.R"))

ui <- dashboardPage(
  dashboardHeader(title = "EO43 Project Dashboard"),
  dashboardSidebar(
    menuItem("Summary",tabName="summary",icon = icon("dashboard")),
    menuItem("Generation",tabName = "generation"),
    menuItem("Energy Efficiency",tabName = "efficiency"),
    menuItem("Energy Equity", tabName = "equity"),
    menuItem("Emissions",tabName = "emissions")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="summary",
              h1("Summary of Overall Progress"),
              fluidRow(
                box(width = 3, plotOutput("renewable_progress_donut")),
                box(width = 3, plotOutput("carbon_free_donut")), 
                # box(width = 3, title = "Energy efficiency goal donut. Data needed."),
                box(width = 3, title = "Energy Storage goal donut. Data needed."),  # Plot still needed
                box(width = 3, title = "Energy Equity goal donut. Data needed.") # Plot still needed# Plot still needed
              ),
              h2("Production"),
              fluidRow(
                box(plotOutput("gen_pie")),
                box(plotOutput("gen_area"))
              ),
              h2("Consumption"),
              fluidRow(
                box(plotOutput("con_pie")),
                box(plotOutput("con_area")),
              ),
              h2("Emissions"),
              fluidRow(
                box(plotOutput("emissions_plot")),
              )
      ),
      tabItem(tabName="generation",
              h1("Generation"),
              h2("Renewable Production"),
              h3("Breakdown"),
              fluidRow(
                box(title = "solar",width = 4),
                box(title = "onshore wind",width = 4),
                box(title = "offshore wind",width = 4)
              ),
              fluidRow(
                box(plotOutput("sw_donut2"))
              ),
              h3("Summary Over Time"),
              fluidRow(
                box(title="time series graph")
              ),
              h3("Solar"),
              fluidRow(
                box(title="solar generation over time",width = 6),
                box(title="map for solar",width = 6),
              ),
              h3("Wind"),
              fluidRow(
                box(title="wind generation over time",width = 6),
                box(title="map for wind",width = 6),
              ),
              h2("Full Data for Genereation"),
              fluidRow(
                box(selectInput(inputId = "gen_download", "Choose the content:",
                                choices = c("VA generation")),
                    downloadButton("download_gen", "Download"))
              ),
              fluidRow(
                box(div(DT::dataTableOutput("gen_table"), style = "font-size: 90%"),width = 12)
              ),
              h1('Energy Storage'),
              h2('Utility-Scale Solar'),
              fluidRow(
                box(title="utility scale over time")
              ),
              h2('Distributed Solar'),
              fluidRow(
                box(title="distributed over time")
              )
      ),
      tabItem(tabName="efficiency",
              h1("Energy Efficiency"),
              fluidRow(
                box(width=6, title = "Dominion Percent Efficiency Investments/Goal Met. Data Needed."),
                box(width=6, title = "APCo Percent Efficiency Investments/Goal Met. Data Needed.")
              ),
              h2("Electricity Consumption"),
              fluidRow(
                box(title = "Timeseries plot of energy usage over time, plot completed, pending insertion")
              ),
              fluidRow(
                box(title = "Timeseries of gap in energy consumption. ")
              ),
              fluidRow(
                box(selectInput(inputId = "con_download", "Choose the content:",
                                choices = c("VA total consumption")),
                    downloadButton("download_con", "Download"))
              ),
              fluidRow(
                box(div(DT::dataTableOutput("con_table")),width = 9)
              )
      ),
      tabItem(tabName = "equity",
              h1("Energy Equity in Virginia"),
              
              fluidRow(
                box(selectInput(inputId = "equity_year","Select year:", choices = c("2019","2018","2017"))),
                box(title = "Map of monthly households expenditures on electricity, in $ and as a % of income, shaded by country"
              )),
              h2("Electricity Expenditures"),
              fluidRow(
                box(title = "Time series of annual expenditures on electricity by income bracket"

                )
              )
              
              
      ),
      tabItem(tabName="emissions",
              h1("VA CO2 Emissions from Electricity Sector"),
              fluidRow(
              ),
              fluidRow(
                box(selectInput(inputId = "electric_emissions_download", "Choose the content:",
                                choices = c("VA CO2 Emissions from Electricity Sector")),
                    downloadButton("download_electric_emissions", "Download"))
              ),
              fluidRow(
                box(div(DT::dataTableOutput("electric_emissions_table")),width = 9)
                
              ))
    ))
  )


server <- function(input,output){
  
  output$renewable_progress_donut <- renderPlot({
    renewable_donut 
  })
  
  output$carbon_free_donut <- renderPlot({
    carbon_free_donut
  })
  
  output$gen_area <- renderPlot({
    va <- melt(eia_elec_gen_va_a, id="year")
    setnames(va, old=c("variable", "value"), new=c("fuel_type","generation"))
    ggplot(va[fuel_type!="total"&year<=2019], aes(fill=fuel_type, x=year, y=generation)) +
      geom_area(aes(fill=fuel_type)) + ylab("generation (GWh)") + 
      xlab(NULL) +
      labs(title ="VA Historical Electricity Generation By Fuel Type",subtitle="2001-2019") 
  })
  
  output$gen_pie <- renderPlot({
    va2019 <- eia_elec_gen_va_a %>%
      gather(key = "group", value = "value", -year) %>%
      filter(year==2019) 
    va2019 <- va2019[,2:3]
    colnames(va2019)<-c("group","value")
    ggplot(va2019, aes(x="", y=value, fill=group)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0)+
      theme_void()+
      labs(title ="VA Current Electric Generation By Fuel Type",subtitle = "Data in 2019") 
  })
  
  output$emissions_plot <- renderPlot({
    series_id = ("EMISS.CO2-TOTV-EC-TO-VA.A")
    virginia_emissions_electric <- get_EIA_series(my_api_key,series_id)
    virginia_emissions_electric <- virginia_emissions_electric[1:18,]
    setnames(virginia_emissions_electric,old="value",new="CO2_emissions")
    ggplot() +
      geom_line(data=virginia_emissions_electric,mapping=aes(x=year, y=CO2_emissions)) + 
      ylab("emissions (million metric tons CO2)") + 
      xlab(NULL) +
      ylim(0,NA)+
      labs(title =paste("VA","Annual CO2 Emissions from Electric Power Industry"),subtitle="2000-2017") 
  })
  
  output$con_area <- renderPlot({
    ggplot(m_con_xtotal, aes(fill=sector, x=year, y=consumption)) +
      geom_area(aes(fill=sector)) + ylab("consumption (Billion Btu)") + 
      xlab(NULL) +
      labs(title ="VA Historical Energy Consumption Estimates",subtitle="by end-use sector,1960-2017") +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$con_pie <- renderPlot({
    ggplot(cpie, aes(x = "", y = prop, fill = group)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = prop), color = "white",size=6)+
      theme_void()+
      labs(title ="VA Current Electric Consumption By Sector",subtitle = "Data in 2017") 
  })
  
  output$renewable_timeline_plot <- renderPlot({
    selected_year <- input$selected_year
    renewable_timeline_df <- filter(renewable_timeline_df, Year == selected_year)
    ggplot(renewable_timeline_df, aes(x = Company, y = percentage_of_energy_from_renewables, fill = Company)) + 
      geom_bar(stat = "identity")+
      labs(title ="Renewable Energy Goals") +
      ylab("Percent of Energy from Renewables")+
      ylim(0,100)
  })
  
  gen_table <- DT::renderDataTable(
    eia_elec_gen_va_a, 
    options = list(pageLength = 19),
    rownames= FALSE
  )
  
  electric_emissions_table <- DT::renderDataTable(
    virginia_emissions_electric,
    options = list(pageLength = 19),
    rownames = FALSE
  )
  
  output$electric_emissions_table <- electric_emissions_table
  
  output$gen_table <- gen_table
  
  electric_emissions_download_input <- reactive({
    switch(input$electric_emissions_download,
           "VA CO2 Emissions Electricity Sector" = electric_emissions_table)
  })
  
  output$download_electric_emissions <- downloadHandler(
    filename = function() {
      paste(input$electric_emissions_download, ".csv", sep= "")
    },
    content = function(file) {
      write.csv(electric_emissions_download_input(), file, row.names = FALSE)
    }
  )
  
  gen_download_Input <- reactive({
    switch(input$gen_download,
           "VA generation" = gen_table)
  })
  
  output$download_gen <- downloadHandler(
    filename = function() {
      paste(input$gen_download, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(gen_download_Input(), file, row.names = FALSE)
    }
  )
  
  output$con_table<-DT::renderDataTable(
    consumption, 
    options = list(pageLength = 20),
    rownames= FALSE
  )
  
  con_download_Input <- reactive({
    switch(input$con_download,
           "VA total consumption" = consumption)
  })
  
  output$download_con <- downloadHandler(
    filename = function() {
      paste(input$con_download, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(con_download_Input(), file, row.names = FALSE)
    }
  )
  output$sw_donut<- renderPlot(sw_capacity_donut)

  
}

shinyApp(ui=ui,server=server)
