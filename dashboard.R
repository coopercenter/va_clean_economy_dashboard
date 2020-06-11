library(here)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr, warn.conflicts = FALSE)
library(DT)

load('dashboard_plots_output.RData')

title <- tags$a(href="https://ceps.coopercenter.org/", 
                tags$img(src="logo.png", height = '40', width = '110'),
                "Virginia Clean Economy Progress")

ui <- tagList(
  dashboardPage(
  dashboardHeader(title = title, titleWidth = 550),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary",tabName="summary",icon = icon("dashboard")),
      menuItem("Generation",tabName = "generation"),
      menuItem("Energy Equity", tabName = "equity"),
      menuItem("Emissions",tabName = "emissions"),
      hr(), 
      menuItem("Energy Efficiency Programs",tabName = "efficiency")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName="summary",
              h1("Summary of Overall Progress"),
              fluidRow(
                box(width = 3, plotlyOutput("renewable_progress_donut")),
                box(width = 3, plotlyOutput("carbon_free_donut")),
              # box(width = 3, title = "Energy efficiency goal donut. Data needed."),
                box(width = 3, title = "Energy Storage goal donut. Data needed."),  # Plot still needed
                box(width = 3, title = "Energy Equity goal donut. Data needed.") # Plot still needed# Plot still needed
              ),
              h2("Production"),
              fluidRow(
                box(plotlyOutput("gen_pie")),
                box(plotOutput("gen_area"))
              ),
              h2("Consumption"),
              fluidRow(
                box(plotlyOutput("con_pie")),
                box(plotOutput("con_area")),
              ),
              h2("Emissions"),
              fluidRow(
                box(plotOutput("electric_emissions_plot")),
              )
      ),
      tabItem(tabName="generation",
              h1("Generation"),
              h2("Goals"),
              fluidRow(
                box(width = 4,plotlyOutput("sw_donut")),
                box(width = 4,plotlyOutput("gen_goal1")),
                box(width = 4,plotlyOutput("gen_goal2"))),
              h2("Progress on Renewable and Carbon-Free Generation"),
              fluidRow(
                box(title = "Percentage of Renewable and Carbon-Free Generation",plotOutput("rc_line"))
              ),
              h2("Breakdown"),
              fluidRow(box(title = "Breakdown of Carbon-Free Generation by Fuel Type",plotOutput("rc_break_line"),align="center")),
              fluidRow(
                box(title="Solar Generation over Time",width = 6,plotOutput("solar_gen")),
                box(title="Wind Generation over Time",width = 6)
              ),
              h2("Full Data for Generation"),
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
                box(title = "Energy Consumption by Sector",plotOutput("con_ts"))
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
              ),
              h2("Residential Solar Distribution"),
              fluidRow(
                box(title= "Interactive map of residential solar distribution, broken down by race, income bracket, and education level")
              )
              
      ),
      tabItem(tabName="emissions",
              h1("Greenhouse Gas Emissions in Virginia"),
              fluidRow(
                box(plotOutput('electric_emissions_plot2')),
                box(plotOutput('co2_emissions_by_fuel')),
                box(plotOutput('emissions_by_compound_plot'))
              ),
              fluidRow(
                box(selectInput(inputId = "electric_emissions_download", "Choose the content:",
                                choices = c("VA CO2 Emissions from Electricity Sector")),
                    downloadButton("download_electric_emissions", "Download"))
              ),
              fluidRow(
                box(div(DT::dataTableOutput("electric_emissions_table")),width = 9)
                
              ))
    ))),
  tags$footer(
    tags$p("Developed by the",
    tags$a(href = "https://ceps.coopercenter.org/", "Weldon Cooper Center for Public Service Center for Economic Policy Studies"), "in conjunction with the Virginia Department of Mines, Minerals, and Energy"),
    align = "center", 
      style = "
      position:relative;
      bottom:0;
      width:100%;
      height:60px;   /* Height of the footer */
      color: white;
      padding: 10px;
      background-color: grey;
      z-index: 1000;")
)

server <- function(input,output){
  
  output$renewable_progress_donut <- renderPlotly({
    single_ring_renewable_donut_p
  })
  
  output$carbon_free_donut <- renderPlotly({
    single_ring_carbon_free_donut_p
  })
  
  output$gen_goal1 <- renderPlotly({
    single_ring_renewable_donut_p
  })
  
  output$gen_goal2 <- renderPlotly({
    single_ring_carbon_free_donut_p
  })
  
  output$gen_area <- renderPlot({
    va_annual_production_area
  })
  
  output$gen_pie <- renderPlotly({
    va_annual_production_2019_pie_chart_p_with_legend
  })
  
  output$electric_emissions_plot <- renderPlot({
    co2_electric_emissions_line
  })
  
  output$electric_emissions_plot2 <- renderPlot({
    co2_electric_emissions_line
  })
  
  output$co2_emissions_by_fuel <- renderPlot({
    carbon_by_fuel_emissions_stacked
  })
  
  output$emissions_by_compound_plot <- renderPlot({
    emissions_line
  })
  
  output$con_area <- renderPlot({
    va_annual_consumption_area
  })
  output$con_ts <- renderPlot({
    va_annual_consumption_area
  })  
  output$con_pie <- renderPlotly({
    va_annual_consumption_2017_pie_chart_p_with_legend
  })
  
  output$renewable_timeline_plot <- renderPlot({
    percent_renewable_and_carbon_free_line
    
  })
  
  gen_table <- DT::renderDataTable(
    va_gen_w_commas, 
    options = list(pageLength = 19),
    rownames= FALSE
  )
  
  electric_emissions_table <- DT::renderDataTable(
    virginia_emissions_electric_commas,
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
    va_con_w_commas, 
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
  output$sw_donut<- renderPlotly(single_ring_sw_capacity_donut_p)
  
  output$rc_line<- renderPlot(percent_renewable_and_carbon_free_line)
  
  output$rc_break_line<- renderPlot(annual_carbon_free_generation_by_type_line2)
  
  output$solar_gen<- renderPlot(solar_generation_time_series_line)
  


  
}


shinyApp(ui=ui,server=server)
