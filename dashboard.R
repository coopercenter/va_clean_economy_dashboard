library(here)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr, warn.conflicts = FALSE)
library(DT)

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
                box(width = 4, plotlyOutput("renewable_progress_donut")),
                box(width = 4, plotlyOutput("carbon_free_donut")),
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
              h2("Renewable and Carbon-Free Generation"),
              h3("Overview"),
              fluidRow(
                box(title = "Percentage of Renewable and Carbon-Free Generation",plotOutput("rc_line")),
                box(title = "Breakdown of Carbon-Free Generation by Fuel Type",plotOutput("rc_break_line")),
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
              )
              
              
      ),
      tabItem(tabName="emissions",
              h1("Greenhouse Gas Emissions in Virginia"),
              fluidRow(
                box(plotOutput('all_co2_emissions_plot')),
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
    ))
)

server <- function(input,output){
  
  output$renewable_progress_donut <- renderPlotly({
    renewable_donut_p
  })
  
  output$carbon_free_donut <- renderPlotly({
    carbon_free_donut_p
  })
  
  output$gen_goal1 <- renderPlotly({
    renewable_donut_p
  })
  
  output$gen_goal2 <- renderPlotly({
    carbon_free_donut_p
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
  
  output$all_co2_emissions_plot <- renderPlot({
    co2_emissions_line
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
    va_annual_generation, 
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
    va_annual_consumption, 
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
  output$sw_donut<- renderPlotly(sw_capacity_donut_p)
  
  output$rc_line<- renderPlot(percent_renewable_and_carbon_free_line)
  
  output$rc_break_line<- renderPlot(annual_carbon_free_generation_by_type_line2
)

  
}


shinyApp(ui=ui,server=server)
