library(here)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr, warn.conflicts = FALSE)
library(DT)
library(sf)

load('dashboard_plot_outputs.RData')

title <- tags$a(
  href = "https://www.dmme.virginia.gov/",
  tags$img(
    src = "DmmeLogo.png",
    height = '30',
    width = '100'
  ),
  "Virginia Clean Economy Progress"
)

dbHeader <-
  dashboardHeader(
    title = title,
    tags$li(
      class = "dropdown",
      tags$style(".main-header {max-height: 60px}"),
      tags$style(
        ".main-header .logo {height: 60px;
        line-height: 55px !important;
        padding: 0 0px;}"
      ),
      tags$style(
        ".main-header .sidebar-toggle {height: 60px;
        line-height: 55px !important;
        padding: 0 20px;}"
      )
      ),
    tags$li(a(
      href = 'https://ceps.coopercenter.org/',
      tags$img(
        src = 'logo.png',
        height = '30',
        width = '115'
      )
    ), class = "dropdown"),
    titleWidth = 500
      )



ui <- tagList(
  dashboardPage(
    title = "Virginia Clean Economy",
    dbHeader,
    dashboardSidebar(
      tags$style(".left-side, .main-sidebar {padding-top: 60px}"),
      sidebarMenu(
        menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
        menuItem("Generation and Capacity", tabName = "generation"),
        menuItem("Energy Equity", tabName = "equity"),
        menuItem("Emissions", tabName = "emissions"),
        hr(),
        menuItem("Energy Efficiency Programs", tabName = "efficiency"),
        hr(),
        menuItem("About Us", tabName = "about")
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        tabItem(
          tabName = "summary",
          h1("Summary of Overall Progress"),
          fluidRow(
            box(width = 4, plotlyOutput("renewable_progress_donut")),
            box(width = 4, plotlyOutput("carbon_free_donut")),
            box(width = 4, plotlyOutput("energy_storage_donut")),
          ),
          h2("Production"),
          fluidRow(box(plotlyOutput("gen_pie")),
                   box(plotlyOutput("gen_area"))),
          h2("Consumption"),
          fluidRow(box(plotlyOutput("con_pie")),
                   box(plotlyOutput("con_area")))
        ),
        tabItem(
          tabName = "generation",
          h1("Generation"),
          h2("Goals"),
          fluidRow(
            box(width = 4, plotlyOutput("sw_donut")),
            box(width = 4, plotlyOutput("gen_goal1")),
            box(width = 4, plotlyOutput("gen_goal2"))
          ),
          h2("Progress on Renewable and Carbon-Free Generation"),
          fluidRow(box(plotlyOutput("rc_line"))),
          h2("Breakdown"),
          fluidRow(box(plotlyOutput("rc_break_line"),
                       align = "center")),
          h3("Solar"),
          fluidRow(box(width = 6,
                       plotlyOutput("solar_gen"))),
          h3("Offshore Wind"),
          fluidRow(box(
            width = 6,
            plotlyOutput("wind_projected_gen")
          ),
          box(
            width = 6,
            plotlyOutput("wind_projected_capacity")
          )),
          fluidRow(box(
            div(DT::dataTableOutput("solar_table"), style = "font-size: 80%"), width =
              12
          )),
          h3("Wind"),
          fluidRow(box(
            div(DT::dataTableOutput("wind_table"), style = "font-size: 80%"), width =
              12
          )),
          
          h3("Full Data for Generation"),
          fluidRow(box(
            selectInput(
              inputId = "gen_download",
              "Content:",
              choices = c("VA generation")
            ),
            downloadButton("download_gen", "Download")
          )),
          fluidRow(box(
            div(DT::dataTableOutput("gen_table"), style = "font-size: 90%"), width = 12
          )),
          h1('Energy Storage'),
          h2('Utility-Scale'),
          fluidRow(box(
            div(DT::dataTableOutput("storage_table"), style = "font-size: 90%"), width = 12
          ))
          
        ),
        tabItem(
          tabName = "efficiency",
          h1("Energy Efficiency"),
          h2("Consumption"),
          fluidRow(box(plotlyOutput("con_per_capita")),
                   box(plotlyOutput("con_per_gdp"))),
          fluidRow(box(
            selectInput(
              inputId = "con_download",
              "Choose the content:",
              choices = c("VA total consumption")
            ),
            downloadButton("download_con", "Download")
          )),
          fluidRow(box(div(
            DT::dataTableOutput("con_table")
          ), width = 9)),
          h2("Emissions"),
          fluidRow(box(plotlyOutput(
            "emissions_per_capita"
          )),
          box(plotlyOutput(
            "emissions_per_gdp"
          ))),
          h2("Annual Savings"),
          fluidRow(box(plotlyOutput("annual_savings_2022_pie_chart")),
                   box(plotlyOutput("annual_savings_2020_pie_chart"))),
          h2("Investment by IOUs"),
          fluidRow(box(
            div(DT::dataTableOutput("investment_table"), style = "font-size: 80%"), width =
              12
          ))

        ),
        
        tabItem(
          tabName = "equity",
          h1("Energy Equity in Virginia"),
          h2("Electricity Expenditures"),
          fluidRow(
            box(
              plotOutput(
            "burden_map_expenditure"
              )
            )
          ),
          fluidRow(box(plotOutput(
            "burden_map_expenditure_2"
          )))
        ),
        tabItem(
          tabName = "emissions",
          h1("Greenhouse Gas Emissions in Virginia"),
          fluidRow(box(
            plotlyOutput('electric_emissions_plot2')
          ),
          box(plotlyOutput(
            'co2_emissions_by_fuel'
          ))),
          fluidRow(box(
            selectInput(
              inputId = "electric_emissions_download",
              "Choose the content:",
              choices = c("VA CO2 Emissions from Electricity Sector")
            ),
            downloadButton("download_electric_emissions", "Download")
          )),
          fluidRow(box(div(
            DT::dataTableOutput("electric_emissions_table")
          ), width = 9))
        ),
        tabItem(tabName = 'about',
                h1("About Us"),
                fluidRow(
                  box(
                    width = 9,
                    h2("Overview"),
                    h4("Summary"),
                    p(
                      "Recent legislation, including the Virginia Governor’s Executive Order 43 (EO-43) “Expanding Access to Clean Energy and Growing the Clean Energy Jobs of the Future” and the Virginia Clean Economy Act, detail goals and pathways to ultimately transition to an entirely clean energy electric grid in Virginia. The Summary section provides an overview of progress towards this objective broadly characterized by generation, capacity, consumption, and emissions data. "
                    ),
                    br(),
                    h4("Generation and Capacity"),
                    p(
                      'The overarching targets for generation in realizing a clean energy electric grid in Virginia include 30% renewable energy generation by 2030 followed by 100% carbon-free energy generation by 2050. There are also specific targets that require certain levels of capacity from renewable sources, including wind and solar, as well as energy storage capacity targets.'
                    ),
                    br(),
                    h4('Energy Equity'),
                    p(
                      'In pursuing a clean energy electric grid, the subject of energy equity must be considered. While transitioning to clean energy is essential, it is also critical that we evaluate how this transition may affect access to or expenses of electricity. Here, we consider energy burdens for households below the federal poverty level in terms of total costs of energy as well as energy costs as a percent of income.'
                    ),
                    br(),
                    h4('Emissions'),
                    p(
                      'An ultimate goal of clean energy is to lower emissions and to eventually achieve net-zero emissions. This goal will be achieved with the integration of carbon free energy generation and negative emissions projects to sequester carbon from the atmosphere. The emissions section tracks emissions in Virginia both by fuel type and by sector to identify where efforts can best be focused to make these necessary reductions.'
                    ),
                    br(),
                    h4('Energy Efficiency'),
                    p(
                      'The purpose of energy efficiency is to reduce the amount of energy necessary to accomplish a certain task or function, thus reducing emissions. The Energy Efficiency section contains trends of consumption per capita and consumption per GDP in Virginia as a measure of energy efficiency. This tab also contains information about ongoing and planned investment by investor-owned public utilities in energy efficiency programs that is as current as 2019.'
                    )
                  )
                ),
                fluidRow(
                  box(
                    width = 9,
                    h2("Credits"),
                    p(
                      "This dashboard was created by a team of researchers from the Weldon Cooper Center for Economic and Policy Studies under the direction of Dr. Arthur Small in collaboration with the Virginia Department of Mines, Minerals, and Energy."
                    ),
                    h3("Team Members"),
                    tags$p("Project Coordinator: Yiyun Zhong"),
                    h4("Dashboard creation, design, and publishing:"),
                    tags$ul(
                      tags$li("Jackson Brandberg"),
                      tags$li("Caleb Neale"),
                      tags$li("Ethan Novak"),
                      tags$li("Yiyun Zhong")
                    ),
                    h4("Data visualization:"),
                    tags$ul(
                      tags$li("Alexis Freitas, Lead"),
                      tags$li("Madeleine Alwine"),
                      tags$li("Neha Awasthi"),
                      tags$li("Lauren Coppins"),
                      tags$li("Aishvarya Pathange")
                    ),
                    h4("Data management and acquisition:"),
                    tags$ul(
                      tags$li("Jackson Brandberg, Lead"),
                      tags$li("Yiyun Zhong, Lead"),
                      tags$li("Chloe Fauvel"),
                      tags$li("Pyung Lee"),
                      tags$li("Mai Luu"),
                      tags$li("Jamison Stevens"),
                      tags$li("Emily Weidenfeller")
                    )
                  )
                )
                )
      )
    )
  ),
  tags$footer(
    tags$p(
      "Developed by the",
      tags$a(
        href = "https://ceps.coopercenter.org/",
        "Weldon Cooper Center for Public Service Center for Economic Policy Studies"
      ),
      "in conjunction with the Virginia Department of Mines, Minerals, and Energy"
    ),
    align = "center",
    style = "
    position:relative;
    bottom:0;
    width:100%;
    height:60px;   /* Height of the footer */
    color: white;
    padding: 10px;
    background-color: grey;
    z-index: 1000;"
  )
  )

server <- function(input, output) {
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
  
  output$gen_area <- renderPlotly({
    va_annual_production_area_p
  })
  
  output$gen_pie <- renderPlotly({
    va_annual_production_2019_pie_chart_p_with_legend
  })
  
  output$electric_emissions_plot <- renderPlotly({
    co2_combined_emissions_line_p
  })
  
  output$electric_emissions_plot2 <- renderPlotly({
    co2_combined_emissions_line_p
  })
  
  output$co2_emissions_by_fuel <- renderPlotly({
    carbon_by_fuel_emissions_stacked_p
  })
  
  output$con_area <- renderPlotly({
    va_annual_consumption_area_p
  })
  output$con_ts <- renderPlotly({
    va_annual_consumption_area_p
  })
  output$con_pie <- renderPlotly({
    va_annual_consumption_2018_pie_chart_p_with_legend
  })
  
  output$renewable_timeline_plot <- renderPlotly({
    percent_renewable_and_carbon_free_line_p
    
  })
  
  gen_table <- DT::renderDataTable(va_gen_w_commas,
                                   options = list(pageLength = 19),
                                   rownames = FALSE)
  
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
      paste(input$electric_emissions_download, ".csv", sep = "")
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
  
  output$con_table <- DT::renderDataTable(va_con_w_commas,
                                          options = list(pageLength = 20),
                                          rownames = FALSE)
  
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
  output$sw_donut <- renderPlotly(single_ring_sw_capacity_donut_p)
  
  output$rc_line <-
    renderPlotly(percent_renewable_and_carbon_free_line_p)
  
  output$rc_break_line <-
    renderPlotly(annual_carbon_free_generation_by_type_line_p)
  
  output$solar_gen <-
    renderPlotly(solar_generation_time_series_line_p)
  
  output$wind_projected_gen <-
    renderPlotly(wind_projected_generation_time_series_line_p)
  
  output$wind_projected_capacity <-
    renderPlotly(wind_projected_capacity_line_p)
  
  output$burden_map_expenditure <-
    renderPlot(va_avg_annual_energy_cost,
               width = 600,
               height = 400)
  
  output$burden_map_expenditure_2 <-
    renderPlot(va_avg_annual_energy_percent_exp,
               width = 600,
               height = 400)
  
  output$solar_table <- DT::renderDataTable(pjm_solar,
                                            options = list(pageLength = 20),
                                            rownames = FALSE)
  
  output$investment_table <- DT::renderDataTable(investment_by_IOUs,
                                            options = list(pageLength = 20),
                                            rownames = FALSE)
  
  
  output$storage_table <- DT::renderDataTable(pjm_storage,
                                              options = list(pageLength = 20),
                                              rownames = FALSE)
  
  output$energy_storage_donut <-
    renderPlotly(single_ring_storage_capacity_donut_p)
  
  output$con_per_capita <-
    renderPlotly(consumption_per_capita_line_p)
  
  
  
  output$con_per_gdp <- renderPlotly(consumption_per_gdp_line_p)
  
  output$emissions_per_capita <-
    renderPlotly(emissions_per_capita_line_p)
  
  output$emissions_per_gdp <- renderPlotly(emissions_per_gdp_line_p)
  
  output$wind_table <- DT::renderDataTable(pjm_wind,
                                           options = list(pageLength = 20),
                                           rownames = FALSE)
  
  output$annual_savings_2022_pie_chart <- renderPlotly(annual_savings_2022_pie_chart_p)
  
  output$annual_savings_2020_pie_chart <- renderPlotly(annual_savings_2020_pie_chart_p)
  
}


shinyApp(ui = ui, server = server)
