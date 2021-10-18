#library(groundhog)
#groundhog.day = "2021-09-01"
#pkgs = c("here", "ggplot2", "shiny", 'shinydashboard', "plotly", "dplyr", "DT", "sf")
#groundhog.library(pkgs, groundhog.day)

library(here)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(sf)

load('dashboard_output.RData')

title <- tags$a(
  href = "https://www.energy.virginia.gov/index.shtml",
  tags$img(
    src = "DE_logo_white_and_clear.png",
    height = '45',
    #width = '150'
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
        src = 'CCPS-Logo_Horiz_White.png',
        height = '30',
        width = '200'
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
        menuItem("Energy Efficiency", tabName = "efficiency"),
        #menuItem("Energy Equity", tabName = "equity"),
        menuItem("Emissions", tabName = "emissions"),
        hr(),
        menuItem("About", tabName = "about"),
        menuItem("Downloadable Data Tables", tabName = "tables"),
        menuItem("Credits", tabName = "credits")
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ), 
      tabItems(
        tabItem(
          tabName = "summary",
          h1("Virginia's Progress Towards a Cleaner Electric Grid"),
          h4("This dashboard includes interactive elements. Move your mouse around to discover them. For more information, click on the 'About' tab."),
          h2("Goals"),
          fluidRow(
            box(width = 4, plotlyOutput("renewable_progress_donut"), align = "center"),
            box(width = 4, plotlyOutput("carbon_free_donut"), align = "center"),
            box(width = 4, plotlyOutput("energy_storage_donut"), align = "center"),
          ),
          h2("Production by Fuel Type"),
          fluidRow(box(plotlyOutput("gen_pie")),
                   box(plotlyOutput("gen_area"))),
          h2("Consumption by Sector"),
          fluidRow(box(plotlyOutput("con_pie")),
                   box(plotlyOutput("con_area")))
        ),
        tabItem(
          tabName = "generation",
          h1("Electricity Capacity and Generation"),
          h2("Goals"),
          fluidRow(
            box(width = 4, plotlyOutput("sw_donut"), align = "center"),
            box(width = 4, plotlyOutput("gen_goal1"), align = "center"),
            box(width = 4, plotlyOutput("offshore_wind_progress"), align = "center")
          ),
          h2("Progress on Renewable and Carbon-Free Generation"),
          h4("The renewable portfolio standard (RPS) is a mandate from the Virginia Clean Economy Act that requires Dominion Power and APCO to have a percentage of its energy production from the existing definition of 'renewable energy' as defined in Va. Code 56-576. More information in the 'About Tab'."),
          fluidRow(box(
            width = 6,
            plotlyOutput("renewable_timeline_plot")
          ),
          box(
            width = 6,
            plotlyOutput("rps_renewable_line_p")
          )),
          
          h3("Virginia's Net Electricity Imports"),
          fluidRow(box(
            width = 6,
            plotlyOutput("va_elec_net_imports_line_p")
          )),
          
          
          h2("Breakdown of Carbon Free Generation by Source"),
          fluidRow(box(plotlyOutput("rc_break_line"),align = "center")),
          h3("Virginia Solar Electric Generation: Utility Scale and Distributed"),
          fluidRow(box(width = 6, plotlyOutput("solar_gen"))),
          h3("Offshore Wind"),
          fluidRow(box(
            width = 6,
            plotlyOutput("wind_projected_gen")
          ),
          box(
            width = 6,
            plotlyOutput("wind_projected_capacity") )
          ),
          h3("Carbon-Free Generation Progress"),
          fluidRow(box(
            width = 6,
            plotlyOutput("gen_goal2") 
          ), align = "center")
          
          
        ),
        tabItem(
          tabName = "efficiency",
          h1("Energy Efficiency"),
          h2("Energy Efficiency Mandates for Appalachian Power and Dominion Energy"),
          fluidRow(box(plotlyOutput("apco_dom_historic_goals"))),
          
          
          h2("Projected Future Savings From Energy Efficiency Programs"), 
          fluidRow(box(plotlyOutput("annual_savings_2020_2022"), width = "100%")),
          
          h2("Consumption"),
          fluidRow(box(plotlyOutput("con_per_capita")),
                   box(plotlyOutput("con_per_gdp")))
        ),
        
        # tabItem(
        #   tabName = "equity",
        #   h1("Energy Equity in Virginia"),
        #   h2("Average Annual Energy Cost for All Electricity"),
        #   fluidRow(
        #     box(plotlyOutput("burden_map_expenditure"), width = 7)
        #     ,
        #     box(plotlyOutput("dollar_reference_figure" ), width = 5)
        #     ),
        #   fluidRow(
        #     box(plotlyOutput("burden_map_expenditure_2"), width = 7)
        #     ,
        #     box(plotlyOutput("percent_income_reference_figure"), width = 5)
        #     )
        # ),
        
        
        tabItem(
          tabName = "emissions",
          h1("Virginia Greenhouse Gas Emissions From Power Production"),
          fluidRow(box(
            plotlyOutput('electric_emissions_plot2')
          ),
          box(plotlyOutput(
            'co2_emissions_by_fuel'
          ))),
          br(),
          h2("Emissions"), 
          fluidRow(box(
            plotlyOutput('emissions_per_capita') 
          ),
          box(plotlyOutput(
            'emissions_per_gdp'
          )))
          
        ),
        
        tabItem(
          tabName = "tables",
          h1("Datasets"),
          
          
          fluidRow(box(
            selectInput(
              "check", "Choose a dataset:",
              choices = c("Solar Generation", "Wind Generation", "Generation By Source", "Utility-Scale Storage","Electricy Sector Emissions", "Consumption by Sector","Programs by Investor Owned Utilities")
            ),
            downloadButton("downloadData", "Download") 
          )),
          
          
          h2("Solar Generation"),
          fluidRow(box(
            div(DT::dataTableOutput("solar_table"), style = "font-size: 80%"), width =
              12
          )),
          
          
          h2("Wind Generation"),
          fluidRow(box(
            div(DT::dataTableOutput("wind_table"), style = "font-size: 80%"), width =
              12
          )),
          
          h2("Generation By Source"),
          fluidRow(box(
            div(DT::dataTableOutput("gen_table"), style = "font-size: 90%"), width = 12
          )),
          
          
          h2('Utility-Scale Storage'),
          fluidRow(box(
            div(DT::dataTableOutput("storage_table"), style = "font-size: 90%"), width = 12
          )),
          
          h2("Electricy Sector Emissions"),
          fluidRow(box(div(
            DT::dataTableOutput("electric_emissions_table")
          ), width = 9)),
          
          
          h2("Consumption by Sector"),
          fluidRow(box(div(
            DT::dataTableOutput("con_table")
          ), width = 9)),
          
          
          h2("Programs by Investor Owned Utilities"),
          fluidRow(box(
            div(DT::dataTableOutput("investment_table"), style = "font-size: 80%"), width =
              12
          ))
        ),
        
        tabItem(tabName = 'about',
                h1("Dashboard Overview and Background"),
                fluidRow(
                  box(
                    width = 9,
                    p(
                      "Recent legislation including the Virginia Clean Economy Act details steps to transition Virginia's electric grid to 100% carbon-free generation by 2050, while improving energy equity. The legislation includes mandates to increase generation from renewable energy, improve energy efficiency, reduce carbon emissions, and increase access to affordable energy to lower-income communities and communities of color. The legislation imposes mandates on investor-owned power utilities, state agencies, and others to achieve specific quantitative targets on specific schedules."
                    ),
                    p(
                      "This dashboard is designed to enable state policy makers, other stakeholders, and the general public to track Virginia's progress towards the realization of these clean energy goals. For each of several quantitative measures, the dashboard displays data on current progress, in the context of historic experience and legislated future targets. The dashboard is designed to be updated as new data become available. It is hoped that the dashboard will assist all stakeholders by providing accountability towards realization of Virginia's clean economy goals."
                    ),
                    p(
                      "All plots in this dashboard are interactive. As the mouse hovers over the plots additional information describing specific data points will appear.
                      If the plots have legends, click on the individual legend items to turn their visibility on the plot on or off. This is true for all plots with legends, including line plots, bar plots, and stacked plots. 
                       For all plots exlcuding the circle plots, left click and drag across the interactive plot to 'zoom in' on the plot. This allows the user to examine specific portions of the graph. Double click on the plot to return to normal scaling."
                    ),
                    br(),
                    h4("Summary"),
                    p(
                      "The Summary section provides an overview of Virginia's progress towards realization of its clean economy targets, broadly characterized by generation, capacity, consumption, and emissions data."
                    ),
                    br(),
                    h4("Generation and Capacity"),
                    p(
                      "Targets include 30% renewable energy generation by 2030 followed by 100% carbon-free energy generation by 2050. To realize these targets, mandates are imposed on investor-owned utilities that require building specified levels of renewable generation capacity for on-shore wind and solar, and off-shore wind, as well as energy storage capacity. 'Other biomass' includes agricultural byproducts, landfill gas, and biogenic municipal solid waste. 'Other energy sources' includes nonbiogenetic municpial solid waste, batteries, chemicals, hydrogen, and pump-hydro storage. The definition of 'renewables' as defined in the VCEA follows the Va. Code § 56-576 definition of renewables but explicitly excludes 'waste heat from fossil-fired facilities' and 'electricity generated from pumped storage'. 
                      ", br(),
                      "The renewable portfolio standard follows the definition of 'renewable energy' defined in the Va. Code § 56-576, but explicitly excludes 'waste heat from fossil-fired facilities' and 'electricity generated from pumped storage'. This excludes nuclear energy."
                    ),
                    br(),
                    # h4('Energy Equity'),
                    # p(
                    #   'Figures in this tab display information about energy burdens for households in Virginia. Data are displayed in terms of total costs of energy as well as energy costs as a percent of income.'
                    # ),
                    # br(),
                    h4('Emissions'),
                    p(
                      'This section tracks emissions in Virginia both by fuel type and by sector.'
                    ),
                    br(),
                    h4('Energy Efficiency'),
                    p('The Virgina Clean Economy Act imposes energy efficiency targets on both Dominion Power and Appalachian Power Company. These targets are expressed as required percentage reductions in total retail sales, from a 2019 baseline. Dominion Power must reduce sales by 5% by 2025. Appalachian Power must reduce sales by 2% by 2025. For context, the Energy Efficiency section also displays trends on consumption per capita and consumption per GDP in Virginia. Data tables at bottom contain information about ongoing and planned investment by investor-owned public utilities in energy efficiency programs.' 
                    ),
                    p("Acronyms include:", br(), "APCO: Appalachian Power Company" ,br(), "C-PACE: Commercial Property Assessed Clean Energy ",br(),"VE:Virginia Energy",br(),"IECC: International Energy Conservation Code",br(),"ESPCs: Energy Savings Performance Costs ",br(),"MUSH: Municipalities, Universities, Schools, and Hospitals"),
                    
                    
                    
                    br(),
                    h4('Links'),
                    p(
                      tags$a(
                        href = "https://www.energy.virginia.gov/renewable-energy/documents/VCEASummary.pdf",
                        "VCEA Reference Summary"
                      ),
                      
                      br(),
                      tags$a(
                        href = "https://www.energy.virginia.gov/index.shtml",
                        "Virginia Department of Energy"
                      ),
                      br(),
                      tags$a(
                        href = "https://ceps.coopercenter.org/",
                        "Weldon Cooper Center for Public Service, University of Virginia"
                      )
                    )
                    
                  )
                )),
        
        tabItem(tabName = 'credits',
                h1("Credits"),
                fluidRow(
                  box(
                    width = 9,
                    p(
                      "This dashboard was created by a team of researchers from the Weldon Cooper Center for Public Service at the University of Virginia under the direction of Dr. Arthur Small in collaboration with the Virginia Department of Energy."
                    ),
                    h3("Team Members"),
                    tags$p("Project Coordinators: Arthur Small, Yiyun Zhong"),
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
        "Weldon Cooper Center for Public Service, Center for Economic Policy Studies"
      ),
      "in conjunction with the Virginia Department of Energy"
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
  
  output$sw_donut <- renderPlotly(single_ring_sw_capacity_donut_p)
  
  output$rc_line <-
    renderPlotly(percent_renewable_and_schedule_goal_combined_line_p)
  
  output$cf_line <- 
    renderPlotly(percent_carbon_free_line_p)
  
  output$rc_break_line <-
    renderPlotly(annual_carbon_free_generation_by_type_line_p)
  
  output$solar_gen <-
    renderPlotly(solar_generation_time_series_line_p)
  
  output$wind_projected_gen <-
    renderPlotly(wind_projected_generation_time_series_line_p)
  
  output$wind_projected_capacity <-
    renderPlotly(wind_projected_capacity_line_p)
  
  output$offshore_wind_progress <-
    renderPlotly(single_ring_offshore_wind_capacity_donut_p)
  
  output$burden_map_expenditure <-
    renderPlotly(va_avg_annual_energy_cost_p)
  
  output$burden_map_expenditure_2 <-
    renderPlotly(va_avg_annual_energy_percent_exp_p)
  
  
  output$energy_storage_donut <-
    renderPlotly(single_ring_storage_capacity_donut_p)
  
  output$con_per_capita <-
    renderPlotly(consumption_per_capita_line_p)
  
  
  output$con_per_gdp <- renderPlotly(consumption_per_gdp_line_p)
  
  output$emissions_per_capita <-
    renderPlotly(emissions_per_capita_line_p)
  
  output$emissions_per_gdp <- renderPlotly(emissions_per_gdp_line_p)
  
  
  output$annual_savings_2020_2022 <- renderPlotly(annual_savings_2020_2022_stacked_bar_chart_p)
  
  output$apco_dom_historic_goals <- renderPlotly(apco_dom_historic_goal_sales_combined_line_p)
  
  output$dollar_reference_figure <- renderPlotly(dollar_reference_figure_p)
  
  output$percent_income_reference_figure <- renderPlotly(percent_income_reference_figure_p)
  
  output$va_elec_net_imports_line_p <- renderPlotly(va_elec_net_imports_line_p)
  
  ## New edits 
  output$rps_renewable_line_p <- renderPlotly(rps_renewable_line_p)
  
  
  
  
  output$solar_table <- DT::renderDataTable(pjm_solar,
                                            options = list(pageLength = 20),
                                            rownames = FALSE)
  
  
  output$wind_table <- DT::renderDataTable(pjm_wind,
                                           options = list(pageLength = 20),
                                           rownames = FALSE)
  
  output$gen_table <- DT::renderDataTable(va_gen_w_commas,
                                          options = list(pageLength = 19),
                                          rownames = FALSE)
  
  output$storage_table <- DT::renderDataTable(pjm_storage,
                                              options = list(pageLength = 20),
                                              rownames = FALSE)
  
  
  output$electric_emissions_table <- electric_emissions_table <- DT::renderDataTable(
    virginia_emissions_electric_commas,
    options = list(pageLength = 19),
    rownames = FALSE)
  output$con_table <- DT::renderDataTable(va_con_w_commas,
                                          options = list(pageLength = 20),
                                          rownames = FALSE)
  
  output$investment_table <- DT::renderDataTable(investment_by_IOUs,
                                                 options = list(pageLength = 20),
                                                 rownames = FALSE)
  datasetInput <- reactive({
    switch(input$check,
           "Solar Generation" = pjm_solar,
           "Wind Generation" = pjm_wind,
           "Generation By Source" = va_gen_w_commas,
           "Utility-Scale Storage" = pjm_storage,
           "Electricy Sector Emissions" = virginia_emissions_electric_commas,
           "Consumption by Sector" = va_con_w_commas,
           "Programs by Investor Owned Utilities" = investment_by_IOUs)
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$check, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file,  row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)