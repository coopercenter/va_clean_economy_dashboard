#Load necessary packages
pkgs = c("here", "ggplot2", "shiny", 'shinydashboard', "plotly", "dplyr", "DT", "sf")
test <- suppressMessages(lapply(pkgs, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
rm(test,pkgs)

#Read in the plots and plot data for interactive graphs
load('dashboard_output.RData')

title <- tags$a(
  href = "https://www.energy.virginia.gov/index.shtml",
  tags$img(
    src = "DE_logo_white_and_clear_spaced.png",
    height = '45',
    width = '180'
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
        #menuItem("Downloadable Data Tables", tabName = "tables"),
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
          h4("This dashboard includes interactive elements. Move your mouse around to discover them. For visualizations with legends, click a legend item once to remove it or double click to isolate it. Double click on the legend to restore all items. For more information, click on the 'About' tab."),
          h2("Goals"),
          fluidRow(
            box(width = 4, plotlyOutput("single_ring_renewable_donut_p"), align = "center"),
            box(width = 4, plotlyOutput("single_ring_carbon_free_donut_p"), align = "center"),
            box(width = 4, plotlyOutput("single_ring_storage_capacity_donut_p"), align = "center"),
          ),
          h2("Electricity Production by Fuel Type"),
          fluidRow(box(plotlyOutput("va_annual_production_pie_chart_p_with_legend")),
                   box(plotlyOutput("va_annual_production_area_p"))),
          h2("Energy Consumption by Sector"),
          fluidRow(box(plotlyOutput("va_annual_consumption_pie_chart_p_with_legend")),
                   box(plotlyOutput("va_annual_consumption_area_p")))
        ),
        tabItem(
          tabName = "generation",
          h1("Electricity Capacity and Generation"),
          h2("Goals"),
          fluidRow(
            box(width = 4, plotlyOutput("single_ring_sw_capacity_donut_p"), align = "center"),
            box(width = 4, plotlyOutput("single_ring_renewable_donut_p"), align = "center"),
            box(width = 4, plotlyOutput("single_ring_offshore_wind_capacity_donut_p"), align = "center")
          ),
          h2("Progress on Renewable and Carbon-Free Generation"),
          h4("The renewable portfolio standard (RPS) is a mandate from the Virginia Clean Economy Act that requires Dominion Power and APCO to have a percentage of its energy production from the existing definition of 'renewable energy' as defined in Va. Code 56-576. More information in the 'About Tab'."),
          fluidRow(box(
            width = 6,
            plotlyOutput("percent_renewable_and_carbon_free_line_p")
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
          fluidRow(box(plotlyOutput("annual_carbon_free_generation_by_type_line_p"),align = "center")),
          h3("Virginia Solar Electric Generation: Utility Scale and Distributed"),
          fluidRow(box(width = 6, plotlyOutput("solar_generation_time_series_line_p"))),
          h3("Offshore Wind"),
          fluidRow(box(
            width = 6,
            plotlyOutput("wind_projected_generation_time_series_line_p")
          ),
          box(
            width = 6,
            plotlyOutput("wind_projected_capacity_line_p") )
          ),
          h3("Carbon-Free Generation Progress"),
          fluidRow(box(
            width = 6,
            plotlyOutput("single_ring_carbon_free_donut_p") 
          ), align = "center")
          
          
        ),
        tabItem(
          tabName = "efficiency",
          h1("Energy Efficiency"),
          #adding in new tabs for Lead By Example and the Energy Efficiency Mandates
          tabBox(height = '800px',width='300px',
                 id='energy efficiency tabs',
                 tabPanel('Lead By Example', style='background: #F0F0F0',
                          h2('Lead by Example'),
                          h4(tags$div("This page highlights the facility tracking goals specified in",tags$a(href="https://law.lis.virginia.gov/vacode/title2.2/chapter6/section2.2-604.2/",
                                                                                                             "§2.2-604.2"),", part of Virginia Energy's",
                                      tags$a(href="https://energy.virginia.gov/renewable-energy/Documents/CEVWebinar2021/LeadByExample2021.pdf",
                                             "Lead By Example Program,"),"and explores the energy use of those facilities as they are added to the database. 
                                      These visualizations will be updated as additional facility information is gathered.")),
                          #plot the building tracking progress compared to the mandated goals, by category
                         h3("Facility Tracking by Agency Category"),
                         h4("See how different categories of state agencies are meeting the building tracking targets"),
                          fluidRow(box(width=10,plotlyOutput('agency_category_progress_plot'))),
                          h4('Select a category to see how many facilities each state agency oversees and how many are currently in the database'),
                          fluidRow(box(width=3,
                                       #select a category of building owners to see how progress is going
                                       selectInput('agency_drilldown',
                                                   label=h5(''),
                                                   choices=c('Administration',
                                                             'Agriculture and Forestry',
                                                             'Commerce and Trade',
                                                             'Culture',
                                                             'Education',
                                                             'Health and Human Services',
                                                             'Natural Resources',
                                                             'Public Safety and Homeland Security',
                                                             'Transportation',
                                                             'Veterans and Defense Affairs'))),
                                   #plot the more detailed view of the building tracking progress by who is responsible for the buildings
                                   box(width=10,height="475px",plotlyOutput('buildings_by_category'))
                                   
                          ),
                         h2('Annual Energy Use'),
                         h3('Tracking annual energy use for facilities recorded in the Virginia Energy database'),
                         h4('Measured in Kilowatt Hours per Square Foot to better understand how facilities of different sizes are using energy, and how their energy savings compare to each other'),
                         #plot the annual bar chart broken down by square foot range
                         fluidRow(box(width=8,plotlyOutput('yearly_values_by_size'))),
                         #plot the detailed annual kWh by building use for a chosen square foot range
                         h4('Select a size to see how the facilities are used and how their energy use is changing over time'),
                         h5("Dot size reflects the number of facilities in a category"),
                         fluidRow(
                           box(width=5,                                       #chose a square foot range for a more detailed view of who is using energy
                               selectInput('square_foot_range',
                                           label=h5(''),
                                           choices=c('5,000 - 50,000 square feet',
                                                     '50,001 - 100,000 square feet',
                                                     '100,001 - 250,000 square feet',
                                                     '250,001 - 500,000 square feet',
                                                     '500,001 - 990,000 square feet'))),
                           box(width=8,height='700px',plotlyOutput('kwh_by_use_by_sqft')))
                 ),
                 tabPanel('Energy Efficiency Mandates',style='background: #F0F0F0',
                          h2("Energy Efficiency Spending"),
                          h4("Tracking the progress of energy companies towards their energy efficiency program spending requirements, and anticipated energy savings compared to required annual savings"),
                          h4('Requirement Definitions:'),
                          htmlOutput('ee_requirement_definitions'),
                          fluidRow(box(width=6,plotlyOutput('apco_ee_spending')),
                                   box(width=6,plotlyOutput('apco_mandates_and_progress')),
                                   box(width=6,plotlyOutput('dominion_ee_spending')),
                                   box(width=6,plotlyOutput('dominion_mandates_and_progress')),
                                   box(width=6,plotlyOutput('odp_ee_spending')),
                                   box(width=6,plotlyOutput('odp_mandates_and_progress'))))
          )
          #h1("Energy Efficiency"),
          #h2("Energy Efficiency Mandates for Appalachian Power and Dominion Energy"),
          #fluidRow(box(plotlyOutput("apco_dom_historic_goals"))),
          
          
          #h2("Projected Future Savings From Energy Efficiency Programs"), 
          #fluidRow(box(plotlyOutput("annual_savings_2020_2022"), width = "100%")),
          
          #h2("Consumption"),
          #fluidRow(box(plotlyOutput("con_per_capita")),
                   #box(plotlyOutput("con_per_gdp")))
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
            plotlyOutput('co2_combined_emissions_line_p')
          ),
          box(plotlyOutput(
            'carbon_by_fuel_emissions_stacked_p'
          ))),
          br(),
          h2("Emissions"), 
          fluidRow(box(
            plotlyOutput('emissions_per_capita_line_p') 
          ),
          box(plotlyOutput(
            'emissions_per_gdp_line_p'
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
          
          
          h2("Energy Consumption by Sector"),
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
                    p('The Virgina Clean Economy Act imposes energy efficiency targets on both Dominion Power and Appalachian Power Company. These targets are expressed as required percentage reductions in total retail sales, from a 2019 baseline. Dominion Power must reduce sales by 5% by 2025. Appalachian Power must reduce sales by 2% by 2025' 
                    ),
                    p("Acronyms found in the VCEA summary include:", br(), "APCO: Appalachian Power Company" ,br(), "C-PACE: Commercial Property Assessed Clean Energy ",br(),"DMME:Department of Mines, Minerals, and Energy (now called Virginia Energy)",br(),"IECC: International Energy Conservation Code",br(),"ESPCs: Energy Savings Performance Costs ",br(),"MUSH: Municipalities, Universities, Schools, and Hospitals"),
                    
                    
                    
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
  output$single_ring_renewable_donut_p <- renderPlotly({
    single_ring_renewable_donut_p
  })
  
  output$single_ring_carbon_free_donut_p <- renderPlotly({
    single_ring_carbon_free_donut_p
  })
  
  #output$single_ring_renewable_donut_p <- renderPlotly({
   # single_ring_renewable_donut_p
  #})
  
  #output$single_ring_carbon_free_donut_p <- renderPlotly({
   # single_ring_carbon_free_donut_p
  #})
  
  output$va_annual_production_area_p <- renderPlotly({
    va_annual_production_area_p
  })
  
  output$va_annual_production_pie_chart_p_with_legend <- renderPlotly({
    va_annual_production_pie_chart_p_with_legend
  })
  
  #output$electric_emissions_plot <- renderPlotly({
  # co2_combined_emissions_line_p
  #})
  
  output$co2_combined_emissions_line_p <- renderPlotly({
    co2_combined_emissions_line_p
  })
  
  output$carbon_by_fuel_emissions_stacked_p <- renderPlotly({
    carbon_by_fuel_emissions_stacked_p
  })
  
  output$va_annual_consumption_area_p <- renderPlotly({
    va_annual_consumption_area_p
  })
  #output$con_ts <- renderPlotly({
  #va_annual_consumption_area_p
  #})
  
  output$va_annual_consumption_pie_chart_p_with_legend <- renderPlotly({
    va_annual_consumption_pie_chart_p_with_legend
  })
  
  output$percent_renewable_and_carbon_free_line_p <- renderPlotly({
    percent_renewable_and_carbon_free_line_p
    
  })
  
  output$single_ring_sw_capacity_donut_p <- renderPlotly(single_ring_sw_capacity_donut_p)
  
  #output$rc_line <-
  # renderPlotly(percent_renewable_and_schedule_goal_combined_line_p)
  
  #output$cf_line <- 
  #renderPlotly(percent_carbon_free_line_p)
  
  output$annual_carbon_free_generation_by_type_line_p <-
    renderPlotly(annual_carbon_free_generation_by_type_line_p)
  
  output$solar_generation_time_series_line_p <-
    renderPlotly(solar_generation_time_series_line_p)
  
  output$wind_projected_generation_time_series_line_p <-
    renderPlotly(wind_projected_generation_time_series_line_p)
  
  output$wind_projected_capacity_line_p <-
    renderPlotly(wind_projected_capacity_line_p)
  
  output$single_ring_offshore_wind_capacity_donut_p <-
    renderPlotly(single_ring_offshore_wind_capacity_donut_p)
  
  #output$burden_map_expenditure <-
  #renderPlotly(va_avg_annual_energy_cost_p)
  
  #output$burden_map_expenditure_2 <-
  # renderPlotly(va_avg_annual_energy_percent_exp_p)
  
  
  output$single_ring_storage_capacity_donut_p <-
    renderPlotly(single_ring_storage_capacity_donut_p)
  
  #output$con_per_capita <-
  #renderPlotly(consumption_per_capita_line_p)
  
  
  #output$con_per_gdp <- renderPlotly(consumption_per_gdp_line_p)
  
  output$emissions_per_capita_line_p <-
    renderPlotly(emissions_per_capita_line_p)
  
  output$emissions_per_gdp_line_p <- renderPlotly(emissions_per_gdp_line_p)
  
  
  #output$annual_savings_2020_2022 <- renderPlotly(annual_savings_2020_2022_stacked_bar_chart_p)
  
  #output$apco_dom_historic_goals <- renderPlotly(apco_dom_historic_goal_sales_combined_line_p)
  
  #output$dollar_reference_figure <- renderPlotly(dollar_reference_figure_p)
  
  #output$percent_income_reference_figure <- renderPlotly(percent_income_reference_figure_p)
  
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
  
  #revamped energy efficiency tab
  
  #html outut for the energy efficiency program definitions
  output$ee_requirement_definitions <- renderUI({
    HTML("<b> Total EE program costs:</b> Provide for the submission of (a) petition(s) for approval of Energy Efficiency programs with projected costs (for design, implementation, and operation) of min. $870M between July 1 2018 - July 1 2028.
         <br> <b>15% carve out:</b> At least 15 percent of proposed costs shall be allocated to programs designed to benefit low-income, elderly, or disabled individuals or veterans.
         <br> <b>HB2789:</b> Submit a petition for approval for a 3-yr EE program for LI, elderly, disabled individuals or veterans costing up to $25M (shall be deemed part of the $870M requirement)")
  })
  
  #plot the overall building tracking and goals
  output$agency_category_progress_plot <- renderPlotly({
    ggplotly(agency_category_progress_plot,tooltip='text')
  })
  
  #get the reactive building tracking by category going
  agency_category <- reactive({switch(input$agency_drilldown,
                                      'Culture'=culture,
                                      'Health and Human Services'=health_and_human_svs,
                                      'Transportation'=transportation,
                                      'Natural Resources'=natural_resources,
                                      'Agriculture and Forestry'=agriculture_and_forestry,
                                      'Education'=education,
                                      'Administration'=administration,
                                      'Public Safety and Homeland Security'=public_safety_and_homeland_security,
                                      'Commerce and Trade'=commerce_and_trade,
                                      'Veterans and Defense Affairs'=veterans_and_defense_affairs)})
  output$buildings_by_category <- renderPlotly({
    data <- agency_category()
    plot_ly(data, x=~facilities_over_5000_sqft,y=~agency_name,
            type='bar', orientation='h',
            name='Facilities Over 5,000 Square Feet',color=as.factor('Facilities Over 5,000 Sqare Feet'),
            colors=theme_colors,
            height="500px") %>%
      add_trace(data,x=~facilities_over_5000_sqft_tracked,y=~agency_name,type='bar',
                orientation='h',
                name='Facilities Over 5,000 Square Feet In Database',color=as.factor('Facilities Over 5,000 Square Feet In Database'),
                colors=theme_colors,
                height="500px") %>%
      layout(xaxis=list(title="Number of Facilities",tickangle=-0),yaxis=list(title=""),
             title='Tracking Progress by Agency',
             paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0")
  })
  
  #plot the annual kwh/square foot bar chart
  output$yearly_values_by_size <- renderPlotly({
    yearly_values_by_size
  })
  
  #create the reactive building use by size range inputs
  size_range <- reactive({switch(input$square_foot_range,
                                 '5,000 - 50,000 square feet'=size_1_use,
                                 '50,001 - 100,000 square feet'=size_2_use,
                                 '100,001 - 250,000 square feet'=size_3_use,
                                 '250,001 - 500,000 square feet'=size_4_use,
                                 '500,001 - 990,000 square feet'=size_5_use)})
  
  #plot the reactive kwh/use/size graphs
  output$kwh_by_use_by_sqft <- renderPlotly({
    data <- size_range()
    plot_ly(data, x = ~year, y = ~kWh/sqft, type = 'scatter',
            mode = 'markers', size = ~buildings, 
            color = ~primaryUse.primaryUseInfo, 
            colors = theme_colors,
            text= ~paste("Building Use: ",primaryUse.primaryUseInfo,
                         "<br>Kilowatt Hours per Square Foot: ",round(kWh/sqft,digits=2),
                         "<br> Cost per Square Foot: $",round(cost/sqft,digits=2),
                         "<br> Cost per Kilowatt Hour: $", round(cost/kWh,digits=2),
                         "<br> Number of Facilities: ",buildings,
                         "<br> Annual Kilowatt Hour Savings Per Square Foot: ",round(savings/kWh,digits=2)),
            hoverinfo="text",
            #Choosing the range of the bubbles' sizes:
            sizes = c(20,50),
            marker = list(sizeref=data$buildings,opacity = 0.78, sizemode = 'diameter'),
            height=675
            ) %>%
      layout(title = paste('Facilities Between ', data$size[1], ' sqft by Primary Use',sep=""),  xaxis = list(title = 'Year', tickangle = -0), 
             yaxis = list(title = 'Kilowatt Hours per Square Foot'), 
             legend = list(title=list(text='<b> Primary Use </b>')),
             paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0")
  })
  
  #plot the energy efficiency spending and other mandates for APCO
  output$apco_ee_spending <- renderPlotly({
    apco_ee_spending
  })
  
  output$apco_mandates_and_progress <- renderPlotly(
    apco_mandates_and_progress
  )
  
  #plot energy efficiency mandates and spending for Dominion
  output$dominion_ee_spending <- renderPlotly(
    dominion_ee_spending
  )
  output$dominion_mandates_and_progress <- renderPlotly(
    dominion_mandates_and_progress
  )
  
  #plot efficiency mandates and spending for odp
  
  output$odp_ee_spending <- renderPlotly(
    odp_ee_spending
  )
  
  output$odp_mandates_and_progress <- renderPlotly(
    odp_mandates_and_progress
  )
  
}

shinyApp(ui = ui, server = server)