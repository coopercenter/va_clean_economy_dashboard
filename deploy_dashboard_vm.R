library(shiny)
library(rsconnect)
#runApp(appDir = 'C:/Users/Chloe/Desktop/va_clean_economy_dashboard/dashboard.R', launch.browser = TRUE)
rsconnect::deployApp(appDir = '~/Desktop/va_clean_economy_dashboard',
                     appPrimaryDoc = 'dashboard.R',
                     launch.browser = TRUE, appName = 'va_clean_economy_dashboard_production', 
                     server = 'shinyapps.io', account = 'cleanenergyva')