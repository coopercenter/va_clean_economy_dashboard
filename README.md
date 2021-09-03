# VA Clean Economy Dashboard

This is a document tutorial to run the dashboard on your local computer. This will be updated over time as new updates are created. 

## Run the Dashboard Locally (Updated 9/3/2021)
1. Clone the **va_clean_economy_dashboard** repository.
2. Turn on your UVA connect VPN.
3. If not already created, create Rproject files for **va_clean_economy_dashboard** repository. 
4. Open RSTUDIO.
8. In RSTUDIO, change your project to **va_clean_economy_dashboard**
9. Look for the **dashboard.R** file in the **va_clean_economy_dashboard** folder. Run it.
10. The dashboard should be up now. Some plots may not show right away but it will after a minute or two. 


## Full Update Process (Updated 9/1/2021)
1. Retrieve eia api key (there should be one in the repo, otherwise this can be done at the eia website)
2. Place postgress credentials in the following format in your working directory in the following format:
	- Filename: "my_postgres_credentials.R"
	- File contents:
	- db_user = "username"
	- ra_pwd = "password"
	- db_host = "va-energy2.postgres.database.azure.com"
3. Connect to UVA Anywhere VPN
4. Run "data_retrieval_and_cleaning/fetch_from_eia_api.R"
5. Run the rest of the scripts in "data_retrieval_and_cleaning" in no particular order
	- Note: All of the scripts starting with "cleaning_" use data from the "raw_data" folder and do not need to be run if that folder is not manually updated. Additionally, these "cleaning_" files contain hard coded indicies, against best practice, and should be evaluated carefully when files in the "raw_data" folder is updated.
6. Run "calculations_and_plotting/dashboard_plots.R"
	- Note: ensure you have all the necesary packages and dependencies to run dashboard_calculations.R as well, as this file is referenced in "dashboard_plots.R"
7. Publish using shinyapps.io the following files:
	1. dashboard_output.RData
	2. www
	3. dashboard.R
