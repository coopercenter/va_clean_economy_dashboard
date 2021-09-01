# VA Clean Economy Dashboard

This is a document tutorial to run the dashboard on your local computer. This will be updated over time as new updates are created. 
LAST UPDATED: 7/24/2020


1. Clone the **ceps-viz** and **va_clean_economy_dashboard** repositories.
2. Turn on your UVA connect VPN.
3. If not already created, create Rproject files for **both** **ceps-viz and va_clean_economy_dashboard** repositories. 
4. Open RSTUDIO.
5. Change the project to **ceps-viz**
	* In the top right corner, there should be an icon of a blue cube. Click on that and choose **ceps-ciz**.
6.  Run **viz_functions.R**, **dashboard_calculations.R**, and **dashboard_plots.R** in that order.
	* viz_fuctions.R and dashboard_plots.R are in the ggplot2 folder. dashboard_calculations.R is in the derived values folder.
	* dashboard_plots.R will take a long time to run. This is normal. Be patient.
7. After all three have been run (wait for the stop sign on the console to disappear, you must wait until this is gone and the environment is finished adding things into it), go into your **ceps-viz** folder and look for a file that says **dashboard_output_test.RData**. Move that file (or copy and paste) into the **va_clean_economy_dashboard** folder. 
8. In RSTUDIO, change your project to **va_clean_economy_dashboard**
	* Refer to step 6 on how to do this.
9. Look for the **dashboard.R** file in the **va_clean_economy_dashboard** folder. Run it.
10. The dashboard should be up now. Some plots may not show right away but it will after a minute or 2. 


## Full Update Process (Updated 9/1/2021)
1. Retrieve eia api key (there should be one in the repo, otherwise this can be done at the eia website)
2. Place postgress credentials in the following format in your working directory in the following format:
	Filename: "my_postgres_credentials.R"
	File contents:
	db_user = "username"
	ra_pwd = "password"
	db_host = "va-energy2.postgres.database.azure.com"
3. Connect to UVA Anywhere VPN
4. Run "data_retrieval_and_cleaning/fetch_from_eia_api.R"
5. Run the rest of the scripts in "data_retrieval_and_cleaning" in no particular order
	Note: All of the scripts starting with "cleaning_" use data from the "raw_data" folder and do not need to be run if that folder is not manually updated. Additionally, these "cleaning_" files contain hard coded indicies, against best practice, and should be evaluated carefully when files in the "raw_data" folder is updated.
6. Run "calculations_and_plotting/dashboard_plots.R"
	Note: ensure you have all the necesary packages and dependencies to run dashboard_calculations.R as well, as this file is referenced in "dashboard_plots.R"
7. 

