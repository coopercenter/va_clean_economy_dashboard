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
