#Loading the new energy efficiency spending mandates into the database from their CSV files------------------------------------------
#this will be updated once their structure is finalized and we have a more formalized data sharing setup with Virginia Energy

ee_resource_standard_projections <- read.csv(here('raw_data/energy_efficiency_resource_standard_projections.csv'))
ee_spending_progress <- read.csv(here('raw_data/energy_efficiency_spending_progress.csv'))
ee_spending_requirements <- read.csv(here('raw_data/energy_efficiency_spending_requirements.csv')) 

dbWriteTable(db,'energy_efficiency_resource_standard_projections',ee_resource_standard_projections)
dbWriteTable(db,'energy_efficiency_spending_progress', ee_spending_progress)
dbWriteTable(db,'energy_efficiency_spending_requirements',ee_spending_requirements)

#Writing the COVA facility tracker sheet from VE into the database----------------------------------------------------------------
#hopefully a more streamlined process soon
agency_facility_tracking <- read.csv(here('raw_data/COVA_Facility_Tracker_Simplified.csv'))

dbWriteTable(db,'agency_facility_tracking',agency_facility_tracking)