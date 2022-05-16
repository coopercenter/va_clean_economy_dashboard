# Energy and emission intensity 
# Beware: energy consumption versus electricity consumption. 
### Work out the proper scaling for appropriate units.
# Currently (2021) the dashboard displays energy per capita
# maybe this should be changed
#Energy intensity
#
# Map local names to EIA data series (copied from dashboard_calculations.R because the local names are referenced here but weren't previously set except by running the calculations first)
#
eia_name=c("ELEC_GEN_COW_VA_99_A",
           "ELEC_GEN_PEL_VA_99_A",
           "ELEC_GEN_NG_VA_99_A",
           "ELEC_GEN_NUC_VA_99_A",
           "ELEC_GEN_SUN_VA_99_A",
           "ELEC_GEN_DPV_VA_99_A",
           "ELEC_GEN_HYC_VA_99_A",
           "ELEC_GEN_HPS_VA_99_A",
           "ELEC_GEN_WND_VA_99_A",
           "ELEC_GEN_WWW_VA_99_A",
           "ELEC_GEN_WAS_VA_99_A",
           "ELEC_GEN_ALL_VA_99_A",
           "SEDS_TETCB_VA_A",
           "SEDS_TERCB_VA_A",
           "SEDS_TECCB_VA_A",
           "SEDS_TEICB_VA_A",
           "SEDS_TEACB_VA_A",
           "SEDS_ELISP_VA_A",
           "EMISS_CO2_TOTV_EC_TO_VA_A",
           "EMISS_CO2_TOTV_TT_TO_VA_A")
local_name=c("Coal",
             "Oil",
             "Gas",
             "Nuclear",
             "Solar_utility", 
             "Solar_distributed",
             "Hydropower",
             "Pumped_storage",
             "Wind",
             "Wood",
             "Other_biomass",
             "Total_gen",
             "Total_energy_cons",
             "Residential",
             "Commercial",
             "Industrial",
             "Transportation",
             "Imported_electricity",
             "Electric_sector_CO2_emissions",
             "Total_CO2_emissions")

setnames(eia_annual_data,
         eia_name, local_name)


intensity_data = merge(eia_annual_data[Total_energy_cons!=0 & Total_CO2_emissions != 0,
                                       .(date,Total_energy_cons,Total_CO2_emissions)],
                       va_state_info,by="date",all=TRUE)
intensity_data[,energy_consumption_per_capita := Total_energy_cons/va_pop]

#### Need to work out the units to report
intensity_data[!is.na(va_rgsp),
               energy_consumption_per_gdp := Total_energy_cons*1000/va_rgsp]
# Emission intensity 
intensity_data[,co2_per_capita := Total_CO2_emissions/va_pop*1000]
intensity_data[,co2_per_gdp := Total_CO2_emissions*1000000/va_rgsp]
intensity_data = intensity_data[!is.na(Total_energy_cons)] 
dbRemoveTable(db,"intensity_data")
dbWriteTable(db,"intensity_data",intensity_data,append=F,row.names=F)
rm(intensity_data)