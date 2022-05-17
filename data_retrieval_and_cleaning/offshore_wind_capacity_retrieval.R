#Read in current estimates on offshore wind capacity and capacity factor from spreadsheet-------------------------------------------
### The source of this data is not documented.
## This needs to be fixed.

offshore_wind <- data.table(read_xlsx(here('raw_data','offshore_wind_data.xlsx'),
                                      sheet = "Offshore_wind_data",skip=1))
total_mw_offshore_wind = offshore_wind[,.(Year,CVOW_Pilot,CVOW_Stage_I,
                                          CVOW_Stage_II,CVOW_Stage_III,Total_mw)]
net_capacity_factor_offshore_wind <- offshore_wind[,.(Year,Pilot_cf,Stage_1_cf,
                                                      Stage_2_cf,Stage_3_cf)]
total_production_forecast_offshore_wind <- offshore_wind[,.(Year,Stage_1_gen,
                                                            Stage_2_gen,Stage_3_gen,Total_gen)]
setnames(total_production_forecast_offshore_wind,'Total_gen','Total_Production')

dbRemoveTable(db,"offshore_wind")
dbWriteTable(db,"offshore_wind",offshore_wind,append=F,row.names=F)
rm(offshore_wind)