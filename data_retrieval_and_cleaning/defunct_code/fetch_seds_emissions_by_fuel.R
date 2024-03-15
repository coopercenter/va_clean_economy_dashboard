#read the excel file directly from the URL
raw_table <- openxlsx::read.xlsx("https://www.eia.gov/electricity/state/Virginia/xls/va.xlsx",startRow = 3,colNames=TRUE,
                                   rows=c(3:25),cols=c(1:32),sheet=8)
#Transpose the table
emissions_table <- data.table(t(raw_table))[-1] %>% #exclude the first row of remnant column names
  #get the years from the axed column names (minus the emissions type column)
  mutate(Year=str_replace(colnames(raw_table)[-1],"Year.","")) %>%
  #rename columns that will be kept
  rename(SO2_Coal=`2`,SO2_Natural_gas=`3`,SO2_Other=`4`, SO2_Petroleum=`5`,SO2_Total=`6`,
         NOx_Coal=`8`,NOx_Natural_gas=`9`,NOx_Other=`10`,NOx_Petroleum=`11`,NOx_Total=`12`,
         CO2_Coal=`14`,CO2_Natural_gas=`15`,CO2_Other=`16`,CO2_Petroleum=`17`,CO2_Total=`18`,
         Sulfur_dioxide_rate=`20`,Nitrogen_oxide_rate=`21`,Carbon_dioxide_rate=`22`) %>%
  #drop the empty columns
  select(-c(`1`,`7`,`13`,`19`))
# Convert from character to number
emissions_table[,1:ncol(emissions_table) := lapply(.SD, as.numeric),.SDcols=1:ncol(emissions_table)]
#Convert the SO2 and NOx values to metric tons from short tons
convert <- function(obs) {obs/1.102}
cols = c(1:10)   # Only SO2 and NOx are in short tons
emissions_table[,(cols) := lapply(.SD, convert),.SDcols=cols]
setkey(emissions_table,"Year")

dbRemoveTable(db,"va_electricity_emissions_by_fuel")
dbWriteTable(db,"va_electricity_emissions_by_fuel",emissions_table,append=F,row.names=F)
rm(emissions_table)

