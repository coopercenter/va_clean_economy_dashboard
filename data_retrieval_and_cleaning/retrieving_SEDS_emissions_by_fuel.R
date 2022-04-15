read_SEDS_emission_data <- function(local_file = "eia_emission_by_fuel.xlsx") {
  ### Divide short tons by 1.102 to get metric tons.
  local_file = here("raw_data/eia_emission_by_fuel.xlsx")
  url = "https://www.eia.gov/electricity/state/Virginia/xls/va.xlsx"
  download.file(url,local_file,method="curl")
  raw_table = data.table(t(read_excel(local_file, skip=1,sheet="7. Emissions")))
  nms <- raw_table[1,1:ncol(raw_table)]
  raw_table = raw_table[-1]
  good_names = gsub("[\n]","_",nms)
  good_names = gsub("[?())]","",good_names)
  good_names = gsub("[[:punct:][:blank:]/]","_",good_names)
  good_names[1] = "Year"
  # SO2 vars are in 3:7, NOx in 9:13, CO2 in 15:19, rate data in 21:23
  good_names[3:7] = paste0("SO2_",good_names[3:7])
  good_names[9:13] = paste0("NOx_",good_names[9:13])
  good_names[15:19] = paste0("CO2_",good_names[15:19])
  good_names[21:23] = paste0(good_names[21:23],"_rate")
  setnames(raw_table,good_names)
  raw_table[,20:=NULL];raw_table[,14:=NULL];raw_table[,8:=NULL];raw_table[,2:=NULL]
  last_col_to_keep = length(raw_table)-1
  emissions_table <- raw_table[,1:last_col_to_keep]
  emissions_table[,Year := gsub("[\n\r]","",Year)]
  emissions_table[,Year := as.integer(str_replace_all(Year,'Year',''))]
  last_col = ncol(emissions_table)
  cols = 2:last_col
  # Convert from character to number
  emissions_table[,(cols) := lapply(.SD, as.numeric),.SDcols=cols]
  # Convert from short tons to metric tons
  # CO2 is already in thousands of metric tons
  convert <- function(obs) {obs/1.102}
  cols = c(2:11)   # Only SO2 and NOx are in short tons
  emissions_table[,(cols) := lapply(.SD, convert),.SDcols=cols]
  setkey(emissions_table,"Year")
  return(emissions_table)
}

# Now for SEDS data on electricity sector emissions by fuel source
# This data.table has SO2,NOx and CO2 emissions and emission rates by pollutant
electricity_emissions_by_fuel = read_SEDS_emission_data(local_file = here("raw_data/eia_emission_by_fuel.xlsx"))

dbRemoveTable(db,"va_electricity_emissions_by_fuel")
dbWriteTable(db,"va_electricity_emissions_by_fuel",electricity_emissions_by_fuel,append=F,row.names=F)
rm(electricity_emissions_by_fuel)