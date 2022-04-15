#Write a function to fetch the latest EIA860 survey data------------------------------------------------------------------------------
readEIA860sheet <- function(year,local_directory,
                            xlsx_file="3_1_Generator") {
  # Downoads EIA zip file, unzips it to local_directory,
  #     then extracts the named sheet. Column labels are cleaned up
  #     into valid variable names
  #lbry<-c("data.table", "RPostgreSQL",  "tidyr", "dplyr","arrow",
         # "tools","lubridate", "Hmisc", "here", "readxl","stringr")
  #test <- suppressMessages(lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE))
  #rm(test,lbry)
  #  year = 2020
  #  local_directory = here("raw_data")
  EIA_path = "https://www.eia.gov/electricity/data/eia860/xls/"
  zip_file_name = paste0("eia860",year,".zip")
  
  url = paste0(EIA_path,zip_file_name) 
  local_file = paste0(local_directory,zip_file_name)
  cat(paste0(local_file,"\n"))
  download.file(url,local_file,method="curl")
  unzip(local_file,exdir=local_directory)
  #  xlsx_file="3_1_Generator"
  sheet="Operable"
  file_name = paste0(local_directory,"/",xlsx_file,"_Y",year,".xlsx")
  nms <- names(read_excel(file_name, sheet, n_max = 0,skip=1))
  good_names = gsub("[?())]","",nms)
  good_names = gsub("[[:punct:][:blank:]/]","_",good_names)
  allStatesData = data.table(read_excel(file_name, sheet,skip=2,col_names=good_names))
  va_gen_capacity = allStatesData[State=="VA",][,State:=NULL,]
  va_gen_capacity[,RTO_ISO_Location_Designation:=RTO_ISO_Location_Designation_for_Reporting_Wholesale_Sales_Data_to_FERC]
  va_gen_capacity[,id:=paste0(Plant_Code,"_",Generator_ID)]
  setkey(id)
  return(va_gen_capacity)
}

#Retrieve EIA860 survey data for the latest year-----------------------------------------------------------------
#    EIA posts this data in around June of the subsequent year
# Then filter for solar (PV) and wind (WT)  and storage (BA)
#    Virginia doesn't have wind or storage up to 2020
#
local_directory = here("raw_data")
# Bypass this test, if you are sure that new data is available
if (month(now())<7) {
  year = year(now())-2
} else {
  year = year(now())-1
}
# This function call returns all Virginia active generation units
va_gen_plants = readEIA860sheet(year,local_directory,xlsx_file="3_1_Generator")
dbRemoveTable(db,"eia_plant_capacities")
dbWriteTable(db,"eia_plant_capacities",va_gen_plants,append=F,row.names=F)

# You can filter for solar, wind or storage
va_solar = va_gen_plants[Prime_Mover=="PV"]
va_solar[,id:=paste0(Plant_Code,"_",Generator_ID)]
setkey(va_solar,id)