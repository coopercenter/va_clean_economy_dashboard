#Write a function to fetch the latest EIA860 survey data------------------------------------------------------------------------------
readEIA860sheet <- function(year,local_directory,xlsx_file){
  # Downloads EIA zip file, unzips it to local_directory,
  #     then extracts the named sheet. Column labels are cleaned up
  #     into valid variable names
  
  #set the download path as a function of the latest year
  download_path = paste0("https://www.eia.gov/electricity/data/eia860/xls/eia860",year,'.zip')
  #set the local directory to download to as a function of the latest year
  local_file = paste0(local_directory,"/eia860",year,".zip")
  #download the .zip file from the EIA website (IDEALLY I'd like to be able to download just the file we use...)
  download.file(download_path,local_file,method="curl")
  #Unzip the specified file
  unzip(local_file,file=paste0(xlsx_file,"_Y",year,".xlsx"),exdir=local_directory)
  #get the full filepath for the file we just unzipped as a function of the current year
  file_name = paste0(local_directory,"/",xlsx_file,"_Y",year,".xlsx")
  #get a list of the column names
  nms <- names(read_excel(file_name, sheet='Operable', n_max = 0,skip=1))
  #format the column names
  nms <- gsub("[?())]","",nms) %>% str_replace_all(" ","_") %>% str_replace("/","_")
  #read in the full dataset with the new column names
  gen_capacity_data <- data.table(read_excel(file_name, sheet='Operable',skip=2,col_names=nms))
  gen_capacity_data <- gen_capacity_data %>% filter(State=="VA") %>% select(-State) %>%
    rename(RTO_ISO_Location_Designation=RTO_ISO_Location_Designation_for_Reporting_Wholesale_Sales_Data_to_FERC) %>%
    mutate(id=paste0(Plant_Code,"_",Generator_ID))
  #clean up the download
  unlink(file_name,recursive=TRUE)
  unlink(local_file,recursive=TRUE)
  setkey(id)
  return(gen_capacity_data)
}

#Retrieve EIA860 survey data for the latest year-----------------------------------------------------------------
#    EIA posts this data in around June of the subsequent year
# Then filter for solar (PV) and wind (WT)  and storage (BA)
#    Virginia doesn't have wind or storage up to 2020
#
#Set the directory to download to
local_directory = here("raw_data")
#Set the year we want to download
# Bypass this test, if you are sure that new data is available
if (month(now())<7) {
  year = year(now())-2
} else {
  year = year(now())-1
}
#Set the file we want to unzip from the .zip download
xlsx_file="3_1_Generator"
# This function call returns all Virginia active generation units
va_gen_plants = readEIA860sheet(year,local_directory,xlsx_file)
#Write the cleaned generation file to the database
dbRemoveTable(db,"eia_plant_capacities")
dbWriteTable(db,"eia_plant_capacities",va_gen_plants,append=F,row.names=F)

# You can filter for solar, wind or storage (redundant, this is not saved here and is done again in calculations)
#va_solar = va_gen_plants[Prime_Mover=="PV"]
#va_solar[,id:=paste0(Plant_Code,"_",Generator_ID)]
#setkey(va_solar,id)