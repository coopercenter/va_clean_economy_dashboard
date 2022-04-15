#Read in VCEA provisions from Excel sheet-------------------------------------------------------------------------
VCEA <- data.table(read_excel(here('raw_data','VCEA_goals.xlsx')))
# Multiply the two decimal percent columns by 100
cols = c("apco_energy_efficiency_as_share_of_2019_sales",
         "dominion_energy_efficiency_as_share_of_2019_sales")
VCEA[,(cols) := lapply(.SD,"*",100),.SDcols=cols]
#
dbRemoveTable(db,"vcea_provisions")
dbWriteTable(db,"vcea_provisions",VCEA,append=F,row.names=F)
rm(VCEA)