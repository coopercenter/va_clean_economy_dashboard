#Queries for the apiv2 versions of the apiv1 series IDs---------------------------------------
#State Total Energy Consumption, Annual
c(SEDS_TETCB_VA_A = paste0("https://api.eia.gov/v2/seds/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[seriesId][]=TETCB&facets[stateId][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State Total Primary Energy Production, Annual
SEDS_TEPRB_VA_A = paste0("https://api.eia.gov/v2/seds/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[seriesId][]=TEPRB&facets[stateId][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State Total Energy Consumed by Residential Sector, Annual
SEDS_TERCB_VA_A = paste0("https://api.eia.gov/v2/seds/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[seriesId][]=TERCB&facets[stateId][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State Total Energy Consumed by Commercial Sector, Annual
SEDS_TECCB_VA_A = paste0("https://api.eia.gov/v2/seds/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[seriesId][]=TECCB&facets[stateId][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State Total Energy Consumed by Industrial Sector, Annual
SEDS_TEICB_VA_A = paste0("https://api.eia.gov/v2/seds/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[seriesId][]=TEICB&facets[stateId][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State Total Energy Consumed by Electric Power Sector, Annual
SEDS_TEEIB_VA_A = paste0("https://api.eia.gov/v2/seds/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[seriesId][]=TEEIB&facets[stateId][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State Total Energy Consumed by Transportation Sector, Annual
SEDS_TEACB_VA_A = paste0("https://api.eia.gov/v2/seds/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[seriesId][]=TEACB&facets[stateId][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#Net interstate flow of electricity, Annual
SEDS_ELISP_VA_A = paste0("https://api.eia.gov/v2/seds/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[seriesId][]=ELISP&facets[stateId][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#Total final electricity sales in million kWh, all sectors, monthly
ELEC_SALES_VA_ALL_A = paste0("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=",eia_key,"&frequency=annual&data[0]=sales&facets[sectorid][]=ALL&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#Total final electricity sales in million kWh, residential, monthly
ELEC_SALES_VA_RES_A = paste0("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=",eia_key,"&frequency=annual&data[0]=sales&facets[sectorid][]=RES&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#Total final electricity sales in million kWh, commercial, monthly
ELEC_SALES_VA_COM_A = paste0("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=",eia_key,"&frequency=annual&data[0]=sales&facets[sectorid][]=COM&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#Total final electricity sales in million kWh, industrial, monthly
ELEC_SALES_VA_IND_A = paste0("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=",eia_key,"&frequency=annual&data[0]=sales&facets[sectorid][]=IND&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#Total final electricity sales in million kWh, transportation, monthly
ELEC_SALES_VA_TRA_A = paste0("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=",eia_key,"&frequency=annual&data[0]=sales&facets[sectorid][]=TRA&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#Total final electricity sales in million kWh, other, monthly
ELEC_SALES_VA_OTH_A = paste0("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=",eia_key,"&frequency=annual&data[0]=sales&facets[sectorid][]=OTH&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State Electricity Sales for All Sectors, Price in Cents per kWh, monthly
ELEC_PRICE_VA_ALL_A = paste0("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=",eia_key,"&frequency=annual&data[0]=price&facets[sectorid][]=ALL&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State Electricity Sales, Commercial, Price in Cents per kWh, monthly
ELEC_PRICE_VA_COM_A = paste0("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=",eia_key,"&frequency=annual&data[0]=price&facets[sectorid][]=COM&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State Electricity Sales, Residential, Price in Cents per kWh, monthly
ELEC_PRICE_VA_RES_A = paste0("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=",eia_key,"&frequency=annual&data[0]=price&facets[sectorid][]=RES&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State Electricity Sales, Industrial, Price in Cents per kWh, monthly
ELEC_PRICE_VA_IND_A = paste0("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=",eia_key,"&frequency=annual&data[0]=price&facets[sectorid][]=IND&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State Electricity Sales, Transportation, Price in Cents per kWh, monthly
ELEC_PRICE_VA_TRA_A = paste0("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=",eia_key,"&frequency=annual&data[0]=price&facets[sectorid][]=TRA&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State customers, all sectors, monthly
ELEC_CUSTOMERS_VA_ALL_M = paste0("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=",eia_key,"&frequency=monthly&data[0]=customers&facets[sectorid][]=ALL&facets[stateid][]=VA&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, all fuel types, all sectors, in thousand MWh, monthly
ELEC_GEN_ALL_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=ALL&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, estimated total solar, all sectors, in thousand MWh, monthly
ELEC_GEN_TSN_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=TSN&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, conventional hydroelectric, all sectors, in thousand MWh, monthly
ELEC_GEN_HYC_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=HYC&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, nuclear, all sectors, in thousand MWh, monthly
ELEC_GEN_NUC_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=NUC&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, coal, all sectors, in thousand MWh, monthly
ELEC_GEN_COW_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=COW&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, natural gas, all sectors, in thousand MWh, monthly
ELEC_GEN_NG_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=NG&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, all renewables, all sectors, in thousand MWh, monthly
ELEC_GEN_AOR_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=AOR&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, petroleum liquids, all sectors, in thousand MWh, monthly
ELEC_GEN_PEL_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=PEL&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, solar, all sectors, in thousand MWh, monthly
ELEC_GEN_SUN_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=SUN&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, solar, IPP Non-CHP, in thousand MWh, monthly
ELEC_GEN_SUN_VA_2_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=SUN&facets[location][]=VA&facets[sectorid][]=2&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, estimated distributed solar photovoltaic, all sectors, in thousand MWh, monthly
ELEC_GEN_DPV_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=DPV&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, estimated solar photovoltaic, all sectors, in thousand MWh, monthly
ELEC_GEN_SPV_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=SPV&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, wind, all sectors, in thousand MWh, monthly
ELEC_GEN_WND_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=WND&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, wood and wood waste, all sectors, in thousand MWh, monthly
ELEC_GEN_WWW_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=WWW&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, renewable waste products, all sectors, in thousand MWh, monthly
ELEC_GEN_WAS_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=WAS&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, hydro-electric pumped storage, all sectors, in thousand MWh, monthly
ELEC_GEN_HPS_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=HPS&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State generation data, other fuel sources, all sectors, in thousand MWh, monthly
ELEC_GEN_OTH_VA_99_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=OTH&facets[location][]=VA&facets[sectorid][]=99&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State CO2 emissions data, all fuels, electric power, in million metric tons of CO2, annual
EMISS_CO2_TOTV_EC_TO_VA_A = paste0("https://api.eia.gov/v2/co2-emissions/co2-emissions-aggregates/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[stateId][]=VA&facets[sectorId][]=EC&facets[fuelId][]=TO&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State CO2 emissions data, all fuels, all sectors, in million metric tons of CO2, annual
EMISS_CO2_TOTV_TT_TO_VA_A = paste0("https://api.eia.gov/v2/co2-emissions/co2-emissions-aggregates/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[stateId][]=VA&facets[sectorId][]=TT&facets[fuelId][]=TO&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State CO2 emissions data, coal, all sectors, in million metric tons of CO2, annual
EMISS_CO2_TOTV_TT_CO_VA_A = paste0("https://api.eia.gov/v2/co2-emissions/co2-emissions-aggregates/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[stateId][]=VA&facets[sectorId][]=TT&facets[fuelId][]=CO&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State CO2 emissions data, natural gas, all sectors, in million metric tons of CO2, annual
EMISS_CO2_TOTV_TT_NG_VA_A = paste0("https://api.eia.gov/v2/co2-emissions/co2-emissions-aggregates/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[stateId][]=VA&facets[sectorId][]=TT&facets[fuelId][]=NG&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State CO2 emissions data, petroleum, all sectors, in million metric tons of CO2, annual
EMISS_CO2_TOTV_TT_PE_VA_A = paste0("https://api.eia.gov/v2/co2-emissions/co2-emissions-aggregates/data/?api_key=",eia_key,"&frequency=annual&data[0]=value&facets[stateId][]=VA&facets[sectorId][]=TT&facets[fuelId][]=PE&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, all fuels, thousand MWh, monthly
ELEC_GEN_ALL_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=ALL&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, all coal products, thousand MWh, monthly
ELEC_GEN_COW_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=COW&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, conventional hydroelectric, thousand MWh, monthly
ELEC_GEN_HYC_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=HYC&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, natural gas, thousand MWh, monthly
ELEC_GEN_NG_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=NG&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, nuclear, thousand MWh, monthly
ELEC_GEN_NUC_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=NUC&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, renewable waste products, thousand MWh, monthly
ELEC_GEN_WAS_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=WAS&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, petroleum liquids, thousand MWh, monthly
ELEC_GEN_PEL_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=PEL&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, hydroelectric pumped storage, thousand MWh, monthly
ELEC_GEN_HPS_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=HPS&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, estimated total solar, thousand MWh, monthly
ELEC_GEN_TSN_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=TSN&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, solar, thousand MWh, monthly
ELEC_GEN_SUN_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=SUN&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, solar photovoltaic, thousand MWh, monthly
ELEC_GEN_SPV_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=SPV&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, wind, thousand MWh, monthly
ELEC_GEN_WND_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=WND&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"),
#State electric power generation, electric utility, wood and wood waste, thousand MWh, monthly
ELEC_GEN_WWW_VA_1_A = paste0("https://api.eia.gov/v2/electricity/electric-power-operational-data/data/?api_key=",eia_key,"&frequency=annual&data[0]=generation&facets[fueltypeid][]=WWW&facets[location][]=VA&facets[sectorid][]=1&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"))