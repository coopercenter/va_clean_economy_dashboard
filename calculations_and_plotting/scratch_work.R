#Can I turn all these similarly structured ring data creation functions into one function?
#Will need some outstanding data gaps to be resolved before these can be generalized, still too many things to set manually


carbon_free_donut_data <- function(){
  recent_year = eia_annual_data[!is.na(Percent_carbon_free),last(Year)]
  carbon_free_percent_gen_recent = round(eia_annual_data[!is.na(Percent_carbon_free),last(Percent_carbon_free)], 1)
  carbon_free_percent_gen_2050_goal = 100 #100% of Virginia’s electricity from carbon-free sources by 2050
  
  carbon_free_ring = data.frame(
    category = c(
      paste0(recent_year," carbon free generation (%)"),
      "Additional progress necessary to reach goal (%)"
    ),
    value = c(
      carbon_free_percent_gen_recent,
      carbon_free_percent_gen_2050_goal - carbon_free_percent_gen_recent
    )
  )
  return(carbon_free_ring)
}


single_ring_capacity_donut_data <- function(){
  onshore_wind_capacity_current_mw = 0  #va_wind[,sum(capacity_mw)]
  sw_capacity_2035_mw_goal = 16100 #16,100 MW of solar and onshore wind by January 1, 2024 (from VCEA Summary 3.0)
  
  sw_ring = data.frame(
    category = c(paste0(va_solar[,max(Operating_Year,na.rm=TRUE)], " capacity"),"Additional capacity necessary to reach goal"),
    value = c(
      va_solar[,sum(capacity_mw)] + onshore_wind_capacity_current_mw,
      sw_capacity_2035_mw_goal - (va_solar[,sum(capacity_mw)] + onshore_wind_capacity_current_mw)
    )
  )
  return(sw_ring)
}

renewable_donut_data <- function(){
  recent_year = eia_annual_data[!is.na(Percent_renewable),last(Year)]
  renewable_percent_gen_recent = round(eia_annual_data[!is.na(Percent_renewable),
                                                       last(Percent_renewable)], 1)
  renewable_percent_gen_2050_goal = 100
  
  renewable_ring = data.frame(
    category = c(
      paste(recent_year, "RPS generation (%)"),
      "Additional progress necessary to reach goal (%)"
    ),
    value = c(
      renewable_percent_gen_recent,
      renewable_percent_gen_2050_goal - renewable_percent_gen_recent
    )
  )
  return(renewable_ring)
}

offshore_wind_capacity_data <- function(){
  #recent_year = pjm_wind_working[,max(year(actual_in_service_date),na.rm=TRUE)]
  recent_year = 2021
  
  # The 12 MW of offshore wind is not showing up on the PJM queue
  # offshore_wind_current_mw = pjm_wind_working[fuel == "Offshore Wind" &
  #                                               status == "In Service", sum(mfo)]
  offshore_wind_current_mw = 12
  offshore_wind_2034_mw_goal = 5200 #Requires Dominion to develop 5,200 MW of offshore wind by Jan. 1, 2034 (from VCEA Summary 3.0)
  
  offshore_wind_ring = data.frame(
    category = c(paste(recent_year, "capacity"),"additional capacity necessary to reach goal"),
    value = c(
      offshore_wind_current_mw,
      offshore_wind_2034_mw_goal - offshore_wind_current_mw
    )
  )
  return(offshore_wind_ring)
}

storage_capacity_ring_data <- function(){
  storage_capacity_current_mw = 4
  storage_capacity_2035_mw_goal = 3100
  
  storage_ring = data.frame(
    category = c(paste(recent_year, "capacity"),
                 "Additional capacity necessary to reach goal"),
    value = c(
      storage_capacity_current_mw,
      storage_capacity_2035_mw_goal - storage_capacity_current_mw
    )
  )
  return(storage_ring)
}