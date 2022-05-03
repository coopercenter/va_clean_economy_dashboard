LBE_building_tracker <- read.csv(here('raw_data/COVA_Facility_Tracker_Simplified.csv'))
#replace #VALUE! with NA's
LBE_building_tracker[LBE_building_tracker=='#VALUE!'] <- NA

#trim whitespace in the name and code columns
LBE_building_tracker$agency_name <- trimws(LBE_building_tracker$agency_name)
LBE_building_tracker$agency_code <- trimws(LBE_building_tracker$agency_code)

#make a new column of categories to assign to the agency names for more readable aggregation
education <- c("CHRISTOPHER NEWPORT UNIVERSITY","GEORGE MASON UNIVERSITY","JAMES MADISON UNIVERSITY",
               "LONGWOOD COLLEGE","MARY WASHINGTON COLLEGE","NORFOLK STATE UNIVERSITY","OLD DOMINION UNIVERSITY",
               "Radford University","UNIVERSITY OF VIRGINIA","University of Virginia's College at Wise","Central Virginia Community College",
               'New River Community College','Patrick Henry Community College','Southwest Virginia Community College',
               'Virginia Community College System','Virginia Highlands Community College','Virginia Western Community College',
               'Wytheville Community College','VIRGINIA COMMONWEALTH UNIVERSITY','Virginia Polytechnic Institute and State University',
               'VIRGINIA STATE UNIVERSITY','WILLIAM AND MARY, COLLEGE OF','VIRGINIA MILITARY INSTITUTE',
               "Roanoke Higher Education Authority",
               'VIRGINIA INSTITUTE OF MARINE SCIENCE',
               "Southwest Virginia 4-H Educational Center",
               'VIRGINIA ASSOCIATED RESEARCH CENTER',
               'VIRGINIA BIOTECHNOLOGY RESEARCH PARK AUTHORITY',
               'TRUCK & ORNAMENTAL RESEARCH ASSOCIATION')

health_and_human_svs <- c('Catawba Hospital','Central Virginia Training Center',
                          'Department of Behavioral Health and Developmental Services',
                          'Southwestern Virginia Mental Health Institute','MEDICAL COLLEGE OF HAMPTON ROADS',
                          'Southern Virginia Mental Health Institute','VIRGINIA DEPARTMENT OF HEALTH',
                          'VIRGINIA DEPARTMENT OF MENTAL HEALTH','VIRGINIA DEPARTMENT FOR AGING AND REHABILITATIVE SERVICES',
                          'VIRGINIA DEPARTMENT FOR THE BLIND AND VISION IMPAIRED', 'Department of Social Services')

transportation <- c('CHESAPEAKE BAY BRIDGE TUNNEL COMMISSION','VIRGINIA DEPARTMENT OF AVIATION',
                    'RICHMOND METROPOLITAN AUTHORITY','VIRGINIA DEPARTMENT OF MOTOR VEHICLES',
                    'VIRGINIA PORT AUTHORITY','WASHINGTON METRO AREA TRANSIT AUTHORITY','Department of Transportation')

natural_resources <- c('VIRGINIA DEPARTMENT OF CONSERVATION AND RECREATION',
                       'VIRGINIA DEPARTMENT OF ENVIRONMENTAL QUALITY','POTOMAC RIVER FISHERIES COMMISSION',
                       'VIRGINIA DEPARTMENT OF GAME AND INLAND FISHERIES','VIRGINIA OUTDOORS FOUNDATION')

agriculture_and_forestry <- c('Department of Forestry','VIRGINIA DEPARTMENT OF AGRICULTURE AND CONSUMER SERVICES')

culture <- c('FRONTIER DISCOVERY MUSEUM','SCIENCE MUSEUM OF VIRGINIA','VIRGINIA MUSEUM OF FINE ARTS',
             'THE BOARD OF REGENTS/GUNSTON','JAMESTOWN FOUNDATION')

administration <- c('VIRGINIA DEPARTMENT OF GENERAL SERVICES')

public_safety_and_homeland_security <- c("Commonwealth's Attorneys' Services Council",
                                         'Department of Fire Programs','Department of Forensic Science',
                                         'VIRGINIA ALCOHOLIC BEVERAGE CONTROL AUTHORITY',
                                         'VIRGINIA STATE POLICE DEPARTMENT','Department of State Police',
                                         'VIRGINIA DEPARTMENT OF CORRECTIONS')

commerce_and_trade <- c('VIRGINIA HOUSING DEVELOPMENT AUTHORITY',
                        'Department of Small Business and Supplier Diversity','Virginia Employment Commission',
                        'Virginia Tourism Authority')

veterans_and_defense_affairs <- c('VIRGINIA DEPARTMENT OF MILITARY AFFAIRS','Department of Veterans Services')

independent_agencies <- c('Virginia Lottery','VIRGINIA RETIREMENT SERVICES')

other <- c('NO CONTRACT NUMBER ASSIGNED - UNIDENTIFIED','Township','Unidentified',
           'VIRGINIA HORSE CENTER','WCTV CHANNEL 23')

LBE_building_tracker$agency_category <- LBE_building_tracker$agency_name

consolidate_agency_categories <- function(agency_list, new_agency_category){
  for (item in agency_list){
    LBE_building_tracker$agency_category <- LBE_building_tracker$agency_category %>% 
      replace(LBE_building_tracker$agency_category==item,new_agency_category)
  }
  return(LBE_building_tracker$agency_category)
}



LBE_building_tracker$agency_category <- consolidate_agency_categories(education,'Education')
LBE_building_tracker$agency_category <- consolidate_agency_categories(health_and_human_svs,'Health and Human Services')
LBE_building_tracker$agency_category <- consolidate_agency_categories(transportation,'Transportation')
LBE_building_tracker$agency_category <- consolidate_agency_categories(natural_resources,'Natural Resources')
LBE_building_tracker$agency_category <- consolidate_agency_categories(agriculture_and_forestry,'Agriculture and Forestry')
LBE_building_tracker$agency_category <- consolidate_agency_categories(culture,'Culture')
LBE_building_tracker$agency_category <- consolidate_agency_categories(administration,'Administration')
LBE_building_tracker$agency_category <- consolidate_agency_categories(public_safety_and_homeland_security,'Public Safety and Homeland Security')
LBE_building_tracker$agency_category <- consolidate_agency_categories(independent_agencies,'Independent Agencies')
LBE_building_tracker$agency_category <- consolidate_agency_categories(commerce_and_trade,'Commerce and Trade')
LBE_building_tracker$agency_category <- consolidate_agency_categories(veterans_and_defense_affairs,'Veterans and Defense Affairs')
LBE_building_tracker$agency_category <- consolidate_agency_categories(other,'Other Services or Category Not Known')


rm(culture,health_and_human_svs,transportation,natural_resources,agriculture_and_forestry,
   education,administration,public_safety_and_homeland_security,independent_agencies,commerce_and_trade,
   veterans_and_defense_affairs,other)

#get all the fully capitalized names in agency_name to be conventionally capitalized
LBE_building_tracker <- LBE_building_tracker %>% 
  mutate(agency_name=str_to_title(LBE_building_tracker$agency_name)) %>%
  #a couple of specific adjustments
  mutate(agency_name = recode(agency_name,
                              'William And Mary, College Of'='College of William and Mary',
                              'Wctv Channel 23'='WCTV Channel 23'))

#write to the database
dbRemoveTable(db,"agency_facility_tracking")
dbWriteTable(db,"agency_facility_tracking",LBE_building_tracker,row.names=FALSE)
