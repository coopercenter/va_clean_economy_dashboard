LBE_building_tracker <- read.csv(here('raw_data/COVA_Facility_Tracker_Simplified.csv'))
#replace #VALUE! with NA's
LBE_building_tracker[LBE_building_tracker=='#VALUE!'] <- NA
LBE_building_tracker <- LBE_building_tracker %>% 
  #trim whitespace in the name and code columns
  mutate(agency_name=trimws(agency_name), agency_code=trimws(agency_code),
         #assign all the agencies to categories
         agency_category=recode(agency_name,
                                #EDUCATION
                                "CHRISTOPHER NEWPORT UNIVERSITY"='Education',
                                "GEORGE MASON UNIVERSITY"='Education',
                                "JAMES MADISON UNIVERSITY"='Education',
                                "LONGWOOD COLLEGE"='Education',
                                "MARY WASHINGTON COLLEGE"='Education',
                                "NORFOLK STATE UNIVERSITY"='Education',
                                "OLD DOMINION UNIVERSITY"='Education',
                                "Radford University"='Education',
                                "UNIVERSITY OF VIRGINIA"='Education',
                                "University of Virginia's College at Wise"='Education',
                                "Central Virginia Community College"='Education',
                                'New River Community College'='Education',
                                'Patrick Henry Community College'='Education',
                                'Southwest Virginia Community College'='Education',
                                'Virginia Community College System'='Education',
                                'Virginia Highlands Community College'='Education',
                                'Virginia Western Community College'='Education',
                                'Wytheville Community College'='Education',
                                'VIRGINIA COMMONWEALTH UNIVERSITY'='Education',
                                'Virginia Polytechnic Institute and State University'='Education',
                                'VIRGINIA STATE UNIVERSITY'='Education',
                                'WILLIAM AND MARY, COLLEGE OF'='Education',
                                'VIRGINIA MILITARY INSTITUTE'='Education',
                                "Roanoke Higher Education Authority"='Education',
                                'VIRGINIA INSTITUTE OF MARINE SCIENCE'='Education',
                                "Southwest Virginia 4-H Educational Center"='Education',
                                'VIRGINIA ASSOCIATED RESEARCH CENTER'='Education',
                                'VIRGINIA BIOTECHNOLOGY RESEARCH PARK AUTHORITY'='Education',
                                'TRUCK & ORNAMENTAL RESEARCH ASSOCIATION'='Education',
                                'Catawba Hospital'='Health and Human Services',
                                #HEALTH AND HUMAN SERVICES
                                'Central Virginia Training Center'='Health and Human Services',
                                'Department of Behavioral Health and Developmental Services'='Health and Human Services',
                                'Southwestern Virginia Mental Health Institute'='Health and Human Services',
                                'MEDICAL COLLEGE OF HAMPTON ROADS'='Health and Human Services',
                                'Southern Virginia Mental Health Institute'='Health and Human Services',
                                'VIRGINIA DEPARTMENT OF HEALTH'='Health and Human Services',
                                'VIRGINIA DEPARTMENT OF MENTAL HEALTH'='Health and Human Services',
                                'VIRGINIA DEPARTMENT FOR AGING AND REHABILITATIVE SERVICES'='Health and Human Services',
                                'VIRGINIA DEPARTMENT FOR THE BLIND AND VISION IMPAIRED'='Health and Human Services', 
                                'Department of Social Services'='Health and Human Services',
                                #TRANSPORTATION
                                'CHESAPEAKE BAY BRIDGE TUNNEL COMMISSION'='Transportation',
                                'VIRGINIA DEPARTMENT OF AVIATION'='Transportation',
                                'RICHMOND METROPOLITAN AUTHORITY'='Transportation',
                                'VIRGINIA DEPARTMENT OF MOTOR VEHICLES'='Transportation',
                                'VIRGINIA PORT AUTHORITY'='Transportation',
                                'WASHINGTON METRO AREA TRANSIT AUTHORITY'='Transportation',
                                'Department of Transportation'='Transportation',
                                #NATURAL RESOURCES
                                'VIRGINIA DEPARTMENT OF CONSERVATION AND RECREATION'='Natural Resources',
                                'VIRGINIA DEPARTMENT OF ENVIRONMENTAL QUALITY'='Natural Resources',
                                'POTOMAC RIVER FISHERIES COMMISSION'='Natural Resources',
                                'VIRGINIA DEPARTMENT OF GAME AND INLAND FISHERIES'='Natural Resources',
                                'VIRGINIA OUTDOORS FOUNDATION'='Natural Resources',
                                #AGRICULTURE AND FORESTRY
                                'Department of Forestry'='Agriculture and Forestry',
                                'VIRGINIA DEPARTMENT OF AGRICULTURE AND CONSUMER SERVICES'='Agriculture and Forestry',
                                #CULTURE
                                'FRONTIER DISCOVERY MUSEUM'='Culture',
                                'SCIENCE MUSEUM OF VIRGINIA'='Culture',
                                'VIRGINIA MUSEUM OF FINE ARTS'='Culture',
                                'THE BOARD OF REGENTS/GUNSTON'='Culture',
                                'JAMESTOWN FOUNDATION'='Culture',
                                #ADMINISTRATION
                                'VIRGINIA DEPARTMENT OF GENERAL SERVICES'='Administration',
                                #PUBLIC SAFETY AND HOMELAND SECURITY
                                "Commonwealth's Attorneys' Services Council"='Public Safety and Homeland Security',
                                'Department of Fire Programs'='Public Safety and Homeland Security',
                                'Department of Forensic Science'='Public Safety and Homeland Security',
                                'VIRGINIA ALCOHOLIC BEVERAGE CONTROL AUTHORITY'='Public Safety and Homeland Security',
                                'VIRGINIA STATE POLICE DEPARTMENT'='Public Safety and Homeland Security',
                                'Department of State Police'='Public Safety and Homeland Security',
                                'VIRGINIA DEPARTMENT OF CORRECTIONS'='Public Safety and Homeland Security',
                                #COMMERCE AND TRADE
                                'VIRGINIA HOUSING DEVELOPMENT AUTHORITY'='Commerce and Trade',
                                'Department of Small Business and Supplier Diversity'='Commerce and Trade',
                                'Virginia Employment Commission'='Commerce and Trade',
                                'Virginia Tourism Authority'='Commerce and Trade',
                                #VETERANS AND DEFENSE AFFAIRS
                                'VIRGINIA DEPARTMENT OF MILITARY AFFAIRS'='Veterans and Defense Affairs',
                                'Department of Veterans Services'='Veterans and Defense Affairs',
                                #INDEPENDENT AGENCIES
                                'Virginia Lottery'='Independent Agencies',
                                'VIRGINIA RETIREMENT SERVICES'='Independent Agencies',
                                #OTHER SERVICES OR CATEGORY NOT KNOWN
                                'NO CONTRACT NUMBER ASSIGNED - UNIDENTIFIED'='Other Services or Category Not Known',
                                'Township'='Other Services or Category Not Known',
                                'Unidentified'='Other Services or Category Not Known',
                                'VIRGINIA HORSE CENTER'='Other Services or Category Not Known',
                                'WCTV CHANNEL 23'='Other Services or Category Not Known'
                                )) %>%
  #get all the fully capitalized names in agency_name to be conventionally capitalized
  mutate(agency_name=str_to_title(agency_name)) %>%
  #a couple of specific adjustments
  mutate(agency_name = recode(agency_name,
                              'William And Mary, College Of'='College of William and Mary',
                              'Wctv Channel 23'='WCTV Channel 23')) %>%
  #fix the capitalized 'of's from str_to_title
  mutate(agency_name=str_replace_all(agency_name,' Of ',' of '))
#write to the database
dbRemoveTable(db,"agency_facility_tracking")
dbWriteTable(db,"agency_facility_tracking",LBE_building_tracker,row.names=FALSE)
