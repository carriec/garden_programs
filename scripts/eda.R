#exploratory data analysis

#library
library(tidycensus)

#set Census API key, obtained at: http://api.census.gov/data/key_signup.html
###census_api_key("INSERT_KEY_HERE", install = TRUE)

#view imported food production data
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx")
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_county <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx", sheet = 2)
