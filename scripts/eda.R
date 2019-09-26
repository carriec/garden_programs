#exploratory data analysis

#library
library(tidycensus)
library(tidyverse)
library(tigris)
library(readxl)

#set Census API key, obtained at: http://api.census.gov/data/key_signup.html
###census_api_key("INSERT_KEY_HERE", install = TRUE)

#Read in imported food production data
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx")
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_county <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx", sheet = 2, col_names = FALSE)
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state <- Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state[1:106,]

#Read in ICPSR data
load("./data/icpsr_data/ICPSR_36128/DS0001/36128-0001-Data.rda")
load("./data/icpsr_data/ICPSR_24642/DS0001/24642-0001-Data.rda")

#Joining food production and ICPSR data
###County Jails: Match imported State Codes in FACID data with JURISID fields
da36128.0001_Codebook_State_FACID <- read_excel("./data/raw_data/36128-0001-Codebook-State-Codes-Key.xlsx")
da36128.0001.temp <- 
  da36128.0001 %>%
  mutate(state_code = as.numeric(substr(as.character(JURISID), 1, 2))) %>%
  left_join(da36128.0001_Codebook_State_FACID, by = c("state_code" = "State_Codes_FACID")) 
###State and Federal Corrections
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state_temp <- 
  Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state %>%
  select(State, `Name of Correctional Facility`)

#Other
#data(fips_codes)