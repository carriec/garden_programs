#exploratory data analysis

#library
library(tidycensus)
library(tidyverse)
library(tigris)

#set Census API key, obtained at: http://api.census.gov/data/key_signup.html
###census_api_key("INSERT_KEY_HERE", install = TRUE)

#Read in imported food production data
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx")
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_county <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx", sheet = 2, col_names = FALSE)
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state <- Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state[1:106,]

#Match imported State Codes in FACID data with JURISID fields
da36128.0001_Codebook_State_FACID <- read_excel("./data/raw_data/36128-0001-Codebook-State-Codes-Key.xlsx")
da36128.0001 %>%
  mutate(state_code = as.numeric(substr(as.character(JURISID), 1, 2)))

fips_codes <- data(fips_codes)