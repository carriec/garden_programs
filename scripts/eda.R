#exploratory data analysis

#library
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
library(tidyverse)
library(readxl)
library(maps)

#set Census API key, obtained at: http://api.census.gov/data/key_signup.html
###census_api_key("INSERT_KEY_HERE", install = TRUE)


#read in imported food production data
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx")
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_county <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx", sheet = 2, col_names = FALSE)
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state <- Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state[1:106,]
Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state <- read_excel("data/raw_data/Correctional_Facility_Contact_Tracking_Addy_States_SUMMER.xlsx")
Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_county <- read_excel("data/raw_data/Correctional_Facility_Contact_Tracking_Addy_States_SUMMER.xlsx", sheet = 2)
Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state <- Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state[1:96,] 
Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_county <- Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_county[1:356,]


#tidy data
Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp <-
  Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state %>%
  pivot_longer(cols = `Farm or ranch?`:`Other? (Identify if so)`, names_to = "Program", values_to = "Type_temp") 

data(fips_codes)
f <- fips_codes %>%
  select(-county_code, -county) %>%
  distinct()

Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp <-
  Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp %>%
  mutate(Type = tolower(Type_temp)) %>%
  left_join(f, by = c("State" = "state_name")) %>%
  select(-Type_temp)

Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_count <-
  subset(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp, !is.na(Type)) %>%
  mutate(state = tolower(State)) %>%
  group_by(state) %>%
  count(Type)
  

#visualize data
p <- ggplot(data = subset(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp, !is.na(Type)), aes(state, ..count..)) +
     geom_bar(aes(fill = Type))
p

map <- map_data("state")
d <- ggplot(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_count, aes(fill = n))
d + geom_map(aes(map_id = state), map = map) +
  expand_limits(x = map$long, y = map$lat) +
  facet_wrap( ~ Type)


#exploring census data
v <- load_variables(2010, dataset = "sf1", cache = TRUE)
View(v)
m <- get_decennial(geography = "state", variables = "P001001", geometry = TRUE) %>%
  left_join()
ggplot(m, aes(fill = value, color = value)) +
  geom_sf() +
  coord_sf(crs = 26914)


#read in ICPSR data
load("./data/icpsr_data/ICPSR_36128/DS0001/36128-0001-Data.rda")
load("./data/icpsr_data/ICPSR_24642/DS0001/24642-0001-Data.rda")


#Joining food production and ICPSR data -- in progress

###County Jails: Match imported State Codes in FACID data with JURISID fields
da36128.0001_Codebook_State_FACID <- read_excel("./data/raw_data/36128-0001-Codebook-State-Codes-Key.xlsx")
da36128.0001.temp <- 
  da36128.0001 %>%
  mutate(state_code = as.numeric(substr(as.character(JURISID), 1, 2))) %>%
  left_join(da36128.0001_Codebook_State_FACID, by = c("state_code" = "State_Codes_FACID")) 
###State and Federal Corrections
