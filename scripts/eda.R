#exploratory data analysis

#library
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
library(tidyverse)
library(readxl)
library(maps)
library(geojsonR)
library(sf)
library(RCurl)
library(tmaptools)


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
Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state <- read_excel("data/raw_data/Correctional_Facility_Contact_Tracking_Josh_States_SUMMER.xlsx")
Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_county <- read_excel("data/raw_data/Correctional_Facility_Contact_Tracking_Josh_States_SUMMER.xlsx", sheet = 2)

#add latitude and longitude
####addy
Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp <-
  Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state %>%
  unite("Name and State", c(`Name of Correctional Facility`, `State`), sep = ", ", remove = FALSE)

latlon.addy.state <- geocode_OSM(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp$`Name and State`)

Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp <-
  Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp %>%
  left_join(latlon.addy.state, by = c("Name and State" = "query"))

####josh
Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp <-
  Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state %>%
  unite("Name and State", c(`Name of Correctional Facility`, `State`), sep = ", ", remove = FALSE)

latlon.josh.state <- geocode_OSM(Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp$`Name and State`)

Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp <-
  Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp %>%
  left_join(latlon.josh.state, by = c("Name and State" = "query"))

#maps of HIFLD data
file_js = FROM_GeoJson(url_file_string = "https://opendata.arcgis.com/datasets/2d6109d4127d458eaf0958e4c5296b67_0.geojson", Average_Coordinates = TRUE)
class(file_js)
file_js$
  
  test<-getURL("https://opendata.arcgis.com/datasets/2d6109d4127d458eaf0958e4c5296b67_0.geojson")
test2 <- st_read(test)
class(test2)
head(test2$geometry,1)
st_bbox(test2[2,])
st_coordinates(test2)
st_geometry(test2)[[1]][[1]]
st_bbox(test2[1])

###min and max lat and lon coordinates

test3 <- test2 %>%
  mutate(xmin = st_bbox(test2[ ,]))
         
    print(row_bbox$xmin) })%>%
  mutate(ymin = for (i in nrow(test2)) {
    row_bbox <- st_bbox(test2[i,])
    print(row_bbox$ymin) }) %>%
  mutate(xmax = for (i in nrow(test2)) {
    row_bbox <- st_bbox(test2[i,])
    print(row_bbox$xmax) }) %>%
  mutate(ymax = for (i in nrow(test2)) {
    row_bbox <- st_bbox(test2[i,])
    print(row_bbox$ymax) })
          

ggplot(data=test2) +
  geom_sf()

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
us_map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap()
us_map + geom_sf(data = test2, inherit.aes = FALSE) +
  scale_fill_brewer(palette = "OrRd") +
  coord_sf(crs = st_crs(4326))

#tidy data
###addy
Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp <-
  Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state %>%
  pivot_longer(cols = `Farm or ranch?`:`Other? (Identify if so)`, names_to = "Program", values_to = "Type_temp") %>%
  distinct()

###josh
names(Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state)
head(Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state[,13])


Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp <-
  Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state %>%
  separate(col = "Other? (Identify if so)", into = c("A","B","C","D","E"), sep = "; ") %>%
  pivot_longer(cols = 11:16, names_to = "Program_temp", values_to = "Type_temp")
  
Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp <-
  Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp %>%
  mutate(Program = (gsub("A|B|C|D|E", "Other? (Identify if so)", Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp$Program_temp))) %>%
  select(-Program_temp) %>%
  distinct()
       
data(fips_codes)
f <- fips_codes %>%
  select(-county_code, -county) %>%
  distinct()

###addy
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

###josh
Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp <-
  Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp %>%
  mutate(Type = tolower(Type_temp)) %>%
  left_join(f, by = c("State" = "state_name")) %>%
  select(-Type_temp)

Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_count <-
  subset(Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp, !is.na(Type)) %>%
  mutate(state = tolower(State)) %>%
  group_by(state) %>%
  count(Type)


#visualize data
###addy
p <- ggplot(data = subset(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp, !is.na(Type)), aes(state, ..count..)) +
     geom_bar(aes(fill = Type))
p
q <- ggplot(data = Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp, aes(state, ..count..)) +
  geom_bar(aes(fill = Program))
q
r <- ggplot(data = subset(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp, !is.na(Type)), aes(state, ..count..)) +
  geom_bar(aes(fill = Type)) +
  facet_wrap(~ Program)
r

###josh
s <- ggplot(data = subset(Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp, !is.na(Type)), aes(state, ..count..)) +
  geom_bar(aes(fill = Type))
s
t <- ggplot(data = Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp, aes(state, ..count..)) +
  geom_bar(aes(fill = Program))
t
u <- ggplot(data = subset(Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp, !is.na(Type)), aes(state, ..count..)) +
  geom_bar(aes(fill = Type)) +
  facet_wrap(~ Program)
u

map <- map_data("state")
d <- ggplot(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_count, aes(fill = n))
d + geom_map(aes(map_id = state), map = map) +
  expand_limits(x = map$long, y = map$lat) +
  facet_wrap( ~ Type)
d + geom_map(aes(map_id = state), map = map) +
  expand_limits(x = map$long, y = map$lat) +
  facet_wrap( ~ Program)


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
view(da24642.0001 %>%
  filter(V1 == "440000000075500000000"))



