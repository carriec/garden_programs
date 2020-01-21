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
library(ggmap)
library(hablar)

#set Census API key, obtained at: http://api.census.gov/data/key_signup.html
###census_api_key("INSERT_KEY_HERE", install = TRUE)

########Importing Data########
#read in imported food/ag program data - states
Becca_States <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx")
Becca_States <- Becca_States[1:112,c(1:3,17, 21:26)]
Becca_States <- Becca_States %>%
  filter(State != "Hawaii (ORG)")
Addy_States <- read_excel("data/raw_data/Correctional_Facility_Contact_Tracking_Addy_States_SUMMER.xlsx")
Addy_States <- Addy_States[, c(1:3, 15:20)]
Josh_States <- read_excel("data/raw_data/Correctional_Facility_Contact_Tracking_Josh_States_SUMMER.xlsx")
Josh_States <- Josh_States[, c(1:3, 15:20)]
Carrie_States <- read_excel("data/raw_data/Correctional_Facility_Contact_Tracking_Carrie_States.xlsx")
Carrie_States <- Carrie_States[, c(7,4,30,42:48)] %>%
  convert(dbl(`Confirmed_Program`))
Azmal_States <- read_excel("data/raw_data/Correctional_Facility_Contact_Tracking_Azmal_States_SUMMER.xlsx")
Azmal_States <- Azmal_States[, c(1:3, 16:22)]
Azmal_States <- Azmal_States %>%
  filter(State %in% c("Florida", "Nevada"))
Evan_States <- read_excel("data/raw_data/Correctional_Facility_Contact_Tracking_Evan_States_SUMMER.xlsx")
Evan_States <- Evan_States[, c(1:3, 15:20)]

########Cleaning Data########
#rename columns to match
Becca_States <- Becca_States %>%
  rename(Crops = "Crops and silviculture", "Animal Agriculture" = "Animal agriculture",
        "Food Production" = "Food production", "Culinary Arts and Food Service" = "Culinary arts and food service")
Addy_States <- Addy_States %>%
  rename("Culinary Arts and Food Service" = "Culinary Arts", Crops = "Crops and Silviculture",
         "Confirmed Program" = "Confirmed Programs")
Josh_States <- Josh_States %>%
  rename(Crops = "Crops and Silviculture")
Carrie_States <- Carrie_States %>%
  rename(state = STATE, "Name of Correctional Facility" = "NAME", "Confirmed Program" = Confirmed_Program,
         Crops = "Crops_and_silviculture", "Animal Agriculture" = Animal_agriculture,
         "Food Production" = Food_production, "Culinary Arts and Food Service" = Culinary_arts_and_food_service)
Azmal_States <- Azmal_States %>%
  rename(Crops = "Crops and Silviculture", "Confirmed Program" = "Confirmed Programs",
         "Horticulture" = "Horitculture")
Evan_States <- Evan_States %>%
  rename(Crops = "Crops and silviculture", "Animal Agriculture" = "Animal agriculture",
         "Food Production" = "Food production", "Culinary Arts and Food Service" = "Culinary arts and food service",
         "Confirmed Program" = "Confirmed Programs")

#state fips codes
data(fips_codes)
f <- fips_codes %>%
  select(-county_code, -county) %>%
  distinct()

#merge all data sets (except Carrie's, Becca's and Azmal's) before converting State names to State abbreviations
All_Other_States <- rbind(Addy_States, Josh_States, Evan_States)

#add full state name to Carrie's data and add state abbreviations to all other data sets
All_Other_States <-
  All_Other_States %>%
  left_join(f, by = c("State" = "state_name")) %>%
  add_column(`Stated Purpose of Activity`=NA)

Azmal_States <-
  Azmal_States %>%
  left_join(f, by = c("State" = "state_name")) 

Becca_States <-
  Becca_States %>%
  left_join(f, by = c("State" = "state_name")) 
  
Carrie_States <-
  Carrie_States %>% 
  left_join(f, by =c("state" = "state")) %>%
  rename("State" = state_name)

#combine all states together and view unique states in order
All_States <- rbind(Becca_States, Carrie_States, Azmal_States, All_Other_States)
sort(unique(All_States$State))

########Analysis########
#What? Counts of types of programs and activities at state prisons

###Number of Prisons with Confirmed (Yes=1/No=0) and Unconfirmed (NA) Programs
view(All_States %>%
  select(State, `Confirmed Program`) %>%
  group_by(State, `Confirmed Program`) %>%
  summarise(prisons_tot = n()))

###List of Prisons with Confirmed Yes (Yes=1) Programs, excluding Culinary Arts and Food Service
view(All_States%>%
  filter(`Confirmed Program`==1 & 
           (!is.na(Horticulture) | !is.na(Crops) | !is.na(`Animal Agriculture`) | !is.na(`Food Production`) | !is.na(Other))
  ))

###Same list as above with unnecessary columns removed
view(All_States%>%
       select(-`Culinary Arts and Food Service`, -`state`, -`state_code`) %>%
       filter (`Confirmed Program`==1 &   
     (!is.na(Horticulture) | !is.na(Crops) | !is.na(`Animal Agriculture`) | !is.na(`Food Production`) | !is.na(Other))
       ))
         
###Program categories offered at each prison with list of subcategories
All_States_pivot <-
  All_States %>%
  pivot_longer(cols = `Horticulture`:`Other`, names_to = "Program Category", values_to = "Subcategory") %>%
  drop_na(Subcategory) %>%
  distinct()

###Number of prisons with activities in a category
All_States_count <-
  All_States_pivot %>%
  select(State, `Confirmed Program`, `Program Category`, Subcategory) %>%
  group_by(State, `Confirmed Program`,`Program Category`) %>%
  summarise(prisons_tot = n())

#######START HERE - LEFT OFF

####number of prisons with food & ag programs by state
q <- ggplot(data = All_States_pivot, aes(state, ..count..)) +
 facet_wrap(~ "Program Category")

q + labs(y = "Number of Prisons", x = "State", title = "Prisons with Food & Ag Programs")
####number of prisons with food & ag programs by subcategory and state
r <- ggplot(data = All_States_pivot, aes(state, ..count..)) +
  geom_bar(aes(fill = Type_temp)) +
  facet_wrap(~ Program)
r + labs(y = "Number of Prison Programs", x = "State", title = "Food & Ag Programs at Prisons",  subtitle = "By Category and Subcategory") +
  guides(fill = guide_legend(title="Subcategories"))



###Count of prisons offering programs in each category
All_States_program_count <-
  All_States_pivot %>%
  distinct("state", "Program Category") %>%
  summarise(count = n())


All_States_pivot_2 <-
  All_States_pivot %>%
  mutate()

Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp2 <-
  Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp %>%
  mutate(Type = tolower(Type_temp)) %>%
  left_join(f, by = c("State" = "state_name")) %>%
  select(-Type_temp)

type_addy <- subset(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp2$Type, !is.na(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp2$Type))
type_count_addy<-max(str_count(type_addy, ";"))+1

Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp3 <-
  Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp2 %>%
  separate(col = "Type", into = c(letters[1:type_count_addy]), sep = "; ") %>%
  drop_na(a) %>%
  pivot_longer(cols = 17:21, names_to = "Program_temp", values_to = "Type_temp") %>%
  drop_na(Type_temp) %>%
  select(-Program_temp)

Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_count <-
  subset(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp2, !is.na(Type)) %>%
  mutate(state = tolower(State)) %>%
  group_by(state) %>%
  count(Type)

###josh - other column distinct values
Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp <-
  Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state %>%
  separate(col = "Other? (Identify if so)", into = c("A","B","C","D","E"), sep = "; ") %>%
  pivot_longer(cols = 11:16, names_to = "Program_temp", values_to = "Type_temp")
  
Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp2 <-
  Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp %>%
  mutate(Program = (gsub("A|B|C|D|E", "Other? (Identify if so)", Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp$Program_temp))) %>%
  select(-Program_temp) %>%
  distinct()

###josh - count of 'Other' column distinct values
Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp3 <-
  Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp2 %>%
  mutate(Type = tolower(Type_temp)) %>%
  left_join(f, by = c("State" = "state_name")) %>%
  select(-Type_temp)

Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_count <-
  subset(Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp3, !is.na(Type)) %>%
  mutate(state = tolower(State)) %>%
  group_by(state) %>%
  count(Type)


#visualize data
###addy
#p <- ggplot(data = subset(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp2, !is.na(Type)), aes(state, ..count..)) +
#     geom_bar(aes(fill = Type))
#p
#q <- ggplot(data = Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp2, aes(state, ..count..)) +
#  geom_bar(aes(fill = Program))
#q
####number of prisons with food & ag programs by state
q2 <- ggplot(data = subset(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp2, !is.na(Type)), aes(state, ..count..)) +
  geom_bar(aes(fill = Program))
q2 + labs(y = "Number of Prisons", x = "State", title = "Prisons with Food & Ag Programs")
####number of prisons with food & ag programs by subcategory and state
q3 <- ggplot(data = Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp3, aes(state, ..count..)) +
  geom_bar(aes(fill = Type_temp)) +
  facet_wrap(~ Program)
q3 + labs(y = "Number of Prison Programs", x = "State", title = "Food & Ag Programs at Prisons",  subtitle = "By Category and Subcategory") +
  guides(fill = guide_legend(title="Subcategories"))

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

#ICPSR quick highlevel plots
a <- ggplot(data = da24642.0001, aes(V7, ..count..)) +
  geom_bar(aes(fill = V208))
a
b <- ggplot(data = da24642.0001, aes(V7, ..count..)) +
  geom_bar(aes(fill = V208)) +
  facet_wrap(~ V22)
b
c <- ggplot(data = da24642.0001, aes(V7, ..count..)) +
  geom_bar(aes(fill = V208)) +
  facet_wrap(~ V23)
c

##################MISCELLANEOUS EDA##################################
#Where? Add latitude and longitude
####Addy - remove this code later, not of use
#Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp <-
#  Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state %>%
#  unite("Name and State", c(`Name of Correctional Facility`, `State`), sep = ", ", remove = FALSE)

#latlon.addy.state <- geocode_OSM(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp$`Name and State`)

#Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp <-
#  Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp %>%
#  left_join(latlon.addy.state, by = c("Name and State" = "query"))

####Josh - remove this code later, not of use
#Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_latlon <-
#  Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state %>%
#  unite("Name and State", c(`Name of Correctional Facility`, `State`), sep = ", ", remove = FALSE)

#latlon.josh.state <- geocode_OSM(Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_latlon$`Name and State`)

#Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_latlon <-
#  Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_latlon %>%
#  left_join(latlon.josh.state, by = c("Name and State" = "query"))

####Maps of HIFLD Prison Boundaries data
file_js = FROM_GeoJson(url_file_string = "https://opendata.arcgis.com/datasets/2d6109d4127d458eaf0958e4c5296b67_0.geojson", Average_Coordinates = TRUE)
class(file_js)
file_js

hifld<-getURL("https://opendata.arcgis.com/datasets/2d6109d4127d458eaf0958e4c5296b67_0.geojson")
hifld <- st_read(hifld)
#class(hifld)
#head(hifld$geometry,1)
#st_bbox(hifld[2,])
#st_coordinates(hifld)
#st_geometry(hifld)[[1]][[1]]
#st_bbox(hifld[1])

####Min and max lat and lon coordinates from HIFLD data set

#hifld2 <- hifld %>%
#  mutate(xmin = st_bbox(hifld[ ,]))

#    print(row_bbox$xmin) })%>%
#  mutate(ymin = for (i in nrow(hifld)) {
#    row_bbox <- st_bbox(hifld[i,])
#    print(row_bbox$ymin) }) %>%
#  mutate(xmax = for (i in nrow(hifld)) {
#    row_bbox <- st_bbox(hifld[i,])
#    print(row_bbox$xmax) }) %>%
#  mutate(ymax = for (i in nrow(hifld)) {
#    row_bbox <- st_bbox(hifld[i,])
#    print(row_bbox$ymax) })

####Map of sites in HIFLD data set - need to check the projection types so that coordinates map correctly
ggplot(data=hifld) +
  geom_sf()

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
us_map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap()
us_map + geom_sf(data = hifld, inherit.aes = FALSE) +
  scale_fill_brewer(palette = "OrRd") +
  coord_sf(crs = st_crs(4326))

#read in imported food/ag program data - counties
#Becca_County <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx", sheet = 2, col_names = FALSE)
#Addy_County <- read_excel("data/raw_data/Correctional_Facility_Contact_Tracking_Addy_States_SUMMER.xlsx", sheet = 2)
#Addy_County <- Addy_County[1:356,]
#Josh_County <- read_excel("data/raw_data/Correctional_Facility_Contact_Tracking_Josh_States_SUMMER.xlsx", sheet = 2)

