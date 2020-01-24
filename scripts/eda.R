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
library(albersusa)
library(ggalt)
library(viridis)
library(ggthemes)


#set Census API key, obtained at: http://api.census.gov/data/key_signup.html
###census_api_key("INSERT_KEY_HERE", install = TRUE)

########Importing Data########
#read in imported food/ag program data - states
Becca_States <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx")
Becca_States <- Becca_States[1:112,c(1:3, 20:26)]
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
  rename(Crops = "Crops and Silviculture", "Confirmed Program" = "Confirmed Programs")
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

#add regions to data
region_state <- data.frame(region = state.region, state= state.abb)

All_States <- 
  All_States %>%
  left_join(region_state)

########Analysis########
#What? Counts of types of programs and activities at state prisons

###Total number of prisons by state with Confirmed (Yes=1/No=0) and Unconfirmed (NA) programs
view(All_States %>%
  group_by(State, `Confirmed Program`) %>%
  summarise(prisons_tot = n()))

###Write to CSV file
write_csv(All_States %>%
            group_by(State, `Confirmed Program`) %>%
            summarise(prisons_tot = n()),
          "./writing/eda_output/prisons_tot_state.csv",
          append=FALSE)

###List of Prisons with Confirmed Yes (Yes=1) Programs, excluding Culinary Arts and Food Service
All_States_Confirmed_Ag <-
  All_States %>%
  select(-`Culinary Arts and Food Service`) %>%
  filter(`Confirmed Program`==1 & 
           (!is.na(Horticulture) | !is.na(Crops) | !is.na(`Animal Agriculture`) | !is.na(`Food Production`) | !is.na(Other))
  )

###View Sum for US of Above List of Prisons with Confirmed Yes (Yes=1) Programs, excluding Culinary Arts and Food Service
view(All_States%>%
    select(-`Culinary Arts and Food Service`) %>%
    filter(`Confirmed Program`==1 & 
             (!is.na(Horticulture) | !is.na(Crops) | !is.na(`Animal Agriculture`) | !is.na(`Food Production`) | !is.na(Other))) %>%
    summarise(total_prisons=n()))
    
###Sum for States of Above List of Prisons with Confirmed Yes (Yes=1) Programs, excluding Culinary Arts and Food Service
All_States_Confirmed_Ag_count <- All_States%>%
       select(-`Culinary Arts and Food Service`) %>%
       filter(`Confirmed Program`==1 & 
                (!is.na(Horticulture) | !is.na(Crops) | !is.na(`Animal Agriculture`) | !is.na(`Food Production`) | !is.na(Other))) %>%
       group_by(State, state) %>%
       summarise(total_prisons=n())

###Write CSV
write_csv(
    All_States%>%
    select(-`Culinary Arts and Food Service`) %>%
    filter(`Confirmed Program`==1 & 
             (!is.na(Horticulture) | !is.na(Crops) | !is.na(`Animal Agriculture`) | !is.na(`Food Production`) | !is.na(Other))) %>%
    group_by(State, state) %>%
    summarise(total_prisons=n()),
    "./writing/eda_output/prisons_tot_state_confirmed_ag.csv",
)
         
###List of Prisons + Program categories and subcategories with Confirmed Programs (Yes=1)
All_States_pivot <-
  All_States %>%
  select(-`Culinary Arts and Food Service`) %>%
  filter(`Confirmed Program`==1) %>%
  pivot_longer(cols = c(`Horticulture`,`Crops`,`Animal Agriculture`,`Food Production`,`Other`) , names_to = "Program Category", values_to = "Subcategory") %>%
  drop_na(Subcategory)

###Number of prisons by state with confirmed (Yes=1) activities in a category, excluding Culinary Arts and Food Service
All_States_count <-
  All_States_pivot %>%
  group_by(State, state, region, `Confirmed Program`,`Program Category`) %>%
  summarise(prisons_tot = n())

#Write to CSV
write_csv(All_States_pivot %>%
            group_by(State, state, region, `Confirmed Program`,`Program Category`) %>%
            summarise(prisons_tot = n()),
          "./writing/eda_output/prisons_tot_confirmed_ag_state_category.csv",
          append=FALSE)

###Number of prisons by region with confirmed (Yes=1) activities in a category, excluding Culinary Arts and Food Service
All_Regions_count <-
  All_States_pivot %>%
  group_by(region, `Confirmed Program`,`Program Category`) %>%
  summarise(prisons_tot = n())

#Write to CSV
write_csv(All_States_pivot %>%
            group_by(region, `Confirmed Program`,`Program Category`) %>%
            summarise(prisons_tot = n()),
          "./writing/eda_output/prisons_tot_confirmed_ag_region_category.csv",
          append=FALSE)

###Number of prisons in US with confirmed (Yes=1) activities in a category, excluding Culinary Arts and Food Service
All_US_count <-
  All_States_pivot %>%
  group_by(`Confirmed Program`,`Program Category`) %>%
  summarise(prisons_tot = n())

#Write to CSV
write_csv(All_States_pivot %>%
          group_by(`Confirmed Program`,`Program Category`) %>%
          summarise(prisons_tot = n()),
          "./writing/eda_output/prisons_tot_confirmed_ag_us_category.csv",
          append=FALSE)

###Bar plot: number of prisons with activities in a category, excluding Culinary Arts and Food Service
#c <- ggplot(data=All_States_count, aes(x=state, y=prisons_tot))+
#  geom_col(aes(fill=`Program Category`))

#Question 1: What are the most common types of activities? 
#Solution: Number of prisons with confirmed activities in a subcategory, excluding Culinary Arts and Food Service

###Calculate the max number of subcategories within a category at a prison
max_col <- max(str_count(All_States_pivot$Subcategory, ";")+1)

###Remove spaces after semi-colons
All_States_pivot$Subcategory <-
  gsub(";\\s+",";", All_States_pivot$Subcategory)

###Split confirmed (Yes=1) subcategories into multiple columns, use semi-colon to parse
All_States_subcat <-
  All_States_pivot %>%
  separate(col=Subcategory, into=c(paste("Sub",1:max_col, sep = "_")), sep=";")

###Pivot subcategory columns into rows
All_States_subcat_pivot <-
  All_States_subcat %>%
  select(-`Stated Purpose of Activity`) %>%
  pivot_longer(cols=c(paste("Sub",1:max_col, sep = "_")), names_to = "Temp", values_to = "Subcategory") %>%
  select(-Temp) %>%
  drop_na("Subcategory")

###Number of prisons by state with confirmed activities within category and subcategory, excluding Culinary Arts and Food Service
All_States_subcat_count <-
  All_States_subcat_pivot %>%
  mutate(Subcategory = str_trim(tolower(All_States_subcat_pivot$Subcategory))) %>%
  group_by(state, region, `Confirmed Program`,`Program Category`, Subcategory) %>%
  summarise(prisons_tot = n())

###Write to CSV
write_csv( All_States_subcat_pivot %>%
             mutate(Subcategory = str_trim(tolower(All_States_subcat_pivot$Subcategory))) %>%
             group_by(state, region, `Confirmed Program`,`Program Category`, Subcategory) %>%
             summarise(prisons_tot = n()),
           "./writing/eda_output/prisons_tot_confirmed_ag_state_subcategory.csv",
           append=FALSE)

###Number of prisons by region with confirmed activities within category and subcategory, excluding Culinary Arts and Food Service
All_Regions_subcat_count <-
  All_States_subcat_pivot %>%
  mutate(Subcategory = str_trim(tolower(All_States_subcat_pivot$Subcategory))) %>%
  group_by(region, `Confirmed Program`,`Program Category`, Subcategory) %>%
  summarise(prisons_tot = n())

###Write to CSV
write_csv( All_States_subcat_pivot %>%
             mutate(Subcategory = str_trim(tolower(All_States_subcat_pivot$Subcategory))) %>%
             group_by(region, `Confirmed Program`,`Program Category`, Subcategory) %>%
             summarise(prisons_tot = n()),
           "./writing/eda_output/prisons_tot_confirmed_ag_region_subcategory.csv",
           append=FALSE)

###Final calculation: The most common types of confirmed activities nationwide by subcategory and category.
All_US_subcat_count <-
  All_States_subcat_pivot %>%
  mutate(Subcategory = str_trim(tolower(All_States_subcat_pivot$Subcategory))) %>%
  group_by(`Program Category`, Subcategory) %>%
  summarise(prisons_tot = n()) %>%
  arrange(desc(prisons_tot))

###Write CSV
write_csv(
    All_States_subcat_pivot %>%
    mutate(Subcategory = str_trim(tolower(All_States_subcat_pivot$Subcategory))) %>%
    group_by(`Program Category`, Subcategory) %>%
    summarise(prisons_tot = n()) %>%
    arrange(desc(prisons_tot)),
    "./writing/eda_output/prisons_tot_confirmed_ag_us_subcategory.csv",
    append=FALSE)

###Uninformative plot of all subcategories and states
#c_subcat <- ggplot(data=All_States_subcat_count, aes(x=state, y=prisons_tot))+
#  geom_col(aes(fill=`Subcategory`))

###Uninformative plot of all states and subcategories
#c2_subcat <- ggplot(data=All_States_subcat_count, aes(x=`Subcategory`, y=prisons_tot))+
#  geom_col(aes(fill=`state`))

#Question 2: Where are things taking place around the country?
#Solution: 50 state map of total activities across all categories: Number of prisons with confirmed activities.

###Map of 50 states using albersusa package

plot(usa_composite(proj="laea"))

us <- usa_composite()
us_map <- fortify(us, region="name")

gg <- ggplot()
gg <- gg + geom_map(data=us_map, map=us_map,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.1, fill=NA)
gg <- gg + theme_map()

##playing around
#us_sf <- usa_sf("laea")
#plot(us_sf["pop_2014"])
#ggsf <- ggplot()
#ggsf <- ggsf + geom_sf(data=us_sf,
#                       size=0.1)
#ggsf <- ggsf + theme_map()

###Prisons with Activities by State 
gg_all <- gg +
  geom_map(data=All_States_Confirmed_Ag_count, map=us_map,
           aes(fill=total_prisons, map_id=State),
           color="white", size=0.1) +
  coord_proj(us_laea_proj) +
  scale_fill_viridis(name="Prisons with Confirmed Ag Activities") +
  theme(legend.position="right")

ggsave("map_prisons_conf_ag.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Question 2a: Are there certain parts of the country with certain activities more so than others? 
#Solution: 50 state map of each category: Number of prisons with confirmed activities.

###Prisons with Horticulture Activities by State
All_States_count_hort <-
  All_States_count %>%
  filter(`Program Category`== "Horticulture")

###Map of prisons with Horticulture Activities
gg_hort <- gg + 
  geom_map(data=All_States_count_hort, map=us_map,
           aes(fill=prisons_tot, map_id=State),
           color="white", size=0.1) +
  coord_proj(us_laea_proj) +
  scale_fill_viridis(name="Prisons with Horticulture") +
  theme(legend.position="right")

ggsave("map_prisons_conf_hort.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

###Prisons with Crops and Silviculture by State
All_States_count_crops <-
  All_States_count %>%
  filter(`Program Category`== "Crops")

###Map of Prisons with Crops and Silviculture
gg_crops <- gg + 
  geom_map(data=All_States_count_crops, map=us_map,
           aes(fill=prisons_tot, map_id=State),
           color="white", size=0.1) +
  coord_proj(us_laea_proj) +
  scale_fill_viridis(name="Prisons with Crops and Silviculture") +
  theme(legend.position="right")

ggsave("map_prisons_conf_crops.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

###Prisons with Animal Ag by State
All_States_count_animals <-
  All_States_count %>%
  filter(`Program Category`== "Animal Agriculture")

###Map of Prisons with Animal Ag
gg_animals <- gg + 
  geom_map(data=All_States_count_animals, map=us_map,
           aes(fill=prisons_tot, map_id=State),
           color="white", size=0.1) +
  coord_proj(us_laea_proj) +
  scale_fill_viridis(name="Prisons with Animal Agriculture") +
  theme(legend.position="right")

ggsave("map_prisons_conf_animals.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

###Prisons with Food Production & Processing by State
All_States_count_food <-
  All_States_count %>%
  filter(`Program Category`== "Food Production")

###Map of Prisons with Food Production & Processing by State
gg_food <- gg + 
  geom_map(data=All_States_count_food, map=us_map,
           aes(fill=prisons_tot, map_id=State),
           color="white", size=0.1) +
  coord_proj(us_laea_proj) +
  scale_fill_viridis(name="Prisons with Food Production & Processing") +
  theme(legend.position="right")

ggsave("map_prisons_conf_food.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Question 2b. What are the regions?

###Look at regions
view(data.frame(region = state.region, state= state.abb))

###Plot program categories broken down by region
region1 <- ggplot(data=All_Regions_count, aes(x=region, y=prisons_tot)) +
  geom_col(aes(fill=`Program Category`)) + 
  labs(y = "Number of Prisons", x = "Region", title = "Prisons with Ag Programs")

ggsave("plot_ag_region1.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

###Plot regions broken down by program categories
region2 <- ggplot(data=All_Regions_count, aes(x=`Program Category`, y=prisons_tot)) +
  geom_col(aes(fill=`region`)) +
  labs(y= "Number of Prisons", x = "Program Category", fill = "Region", title = "Prisons with Ag Programs")

ggsave("plot_ag_region2.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Question 3. Why? What are the purposes of the programs?

###Subset of all states  confirmed ag data with purpose not null
All_States_purpose <-
  All_States_Confirmed_Ag %>%
  filter(!is.na(`Stated Purpose of Activity`))

###Calculate the max number of purposes at a prison
max_col_purpose <- max(str_count(All_States_purpose$`Stated Purpose of Activity`, ";")+1)

###Remove spaces after semi-colons
All_States_purpose$`Stated Purpose of Activity` <-
  gsub(";\\s+",";", All_States_purpose$`Stated Purpose of Activity`)

###Separate data so that each purpose at a facility has one column
All_States_purpose_separate <-
  All_States_purpose %>%
  separate(col=`Stated Purpose of Activity`, 
           into=c(paste("Purpose",1:max_col_purpose, sep = "_")), sep=";")

###Pivot data so that each purpose at a facility has one row
All_States_purpose_pivot <-
  All_States_purpose_separate %>%
  pivot_longer(cols=c(paste("Purpose",1:max_col_purpose, sep = "_")), names_to = "Temp", values_to = "Purpose") %>%
  select(-Temp) %>%
  drop_na("Purpose")

###Remove any blank entries
All_States_purpose_pivot <-
   All_States_purpose_pivot %>%
  mutate(Purpose = str_trim(tolower(Purpose), side = "both")) %>%
    filter(Purpose != "")

###Plot purposes broken down by region
region3 <- ggplot(data=All_States_purpose_pivot, aes(Purpose, ..count..)) +
  geom_bar(aes(fill=region)) +
  labs(y = "Number of Prisons", x = "Purpose", fill = "Region", title = "Purpose of Ag Programs") +
  coord_flip()

ggsave("plot_ag_region3.png", plot = last_plot(), device = "png", path = "./writing/eda_output/")

###Plot regions broken down by purposes
region4 <- ggplot(data=All_States_purpose_pivot, aes(region, ..count..)) +
  geom_bar(aes(fill=Purpose)) +
  labs(y = "Number of Prisons", x = "Region", title = "Purpose of Ag Programs")

ggsave("plot_ag_region4.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

###This won't work for a map because there are too many ties.
view(
  All_States_purpose_pivot %>%
  group_by(state, Purpose) %>%
  summarise(prisons_tot = n()) %>%
  mutate(rank = rank(prisons_tot, ties="average"))
)







#######LEFT OFF HERE
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

v2015 <- load_variables(2015, dataset = "acs5", cache = TRUE)
View(v2015)
v <- load_variables(2010, dataset = "sf1", cache = TRUE)
View(v)

# total population by state 2010 census - shift location for Alaska/Hawaii (see https://map-rfun.library.duke.edu/02_choropleth.html#alaska__hawaii_-_shift)
m <- get_decennial(geography = "state", variables = "P001001", geometry = TRUE, shift_geo = TRUE) 

ggplot(m, aes(fill = value, color = value)) +
  geom_sf() +
  coord_sf(crs = us_laea_proj)

###slightly different map projection
#ggplot(m, aes(fill = value, color = value)) +
#  geom_sf() +
#  coord_sf(crs = 26914)

###Map of 2014 census data using albersusa package functions
#us_sf <- usa_sf("laea")

#ggplot(us_sf, aes(fill=pop_2014, color=pop_2014)) +
#  geom_sf()
   
###Note: Think about joining these data sets to map census data using albersusa functions.

v18 <- load_variables(2018, "acs5", cache = TRUE)    
#Population by region 2018 5-year ACS
region_pop <- get_acs(geography="region",
                  variables = "B19013_001")

ggplot(region_pop, aes(y=estimate, x=NAME)) +
  geom_col(aes(fill=NAME))

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

###Maps of HIFLD Prison Boundaries data
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

###Mapping U.S. states and counties alternative: see https://socviz.co/maps.html

#library(maptools)
#library(mapproj)
#library(rgeos)
#library(rgdal)

us_states <- map_data("state")
head(us_states)

p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group))

p + geom_polygon(fill = "white", color = "black")

#Map the fill aesthetic to region and change the color mapping to a light gray and thin the lines to make the state borders a little nicer. Do not plot a legend.

p <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)

#Transform the default projection used by geom_polygon(), via the coord_map() function. The Albers projection requires two latitude parameters, lat0 and lat1. We give them their conventional values for a US map here.
p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)

#Merge our horticulture data with that data frame to get our data on the  map. Use left_join to merge.
#All_States_count$region <- tolower(All_States_count$State)
#view(us_states %>%
#       filter(region=="alaska"))

All_States_count_hort <-
  All_States_count %>%
  filter(`Program Category`== "Horticulture")
#All_States_count_hort <- left_join(us_states, All_States_count_hort)

#Plot our horticulture data in a map.
p <- ggplot(data = All_States_count_hort,
            aes(x = long, y = lat,
                group = group, fill = prisons_tot))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

#Make the map prettier.
p0 <- ggplot(data = All_States_count_hort,
             mapping = aes(x = long, y = lat,
                           group = group, fill = prisons_tot))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_equal()

coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p2 <- p1  + 
  scale_fill_gradient(low="#E5F5F9",high="#2CA25F") +
  labs(title = "State Prisons with Horticulture", fill = NULL)

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

#map U.S. counties
us_counties <- readOGR(dsn="data/geojson/gz_2010_us_050_00_5m.json",
                       layer="OGRGeoJSON")

us_counties_aea <- spTransform(us_counties,
                               CRS("+proj=laea +lat_0=45 +lon_0=-100 \
                         +x_0=0 +y_0=0 +a=6370997 +b=6370997 \
                         +units=m +no_defs"))

us_counties_aea@data$id <- rownames(us_counties_aea@data)

#relocate alaska and hawaii
alaska <- us_counties_aea[us_counties_aea$STATE == "02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_counties_aea)

hawaii <- us_counties_aea[us_counties_aea$STATE=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_counties_aea)

us_counties_aea <- us_counties_aea[!us_counties_aea$STATE %in% c("02", "15", "72"),]
us_counties_aea <- rbind(us_counties_aea, alaska, hawaii)

#tidy the spatial object into a data frame that ggplot can use, and clean up the id label by stripping out a prefix from the string.

county_map <- tidy(us_counties_aea, region = "GEO_ID")
county_map$id <- stringr::str_replace(county_map$id,
                                      pattern = "0500000US", replacement = "")

#county_map object is ready to be merged with a table of FIPS-coded US county data using either merge() or left_join().