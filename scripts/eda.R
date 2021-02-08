#exploratory data analysis

#library
library(readxl) ## Read imported excel files
library(hablar) ## Convert data types
library(tigris) ## Get fips codes for Census geographic classifications
options(tigris_use_cache = TRUE)
library(tidyverse) ## For tidying data 
library(RCurl) ## For downloading URLs
library(sf) ## For reading simple features (sf) from a file / st_read function
library(naniar) ## For replace_with_na function
library(RecordLinkage) ## For linking records between multiple data sets
library(albersusa) ## For plotting US map with Alaska/Hawaii elided
library(viridis) ## For map color palette
library(scales) ## For dot plots

library(tidycensus)
library(maps)
library(geojsonR)


library(tmaptools)
library(ggmap)


library(ggalt)

library(ggthemes)


library(forcats)
library(readr)
library(RColorBrewer)


#set Census API key, obtained at: http://api.census.gov/data/key_signup.html
###census_api_key("INSERT_KEY_HERE", install = TRUE)

########Importing Prison Food/Ag Activity Data########

#read in imported food/ag program data - states
Becca_States <- read_excel("./data/raw_data/2020-10-06/Correctional_Facility_Hort_Programs_Becca.xlsx")
Becca_States <- Becca_States[1:179,c(1:3, 20:25,27)]
Addy_States <- read_excel("data/raw_data/2020-10-06/Correctional_Facility_Contact_Tracking_Addy_States_SUMMER.xlsx")
Addy_States <- Addy_States[, c(1:3, 15:21)]
Josh_States <- read_excel("data/raw_data/2020-10-06/Correctional_Facility_Contact_Tracking_Josh_States_NEWEST.xlsx")
Josh_States <- Josh_States[, c(1:3, 15:21)]
Carrie_States <- read_excel("data/raw_data/2020-10-06/Correctional_Facility_Contact_Tracking_Carrie_States.xlsx")
Carrie_States <- Carrie_States[, c(7,4,30,42:48)] %>%
  convert(dbl(`Confirmed_Program`))
Azmal_States <- read_excel("data/raw_data/2020-10-06/Correctional_Facility_Contact_Tracking_Azmal_States_SUMMER.xlsx")
Azmal_States <- Azmal_States[, c(1:3, 16:20, 22:23)]
Evan_States <- read_excel("data/raw_data/2020-10-06/Correctional_Facility_Contact_Tracking_Evan_States_SUMMER.xlsx")
Evan_States <- Evan_States[, c(1:3, 15:21)]

########Cleaning Data########

#rename columns to match
Becca_States <- Becca_States %>%
  rename(
    "Crops & Silviculture" = "Crops and silviculture", 
   "Horticulture & Landscaping" = "Horticulture",
     "Animal Agriculture" = "Animal agriculture",
    "Food Processing & Production" = "Food production", "Culinary Arts & Food Service" = "Culinary arts and food service",
        "Stated Purpose of Activity" = "Stated Purpose of Activity (Ag/Horticulture)")
Addy_States <- Addy_States %>%
  rename("Culinary Arts & Food Service" = "Culinary Arts", 
         "Crops & Silviculture" = "Crops and Silviculture",
         "Horticulture & Landscaping" = "Horticulture",
         "Food Processing & Production" = "Food Production",
         "Confirmed Program" = "Confirmed Programs")
Josh_States <- Josh_States %>%
  rename("Food Processing & Production" = "Food Production",
   "Crops & Silviculture" = "Crops and Silviculture",
   "Horticulture & Landscaping" = "Horticulture",
   "Culinary Arts & Food Service" = "Culinary Arts and Food Service"
    )
Carrie_States <- Carrie_States %>%
  rename(state = STATE, "Name of Correctional Facility" = "NAME", "Confirmed Program" = Confirmed_Program,
        "Crops & Silviculture" = "Crops_and_silviculture", 
        "Horticulture & Landscaping" = "Horticulture",
        "Animal Agriculture" = Animal_agriculture,
         "Food Processing & Production" = Food_production, "Culinary Arts & Food Service" = Culinary_arts_and_food_service)
Azmal_States <- Azmal_States %>%
  rename("Food Processing & Production" = "Food Production",
    "Crops & Silviculture" = "Crops and Silviculture", 
    "Horticulture & Landscaping" = "Horticulture",
    "Culinary Arts & Food Service" = "Culinary Arts and Food Service",
    "Confirmed Program" = "Confirmed Programs")
Evan_States <- Evan_States %>%
  rename(
    "Crops & Silviculture" = "Crops and silviculture", 
    "Animal Agriculture" = "Animal agriculture",
    "Horticulture & Landscaping" = "Horticulture",
         "Food Processing & Production" = "Food production", "Culinary Arts & Food Service" = "Culinary arts and food service",
         "Confirmed Program" = "Confirmed Programs")

#state fips codes

data(fips_codes)
f <- fips_codes %>%
  select(-county_code, -county) %>%
  distinct()

#merge all data sets (except Carrie's) before converting State names to State abbreviations
All_Other_States <-rbind(Addy_States, Azmal_States, Becca_States, Josh_States, Evan_States)

#add full state name to Carrie's data and add state abbreviations to all other data sets
All_Other_States <-
  All_Other_States %>%
  left_join(f, by = c("State" = "state_name"))
  
Carrie_States <-
  Carrie_States %>% 
  left_join(f, by =c("state" = "state")) %>%
  rename("State" = state_name)

#combine all states together and view unique states in order
All_States <- rbind(Carrie_States, All_Other_States)
sort(unique(All_States$State))

#add regions to data
region_state <- data.frame(region = state.region, division = state.division, state= state.abb)

region_state$region <- str_replace(string = region_state$region, pattern = "North Central", replacement = "Midwest" )

All_States <- 
  All_States %>%
  left_join(region_state)

#Add rownames as a column to All States data
#All_States <- All_States %>%
#  rownames_to_column(var = "row_num")

#Convert Confirmed Program to numeric
All_States <- All_States %>%
  convert(num(`Confirmed Program`))

#remove unneeded data
rm(Addy_States, All_Other_States, Azmal_States, Becca_States, Carrie_States,
            Evan_States, f, fips_codes, Josh_States)
rm(region_state)

########Join data set with February 2020 HIFLD IDs using previously created key and add ID.PrisonAg for new row entries########
#(see: https://rpubs.com/ahmademad/RecordLinkage), (see: "scripts/Archive/eda.R" and "writing/eda_output_internal/Archive/2020-02-15/PrisonAg_HIFLD_ID_key.csv")

#Import Assigned Prison Ag dataset ID and HIFLD Facility ID from original data record linkage in February 2020
All_States_finalresults_202002 <- readRDS(file = "writing/eda_output_internal/Archive/2020-02-15/All_States_finalresults.Rds")
All_States_finalresults_202002 <- All_States_finalresults_202002 %>%
  select(ID.PrisonAg, state, NAME.Prison_Ag)

#Import the key between Prison Ag and HIFLD IDs from February 2020
PrisonAg_HIFLD_ID_key_202002 <- read_csv("./writing/eda_output_internal/Archive/2020-02-15/PrisonAg_HIFLD_ID_key.csv")

#Remove OBJECT ID field (no longer appears in HIFLD data set as of September 2020)
PrisonAg_HIFLD_ID_key_202002 <- PrisonAg_HIFLD_ID_key_202002 %>%
  select(-OBJECTID, -FID)

#Join final results to key  
All_States_finalresults_202002_key_join <- All_States_finalresults_202002 %>%
  convert(num(ID.PrisonAg)) %>%
  left_join(PrisonAg_HIFLD_ID_key_202002, by = "ID.PrisonAg")

#Join id based on the state and correctional facility
All_States_join <- All_States %>%
  mutate(NAME.Prison_Ag = toupper(`Name of Correctional Facility`)) %>%
  full_join((All_States_finalresults_202002_key_join %>%
            mutate(NAME.Prison_Ag = toupper(NAME.Prison_Ag)))
            , by = c("state", "NAME.Prison_Ag")) 

###Remove facilities that are closed or privately owned
#Remove ID.PrisonAg = 509 from joined data set - "Crowley County Correctional Facility" -- it is privately owned and should not have been included in February 2020 data
#Remove ID.PrisonAg = 444 from joined data set - "New Castle Facility" (IN) - privately owned
#Remove ID.PrisonAg = 439 from joined data set - Heritage Trail Correctional Facility" (IN) -  privately owned
#Remove ID.PrisonAg = 623 from joined data set - "Lanesboro Correctional Institution" (NC) -- facility moved inmates and reopened as all female facility "Anson Correctional Institution - Female"
#Remove ID.PrisonAg = 384 from joined data set - "Enfield Correctional Institution" (CT) -- facility closed January 23, 2018
#Remove ID.PrisonAg = 18 from joined data set - "CHESAPEAKE DETENTION FACILITY" (MD) -- facility appears to be contracted as federal detention facility
#Remove ID.PrisonAg = 245 from joined data set - "Pensacola Work Release Center" (FL) -- facility does not exist; updated data set to include "Pensacola Community Release Center" as a new entry 
#Remove ID.PrisonAg = 246 from joined data set - "Panama City Work Release Center" (FL) -- facility does not exist; updated data set to include "Panama City Community Release Center" as a new entry 
#Remove ID.PrisonAg = 244 from joined data set - "Tallahassee Work Release Center" (FL) -- facility does not exist; updated data set to include "Tallahassee Community Release Center" as a new entry 
#Remove ID.PrisonAg = 136 from joined data set - "Bernadette Building" (RI) -- facility is technically open but has not housed inmates since 2016

All_States_join <- All_States_join %>%
  filter(is.na(ID.PrisonAg) | ID.PrisonAg != 509) %>%
  filter(is.na(ID.PrisonAg) | ID.PrisonAg != 444) %>%
  filter(is.na(ID.PrisonAg) | ID.PrisonAg != 439) %>%
  filter(is.na(ID.PrisonAg) | ID.PrisonAg != 623) %>%
  filter(is.na(ID.PrisonAg) | ID.PrisonAg != 384) %>%
  filter(is.na(ID.PrisonAg) | ID.PrisonAg != 18) %>%
  filter(is.na(ID.PrisonAg) | ID.PrisonAg != 245) %>%
  filter(is.na(ID.PrisonAg) | ID.PrisonAg != 246) %>%
  filter(is.na(ID.PrisonAg) | ID.PrisonAg != 244) %>%
  filter(is.na(ID.PrisonAg) | ID.PrisonAg != 136)

##Check missing joins between Prison Ag and HIFLD from February 2020 datasets and correct
view(All_States_join %>%
  drop_na(ID.PrisonAg) %>%
    select(`Name of Correctional Facility`, ID.PrisonAg, FACILITYID))

#Load original HIFLD from February 2020 and search for facilities identified above
hifld_no_sf_20200215 <- readRDS(file = "data/hifld_data/2020-02-15/hifld_no_sf.Rds")

###Make updates based on identified FACILITYID
#Add Facility ID to Women's Therapeutic Reentry Center in TN, which we have now identified as located at the West Tennessee State Penitentiary
#and to Northern Correctional Facility in WV which we have now identified as 10001874	NORTHERN REGIONAL JAIL / CORRECTIONAL FACILITY
#and SCI Phoenix in PA sits on the grounds (replaces) 10003101	SCI GRATERFORD
#and Arrendale Probation RSAT and Arrendale TC in GA sit on the grounds of 10002102	ARRENDALE STATE PRISON
#and Northwest Probation RSAT in GA sits on the grounds of 10000283	WALKER STATE PRISON
#and Southern Maine Women's Reentry Center in ME sits on the grounds of 10001165	MAINE CORRECTIONAL CENTER
#no further joins identified - Metro Reentry Facility only reopened in 2018 and did not appear in February 2020 HIFLD data
All_States_join <- All_States_join %>%
  mutate(FACILITYID = case_when
         (ID.PrisonAg == 161 ~ 10001711,
          ID.PrisonAg == 721 ~ 10001874,
          ID.PrisonAg == 898 ~ 10003101,
          ID.PrisonAg == 523 ~ 10002102,
          ID.PrisonAg == 525 ~ 10002102,
          ID.PrisonAg == 563 ~ 10000283,
          ID.PrisonAg == 730 ~ 10001165,
           TRUE ~ FACILITYID))

#Add ID.PrisonAg for Wisconsin Resource Center to match May 26, 2020 assignment of ID.PrisonAg = 998
All_States_join <- All_States_join %>%
  mutate(ID.PrisonAg = case_when
         (`Name of Correctional Facility` == "Wisconsin Resource Center" ~ 998,
           TRUE ~ ID.PrisonAg))

##Add ID.PrisonAg for all other new entries since February 2020
#First, arrange rows by ID.Prison Ag
All_States_join <- All_States_join %>%
  arrange(ID.PrisonAg)

#Then, add sequential new IDs
All_States_join[is.na(All_States_join$ID.PrisonAg), "ID.PrisonAg"] <- seq(max(All_States_join$ID.PrisonAg, na.rm = TRUE) + 1, max(All_States_join$ID.PrisonAg, na.rm = TRUE) + sum(is.na(All_States_join$ID.PrisonAg)))

#Assign newly added Northern Regional Jail (WV) to same FACILIITYID as Northern Correctional Facility (WV) - combined entry in HIFLD
All_States_join <- All_States_join %>%
  mutate(FACILITYID = case_when
         (ID.PrisonAg == 1090 ~ 10001874,
           TRUE ~ FACILITYID))

########Using HIFLD Data (September 2020)########

#Load
hifld.no_sf <- readRDS(file = "data/hifld_data/2020-09-28/hifld_no_sf.Rds")

###################Adding new record linkage####################3

#Part I: Perform record linkage between September 2020 HIFLD data and our collected data without FACILITYIDs
#(see: https://rpubs.com/ahmademad/RecordLinkage)

###To make joining easier (with less data entries), create subset of HIFLD data filtered for likely facilities 
#Select open adult (not juvenile) state & county facilities (some state facilities classified in HIFLD as county), excluding Puerto Rico and Washington, D.C. Add row numbers as column.
hifld.filter <- hifld.no_sf %>%
  filter(STATUS %in% c("OPEN","NOT AVAILABLE") & TYPE %in% c("COUNTY","STATE","MULTI","NOT AVAILABLE") & SECURELVL %in% c("CLOSE","MAXIMUM","MEDIUM","MINIMUM","NOT AVAILABLE"))

hifld.filter <- hifld.filter %>%
  filter(! STATE %in% c("DC","PR")) 

###Now filter out FACILITY IDs from hifld.filter that are not already assigned to Prison Ag (in All_States_join) 
hifld.filter <- hifld.filter %>%
  convert(int(FACILITYID)) %>%
  anti_join(All_States_join, by = "FACILITYID") %>%
  rownames_to_column("id1")

###Preprocessing: Select common fields and, in our data, convert name to upper case and trim
hifld.rl <- hifld.filter %>%
  select(id1, FACILITYID, NAME, STATE) %>%
  mutate(NAME = str_replace_all(NAME, " & ", " AND ")) %>%
  mutate(NAME = str_squish(str_replace_all(str_trim(NAME), "[^'[:^punct:]+]", " "))) %>%
  mutate(NAME = str_replace_all(NAME, " AND ", " & "))

All_States.rl <- All_States_join %>%
  filter(is.na(FACILITYID)) %>%
  select(ID.PrisonAg, "Name of Correctional Facility", "state") %>%
  mutate(`Name of Correctional Facility` = str_replace_all(`Name of Correctional Facility`, " & ", " AND ")) %>%
  mutate(`Name of Correctional Facility` = str_squish(str_replace_all(str_trim(toupper(`Name of Correctional Facility`)), "[^'[:^punct:]+]", " "))) %>%
  mutate(`Name of Correctional Facility` = str_replace_all(`Name of Correctional Facility`, " AND ", " & ")) %>%
  rename(NAME = `Name of Correctional Facility`) %>%
  rename(STATE = state) %>%
  rownames_to_column("id2")

#Create pairs from linking two data sets with cutoff between 0 and 1
a <- compare.linkage(hifld.rl, All_States.rl, blockfld = c("STATE"), strcmp=T, exclude =(1:2))
#print(head(a$pairs))
b <- emWeights(a, cutoff = 0.95)
#summary(b)
#head(b)
#allPairs <- getPairs(b)
#head(allPairs)
#write_csv(allPairs, path = "./writing/eda_output/allPairs.csv",
#          append=FALSE)

#Look at weights of pairs
weights <- as.data.frame(b$Wdata)
weights <- weights %>%
  rename(Wdata = `b$Wdata`)
#ggplot(weights, mapping=aes(Wdata)) +
#  geom_histogram(binwidth = .01)
#summary(weights$Wdata)
table(weights$Wdata)

#Retrieve the weight value that will serve as the upper threshold for Links (L)     
upper <- max(weights$Wdata)

#Set thresholds for predictions of Link (L), Possible (P), and Not a link (N)
#Thresholds based on table weights, and iterative review of the Linkage.join data below
c <- emClassify(b, threshold.lower = 0, threshold.upper = upper)
#summary(c)
linkage <- c$pairs
linkage$weight <- c$Wdata
linkage$links <- c$prediction
linkage$id1 <- as.character(linkage$id1)
linkage$id2 <- as.character(linkage$id2)

rm(a, b, c)
rm(weights)

#Convert All_States_rl for join with final linkage results
All_States.rl <- as.data.frame(All_States.rl)

#Results of HIFLD and All States rl subsets joined using linkage key
linkage.join <- linkage %>%
  left_join(hifld.rl, by = "id1") %>%
  left_join(All_States.rl, by = "id2")

#Filtered for link (L) rows
linkage.join_L <- linkage.join %>%
  filter(links == "L")

#Filtered for possible link (P) rows
linkage.join_P <- linkage.join %>%
  filter(links == "P") %>%
  anti_join(linkage.join_L, by = "id1") %>%
  anti_join(linkage.join_L, by = "id2")

#Verify manually that all the links = P rows are actually links
view(linkage.join_P)

####
#Manually verified that all entries above are actually links; remove any that are not links
#linkage.join_P <- linkage.join_P %>%
#  filter(!id1 == "") 

#now joining with L rows
linkage.join_LP <- bind_rows(linkage.join_L, linkage.join_P)

#Examine the facilities that did not have matches
linkage.join_N <- linkage.join %>%
  filter(links == "N") %>%
  anti_join(linkage.join_LP, by = "id1") %>%
  anti_join(linkage.join_LP, by = "id2")

#Examine the top match of facilities from All States data with linkage N    
linkage.join_N_top_1 <- linkage.join_N %>%
  group_by(id2) %>%
  top_n(n = 1, wt = NAME.x)

view(linkage.join_N_top_1)

#Filter out non-matches
linkage.join_N_matches <- linkage.join_N_top_1 %>%
  filter(!NAME %in% c(    "NORTH ALABAMA COMMUNITY BASED FACILITY COMMUNITY WORK CENTER",
                          "MURPHYSBORO LIFE SKILLS RE ENTRY CENTER",
                          "MADISON COUNTY COMMUNITY WORK CENTER",
                          "DELTA CORRECTIONAL FACILITY",
                          "NORTHEAST NEW MEXICO CORRECTIONAL FACILITY",
                          "WERNERSVILLE CCC 30",
                          "WERNERSVILLE CCC 18",
                          "DEERFIELD MEN'S WORK CENTER 2",
                          "NORTHERN REGIONAL JAIL & CORRECTIONAL CENTER")) %>% #mismatches
  filter(!NAME.y == "PEORIA COUNTY JAIL") #mismatches

#Manually include facility matches not linked by name similarity
linkage.join_N_unsimilar <- linkage.join_N %>%
  filter(id1 == 1359 & id2 == 64) %>% #North Alabama Community Based Center / Community Work Center is Decatur Community Based Facility / Community Work Center
  bind_rows(linkage.join_N %>%
              filter(id1 == 3127 & id2 == 75)) # Delta Correctional Facility is Leflore County Technical Violation Center

#Combine results so far
linkage.join_prelimresults <- 
  bind_rows(linkage.join_LP, linkage.join_N_matches, linkage.join_N_unsimilar)

#See how many unique entries from All States have multiple linked HIFLD entries
#linkage.join_multiple_hifld<- linkage.join_prelimresults %>%
#  group_by(id2, STATE.y, NAME.y) %>%
#  summarise(count = n()) %>%
#  filter(count > 1)
#remove(linkage.join_multiple_hifld)

#See how many unique entries from HIFLD have multiple linked All States entries
#linkage.join_multiple_All_States <- linkage.join_prelimresults %>%
#  group_by(id1, STATE, NAME) %>%
#  summarise(count = n()) %>%
#  filter(count >1)
#remove(linkage.join_multiple_All_States)

#See which entries from All States are not in linked preliminary results
All_States_no_HIFLD <- All_States.rl %>%
  anti_join(linkage.join_prelimresults, by = "ID.PrisonAg")
#Checked - the following facilities do not have entries in HIFLD but should be in the data:
#NORTHEAST NEW MEXICO CORRECTIONAL FACILITY	NM
#MURPHYSBORO LIFE SKILLS RE ENTRY CENTER	IL
#MADISON COUNTY COMMUNITY WORK CENTER	MS
#DEERFIELD MEN'S WORK CENTER 2	VA
#WERNERSVILLE CCC 30	PA
#WERNERSVILLE CCC 18	PA

####Add new FACILITYIDs to prison ag data set in All_States_join

All_States_join <- All_States_join %>%
  select(-FACILITYID) %>%
  right_join((linkage.join_prelimresults %>%
              convert(num(FACILITYID)) %>%
               select(FACILITYID, ID.PrisonAg)), by = c("ID.PrisonAg")) %>%
  bind_rows(All_States_join %>%
              anti_join(linkage.join_prelimresults, by = "ID.PrisonAg"))

#####Create key for Geospatial Centroid work
PrisonAg_HIFLD_ID_key <- All_States_join %>%
  select(ID.PrisonAg, FACILITYID)

#Save file
write_csv(PrisonAg_HIFLD_ID_key, path = "writing/eda_output_internal/Archive/2020-10-06/PrisonAg_HIFLD_ID_key.csv", append = FALSE)

#Create list of Ag Activities for Geospatial Centroid work

#Export distinct ID.PrisonAg entries from All_States_join data for Prison Ag for Geocentroid Team Work
PrisonAg <- All_States_join %>%
  select(-FACILITYID) %>%
  rename(Name = NAME.Prison_Ag, Confirmed_Activities = 'Confirmed Program') %>%
  distinct() %>%
  select(ID.PrisonAg, Name, State, state, state_code, region, division, Confirmed_Activities)

write_csv(PrisonAg, path = "writing/eda_output_internal/Archive/2020-10-06/PrisonAg.csv", append = FALSE)

####Save All_States, All_States_join for future work
saveRDS(All_States, file = "data/raw_data/2020-10-06/All_States.Rds")
saveRDS(All_States_join, file = "data/raw_data/2020-10-06/All_States_join.Rds")

#Load October 2020 All_States_finalresults data
All_States <- readRDS(file = "data/raw_data/2020-10-06/All_States.Rds")
All_States_join <- readRDS(file = "data/raw_data/2020-10-06/All_States_join.Rds")

#Remove undeeded data
remove(All_States_finalresults_202002)
remove(All_States_finalresults_202002_key_join)
remove(hifld_no_sf_20200215)
remove(hifld.comparison)
remove(PrisonAg_HIFLD_ID_key_202002)
remove(All_States_no_HIFLD)
remove(All_States.rl)
remove(hifld.rl)
remove(linkage, linkage.join, linkage.join_L, linkage.join_LP, linkage.join_N, linkage.join_N_matches,
       linkage.join_N_top_1, linkage.join_N_unsimilar, linkage.join_P, linkage.join_prelimresults)
remove(PrisonAg, PrisonAg_HIFLD_ID_key)
remove(upper)

#Join with HIFLD fields to create new All_States_finalresults table
All_States_finalresults <- All_States_join %>%
  select(-FACILITYID) %>%
  distinct() %>%
  select(ID.PrisonAg, NAME.Prison_Ag, state, state_code, State, region, division, everything())

#Save September 2020 data locally in project files
saveRDS(All_States_finalresults, file = "data/raw_data/2020-10-06/All_States_finalresults.Rds")

#Load September 2020 data
All_States_finalresults <- readRDS(file = "data/raw_data/2020-10-06/All_States_finalresults.Rds")

#Part II (Skipping this step for now): Perform record linkage between HIFLD data and census data
#Note: Original linkage in Archive EDA script. 
#(see: https://rpubs.com/ahmademad/RecordLinkage)

#Load 2005 Prison Census Data
load("./data/icpsr_data/ICPSR_24642/DS0001/24642-0001-Data.rda")

#Pre-processing: cleaned name of facility and filtered to only state operated facilities; rename pertinent columns for record linkage
da24642.0001.filter <- da24642.0001 %>%
  filter(V21 == "(2) St") %>%
  mutate(V2 = str_replace_all(V2, " & ", " AND ")) %>%
  mutate(V2 = str_squish(str_replace_all(str_trim(V2), "[^'[:^punct:]+]", " "))) %>%
  mutate(V2 = str_replace_all(V2, " AND ", " & ")) %>%
  mutate(V11 = str_trim(V11)) %>%
  separate(V12, into = c("ZIP", "ZIP4"), sep=5, remove = TRUE) %>%
  rename(NAME = V2, CITY = V10, STATE = V11) %>%
  rownames_to_column(var = "id")

########Analysis of All_States, HIFLD, and Census Data########

#Rename Confirmed Program to Confirmed Activities
All_States_finalresults <- All_States_finalresults %>%
  rename(`Confirmed Activities` = `Confirmed Program`)

#What? Counts of activities at state correctional facilities

#Total number of facilities by state with Confirmed (Yes=1/No=0) and Unconfirmed (NA) activities
All_States_finalresults %>%
       group_by(State, region, division, `Confirmed Activities`) %>%
       summarise(facilities_tot = n())
      
#Write to CSV file
#write_csv(All_States_finalresults %>%
#            group_by(State, region, division, `Confirmed Activities`) %>%
#            summarise(facilities_tot = n()),
#          "./writing/eda_output/facilities_tot_state.csv",
#          append=FALSE)

#List of correctional facilities with Confirmed Yes (Yes=1) Activities, excluding Culinary Arts and Food Service and Other
All_States_confirmed_ag <-
  All_States_finalresults %>%
  select(-`Culinary Arts & Food Service`, -Other) %>%
  filter(`Confirmed Activities`==1 & 
           (!is.na(`Horticulture & Landscaping`) | !is.na(`Crops & Silviculture`) | !is.na(`Animal Agriculture`) | !is.na(`Food Processing & Production`))
  )

#View Sum for all states in US of Above List of Facilities with Confirmed Yes (Yes=1) Activities, excluding Culinary Arts and Food Service and Other
All_States_finalresults %>%
  select(-`Culinary Arts & Food Service`, -Other) %>%
  filter(`Confirmed Activities`==1 & 
           (!is.na(`Horticulture & Landscaping`) | !is.na(`Crops & Silviculture`) | !is.na(`Animal Agriculture`) | !is.na(`Food Processing & Production`))) %>%
  summarise(facilities_confirmed_ag=n())

#View Sum for all states in US of Above List of Facilities with Confirmed Yes (Yes=1) Activities, excluding Culinary Arts and Food Service but including Other
All_States_finalresults %>%
       select(-`Culinary Arts & Food Service`) %>%
       filter(`Confirmed Activities`==1 & 
                (!is.na(`Horticulture & Landscaping`) | !is.na(`Crops & Silviculture`) | !is.na(`Animal Agriculture`) | !is.na(`Food Processing & Production`)| !is.na(`Other`))) %>%
       summarise(facilities_confirmed_ag=n())

#View Sum for all states in US of Above List of Facilities with Confirmed Yes (Yes=1) Activities, including Culinary Arts and Food Service and Other
All_States_finalresults %>%
       filter(`Confirmed Activities`==1 & 
                (!is.na(`Horticulture & Landscaping`) | !is.na(`Crops & Silviculture`) | !is.na(`Animal Agriculture`) | !is.na(`Food Processing & Production`)| !is.na(`Other`)| !is.na(`Culinary Arts & Food Service`))) %>%
       summarise(facilities_confirmed_ag=n())

#View sums for all states for facilities with confirmed/unconfirmed/confirmed no that have non-null category columns, excluding Culinary Arts and Food Service and Other
All_States_finalresults %>%
       select(-`Culinary Arts & Food Service`, - Other) %>%
       filter(
         (!is.na(`Horticulture & Landscaping`) | !is.na(`Crops & Silviculture`) | !is.na(`Animal Agriculture`) | !is.na(`Food Processing & Production`))) %>%
       group_by(`Confirmed Activities`, state) %>%
       summarise(facilities_confirmed_ag=n()) %>%
       arrange(desc(facilities_confirmed_ag))

#Count by state Above List of Facilities with Confirmed Yes (Yes=1) Activities, excluding Culinary Arts and Food Service and Other
All_States_Confirmed_Ag_count <- All_States_finalresults %>%
       select(-`Culinary Arts & Food Service`, - Other) %>%
       filter(`Confirmed Activities`==1 & 
                (!is.na(`Horticulture & Landscaping`) | !is.na(`Crops & Silviculture`) | !is.na(`Animal Agriculture`) | !is.na(`Food Processing & Production`))) %>%
       group_by(region, division, State, state) %>%
       summarise(facilities_confirmed_ag=n())

#Count by state Above List of Facilities with Confirmed Yes (Yes=1) Activities, including Culinary Arts and Food Service and Other
#All_States_Confirmed_Ag_count_ca <- All_States_finalresults %>%
#  filter(`Confirmed Activities`==1 & 
#            (!is.na(`Horticulture & Landscaping`) | !is.na(`Crops & Silviculture`) | !is.na(`Animal Agriculture`) | !is.na(`Food Processing & Production`))) %>%
#  group_by(region, division, State, state) %>%
#  summarise(facilities_confirmed_ag=n())

#a)Sum for States of All Correctional Facilities in Our Data Set; b) Sum of Prisons with Confirmed (Yes=1) Activities, excluding Culinary Arts and Food Service and Other, c) and Calculated as a Percentage (b/a)
All_States_confirmed_ag.count.pct <- All_States_finalresults %>%
  group_by(State, state, region, division) %>%
  summarise(facilities_tot = n()) %>% #all facilities surveyed in each state
  left_join(All_States_Confirmed_Ag_count, by = c("State", "state", "division", "region")) %>%
  mutate(pct = facilities_confirmed_ag/facilities_tot) #facilities with confirmed ag activities as a percentage of all facilities surveyed in each state

#Write CSV
write_csv(All_States_confirmed_ag.count.pct,
  "./writing/eda_output/facilities_tot_state_confirmed_ag_count_pct.csv",
)

#Above including culinary arts and food service
#All_States_confirmed_ag.count.ca.pct <- All_States_finalresults %>%
#  group_by(State, state, region, division) %>%
#  summarise(facilities_tot = n()) %>% #all facilities surveyed in each state
#  left_join(All_States_Confirmed_Ag_count_ca, by = c("State", "state", "division", "region")) %>%
#  mutate(pct = facilities_confirmed_ag/facilities_tot) #facilities with confirmed ag activities as a percentage of all facilities surveyed in each state

#Write CSV
#write_csv(All_States_confirmed_ag.count.ca.pct,
#          "./writing/eda_output/facilities_tot_state_confirmed_ag_count_ca_pct.csv",
#)


#What? Counts of types (i.e. categories) of activities at state correctional facilities
#List of Correctional Faciltiies + activity categories and subcategories for Confirmed activities excluding culinary arts and Other (Yes=1)
All_States_pivot <-
  All_States_finalresults %>%
  select(ID.PrisonAg, state, state_code, State, region, division, NAME.Prison_Ag, `Confirmed Activities`, `Horticulture & Landscaping`, 
         `Crops & Silviculture`, `Animal Agriculture`, `Food Processing & Production`, `Culinary Arts & Food Service`, Other 
  ) %>%
  # All_States %>%
  # rename(`Confirmed Activities` = `Confirmed Program`, ID.PrisonAg = id) %>%
  select(-`Culinary Arts & Food Service`, - Other) %>%
  filter(`Confirmed Activities`==1) %>%
  pivot_longer(cols = c(`Horticulture & Landscaping`,`Crops & Silviculture`,`Animal Agriculture`,`Food Processing & Production`) , names_to = "Activity Category", values_to = "Subcategory") %>%
  drop_na(Subcategory) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Horticulture", "Horticulture")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Horticulture class", "Horticulture")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Unspecified horticulture", "Horticulture")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "unspecified horticulture", "Horticulture")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Horticulture Program", "Horticulture")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Horticulture program", "Horticulture")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "horticulture program", "Horticulture")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Horticulture", "Horticulture Program")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Master gardener program", "Master Gardener Class")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Flower garden", "Flowers")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Flower Garden", "Flowers")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "flower garden", "Flowers")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Flower production", "Flowers")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Flower starters", "Flowers")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Flower sales", "Flowers")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Pesticide applicator", "Pest Management")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Pesticide management", "Pest Management")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Juice Production", "Juice Production")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Plant Science", "Plant Science")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Spice Production", "Spice Production")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Seafood orientation program", "Seafood Handling")) %>%
  mutate(Subcategory = trimws(Subcategory, which = "both")) 

#Number of correctional facilities by state with confirmed (Yes=1) activities within a category, excluding Culinary Arts and Food Service and Other
#Plus each facility within a given category as a percent of all facilities in that state that offer ag actvities
#Plus each facility within a given category as a percent of all facilities in a state in our dataset
All_States_cat <-
  All_States_pivot %>%
  group_by(State, state, region, division, `Confirmed Activities`,`Activity Category`) %>%
  summarise(cat_tot = n()) %>% #number of facilities in a state with confirmed activity in a given category
  left_join(All_States_Confirmed_Ag_count, by = c("State", "state", "region", "division")) %>%
  mutate(pct_of_facilities_confirmed_ag = cat_tot/facilities_confirmed_ag) %>% #percentage of confirmed ag facilities with a given activity
  left_join((All_States_finalresults %>%
            group_by(State, state, region, division) %>%
            summarise(facilities_tot = n())), by = c("State", "state", "region", "division")) %>%
  mutate(pct_of_facilities_tot = cat_tot/facilities_tot)

#Write to CSV
write_csv(All_States_cat, "./writing/eda_output/facilities_tot_confirmed_ag_state_category.csv", append=FALSE)

All_States_cat_count <- All_States_cat %>%
       select(State, `Activity Category`) %>%
       group_by(State) %>%
       summarise(number_activity_types = n())

All_States_cat_count_by_cat <- All_States_cat %>%
  select(State, `Activity Category`) %>%
  group_by(`Activity Category`) %>%
  summarise(states_with_activity_type = n())

#Write to CSV
write_csv(All_States_cat, "./writing/eda_output/total_states_with_each_activity_type.csv", append=FALSE)

#Number of correctional facilities by division with confirmed (Yes=1) activities in a category, excluding Culinary Arts and Food Service
All_Divisions_count <-
  All_States_pivot %>%
  group_by(region, division, `Confirmed Activities`,`Activity Category`) %>%
  summarise(facilities_confirmed_ag = n())

#Write to CSV
write_csv(All_Divisions_count,
          "./writing/eda_output/prisons_tot_confirmed_ag_division_category.csv",
          append=FALSE)

#Sum for States of All Prisons Surveyed and Sum of Prisons with Confirmed (Yes=1) activities, excluding Culinary Arts and Food Service, and Percentage of Confirmed
All_Divisions_Confirmed_Ag_pct <- All_States_finalresults %>%
  group_by(region, division) %>%
  summarise(all_prisons = n()) %>%
  left_join((All_States_Confirmed_Ag_count %>%
            group_by(region, division) %>%
            summarise(facilities_confirmed_ag = sum(facilities_confirmed_ag))), 
  by = c("region", "division")) %>%
  mutate(pct = facilities_confirmed_ag/all_prisons)


#Write CSV
write_csv(All_Divisions_Confirmed_Ag_pct,
          "./writing/eda_output/prisons_tot_division_confirmed_ag_pct.csv",
)

#Sum for States of All Prisons Surveyed and Sum of Prisons with Confirmed (Yes=1) activities, excluding Culinary Arts and Food Service and Other, and Percentage of Confirmed
All_Regions_Confirmed_Ag_pct <- All_States_finalresults %>%
  group_by(region) %>%
  summarise(all_facilities = n()) %>%
  left_join((All_States_Confirmed_Ag_count %>%
               group_by(region) %>%
               summarise(facilities_confirmed_ag = sum(facilities_confirmed_ag))), 
            by = c("region")) %>%
  mutate(pct = facilities_confirmed_ag/all_facilities)

#Write CSV
write_csv(All_Regions_Confirmed_Ag_pct,
          "./writing/eda_output/prisons_tot_region_confirmed_ag_pct.csv",
)

#Sum for Regions of All Prisons Surveyed and Sum of Prisons with Confirmed (Yes=1) activities by Category, excluding Culinary Arts and Food Service and Other, and Percentage of Facilities
All_Regions_cat <- All_States_cat %>%
  group_by(region, `Activity Category`) %>%
  summarise(facilities_cat_tot = sum(cat_tot))%>%
  left_join(All_States_finalresults %>%
  group_by(region) %>%
  summarise(facilities_tot = n()), by = "region") %>%
  mutate(pct = facilities_cat_tot/facilities_tot)

#Write CSV
write_csv(All_Regions_cat,
          "./writing/eda_output/prisons_tot_region_confirmed_ag_cat_pct.csv",
)

#Redundant of All_Regions_cat - check to see if any other code depends on it
#Number of prisons by region with confirmed (Yes=1) activities in a category, excluding Culinary Arts and Food Service and Other
#All_Regions_count <-
#  All_States_pivot %>%
#  group_by(region, `Confirmed Activities`,`Activity Category`) %>%
#  summarise(facilities_confirmed_ag = n())

#Write to CSV
#write_csv(All_Regions_count,
#          "./writing/eda_output/prisons_tot_confirmed_ag_region_category.csv",
#          append=FALSE)


#Number of prisons in US with confirmed (Yes=1) activities in a category, excluding Culinary Arts and Food Service
All_US_count <-
  All_States_pivot %>%
  group_by(`Confirmed Activities`,`Activity Category`) %>%
  summarise(facilities_confirmed_ag = n())

#Write to CSV
write_csv(All_US_count,
          "./writing/eda_output/prisons_tot_confirmed_ag_us_category.csv",
          append=FALSE)

#Bar plot: number of prisons with activities in a category, excluding Culinary Arts and Food Service
#c <- ggplot(data=All_States_count, aes(x=state, y=prisons_tot))+
#  geom_col(aes(fill=`Activity Category`))

#Question 1: What are the most common types of activities? 
#Solution: Number of prisons with confirmed activities in a subcategory, excluding Culinary Arts and Food Service

#Calculate the max number of subcategories within a category at a prison
max_col <- max(str_count(All_States_pivot$Subcategory, ";")+1)

#Remove spaces after semi-colons
All_States_pivot$Subcategory <-
  gsub(";\\s+",";", All_States_pivot$Subcategory)

#Split confirmed (Yes=1) subcategories into multiple columns, use semi-colon to parse
All_States_subcat <-
  All_States_pivot %>%
  separate(col=Subcategory, into=c(paste("Sub",1:max_col, sep = "_")), sep=";")

#Pivot subcategory columns into rows
All_States_subcat_pivot <-
  All_States_subcat %>%
  pivot_longer(cols=c(paste("Sub",1:max_col, sep = "_")), names_to = "Temp", values_to = "Subcategory") %>%
  select(-Temp) %>%
  drop_na("Subcategory")%>%
  mutate(Subcategory = str_to_title(Subcategory)) %>%
  mutate(Subcategory = str_replace_all(Subcategory, " And ", " and ")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, " Or ", " or ")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, " And ", " and "))

###Create export version
All_States_subcat_pivot_export <-
  All_States_subcat_pivot %>%
  select(ID.PrisonAg, `Confirmed Activities`, `Activity Category`, Subcategory) %>%
  rename(Confirmed_Activities = `Confirmed Activities`, Activity_Category = `Activity Category`, Activity_Subcategory = `Subcategory`)

view(All_States_subcat_pivot_export %>%
       select(Activity_Category, Activity_Subcategory) %>%
       distinct())

#Export
write_csv(All_States_subcat_pivot_export,
          "./writing/eda_output/PrisonAg_pivot_subcategories.csv",
          append=FALSE)

#Number of prisons by state with confirmed activities within category and subcategory, excluding Culinary Arts and Food Service and Other
All_States_subcat_count <-
  All_States_subcat_pivot %>%
  group_by(state, region, `Confirmed Activities`,`Activity Category`, Subcategory) %>%
  summarise(facilities_tot = n())

#Write to CSV
write_csv( All_States_subcat_count,
           "./writing/eda_output/prisons_tot_confirmed_ag_state_subcategory.csv",
           append=FALSE)

All_US_subcat_count_rank_cat_group <- All_States_subcat_count %>%
  group_by(`Activity Category`, Subcategory) %>%
    summarise(total = sum(facilities_tot)) %>%
    arrange(`Activity Category`, desc(total)) %>%
    mutate(`Rank in Activity Category` = rank(desc(total), ties.method = "min"))

All_US_subcat_count_rank_overall <- All_States_subcat_count %>%
  group_by(Subcategory) %>%
  summarise(total = sum(facilities_tot)) %>%
  arrange(desc(total)) %>%
  mutate(`Overall Rank` = rank(desc(total), ties.method = "min")) %>%
  left_join(
     (All_US_subcat_count_rank_cat_group %>%
          distinct(`Activity Category`, Subcategory)), by = "Subcategory") %>%
  select(`Activity Category`, Subcategory, `Overall Rank`)

###Dot plot of Top 4 Ranked Subcategories
dot <- All_US_subcat_count_rank_cat_group %>%
  filter(`Rank in Activity Category`<= 4) %>%
  arrange(total) %>%    # First sort by rank This sort the dataframe but NOT the factor levels
 mutate(Subcategory=factor(Subcategory, levels=Subcategory)) %>%   # This trick update the factor levels
 ggplot(aes(x=Subcategory, y=total)) + 
  geom_point(size=5, aes(colour = `Activity Category`)) +   # Draw points
  scale_color_viridis(name = "Activity Type", discrete=TRUE) +
  labs(x = "Activity Subcategory", y = "State Operated Facilities", title="Top Subcategories", 
       subtitle="By Activity Type") +  
  theme(axis.text = element_text(size=12)) +
  coord_flip()

ggsave("plot_ag_rank_subcategories.tiff", plot = last_plot(), device = "tiff", dpi = 300, path = "./writing/eda_output/")

#Number of prisons by division with confirmed activities within category and subcategory, excluding Culinary Arts and Food Service and Other
All_Divisions_subcat_count <-
  All_States_subcat_pivot %>%
  mutate(Subcategory = str_trim(tolower(All_States_subcat_pivot$Subcategory))) %>%
  group_by(region, division, `Confirmed Activities`,`Activity Category`, Subcategory) %>%
  summarise(facilities_confirmed_ag = n())

#Write to CSV
write_csv(All_Divisions_subcat_count,
          "./writing/eda_output/prisons_tot_confirmed_ag_division_subcategory.csv",
          append=FALSE)

#Number of prisons by region with confirmed activities within category and subcategory, excluding Culinary Arts and Food Service and Other
All_Regions_subcat_count <-
  All_States_subcat_pivot %>%
  mutate(Subcategory = str_trim(tolower(All_States_subcat_pivot$Subcategory))) %>%
  group_by(region, `Confirmed Activities`,`Activity Category`, Subcategory) %>%
  summarise(facilities_confirmed_ag = n())

#Write to CSV
write_csv(All_Regions_subcat_count,
           "./writing/eda_output/prisons_tot_confirmed_ag_region_subcategory.csv",
           append=FALSE)

#Final calculation: The most common types of confirmed activities nationwide by subcategory and category.
All_US_subcat_count <-
  All_States_subcat_pivot %>%
  mutate(Subcategory = str_trim(tolower(All_States_subcat_pivot$Subcategory))) %>%
  group_by(`Activity Category`, Subcategory) %>%
  summarise(facilities_confirmed_ag = n()) %>%
  arrange(desc(facilities_confirmed_ag))

#Write CSV
write_csv(
  All_US_subcat_count,
  "./writing/eda_output/prisons_tot_confirmed_ag_us_subcategory.csv",
  append=FALSE)

#Uninformative plot of all subcategories and states
#c_subcat <- ggplot(data=All_States_subcat_count, aes(x=state, y=prisons_tot))+
#  geom_col(aes(fill=`Subcategory`))

#Uninformative plot of all states and subcategories
#c2_subcat <- ggplot(data=All_States_subcat_count, aes(x=`Subcategory`, y=prisons_tot))+
#  geom_col(aes(fill=`state`))

########Question 2: Where are things taking place around the country?
#Solution: 50 state map of total activities across all categories: Number of prisons with confirmed activities.

#Loading 50 state data with geometry to produce maps using albersusa package

plot(usa_composite(proj="laea"))

#Old map using albers projection with Alaska/Hawaii. Retired map because of error with new version of ggplot2, as of June 24, 2020
#us <- usa_composite()
#us_map <- fortify(us, region="name")
#gg <- ggplot()
#gg <- gg + geom_map(data=us_map, map=us_map,
#                    aes(x=long, y=lat, map_id=id),
#                    color="#2b2b2b", size=0.1, fill=NA)
#gg <- gg + theme_map() + 
#  coord_proj(us_laea_proj)

#New base map as of June 24, 2020
us_sf <- usa_sf("laea")
us_sf <- us_sf %>%
  rename(State = name)
gg <- ggplot() +
  geom_sf(data = us_sf, size = 0.125)
gg <- gg +
  theme(panel.grid.major = element_line(colour = "transparent"), 
        panel.background = element_rect(fill = "transparent")) +
  coord_sf(datum = NA)
gg

#Add state geometry for maps
All_States_Confirmed_Ag_count_sf <- us_sf %>%
  select(State, geometry) %>%
  left_join(All_States_Confirmed_Ag_count,
            by = "State") %>%
  drop_na(facilities_confirmed_ag)

#Prisons with Activities by State 
ggplot(data=All_States_Confirmed_Ag_count_sf, aes(fill = facilities_confirmed_ag)) +
  geom_sf() +
  theme(panel.grid.major = element_line(colour = "transparent"), 
        panel.background = element_rect(fill = "transparent")) +
  coord_sf(datum = NA) +
  scale_fill_viridis(name="Facilities") +
  theme(legend.position="right") +
  labs(title = "State Correctional Facilities with Ag Activities")

ggsave("map_prisons_confirmed_ag.tiff", plot = last_plot(), device = "tiff", dpi = 300, path = "./writing/eda_output/")

#Add state geometry for maps (for same map above including Culinary Arts)
#All_States_Confirmed_Ag_count_ca_sf <- us_sf %>%
#  select(State, geometry) %>%
#  left_join(All_States_Confirmed_Ag_count_ca,
#            by = "State") %>%
#  drop_na(facilities_confirmed_ag)

#Add state geometry for maps
All_States_confirmed_ag.count.pct_sf <- us_sf %>%
  select(State, geometry) %>%
  left_join(All_States_confirmed_ag.count.pct,
            by = "State") %>%
  drop_na(facilities_confirmed_ag)

#Percentage of Prisons with Activities by State 
ggplot(data=All_States_confirmed_ag.count.pct_sf, aes(fill = pct)) +
  geom_sf() +
  theme(panel.grid.major = element_line(colour = "transparent"), 
        panel.background = element_rect(fill = "transparent")) +
  coord_sf(datum = NA) +
  scale_fill_viridis(name="Percent",  labels=scales::percent) +
  theme(legend.position="right") +
  labs(title = "Facilities with Ag Activities as Percent of State Correctional Facilities")

ggsave("map_prisons_confirmed_ag_pct.tiff", plot = last_plot(), device = "tiff", dpi = 300, path = "./writing/eda_output/")

#Add state geometry for maps (for same map as above with Culinary Arts)
#All_States_confirmed_ag.count.ca.pct <- us_sf %>%
#  select(State, geometry) %>%
#  left_join(All_States_confirmed_ag.count.ca.pct,
#            by = "State") %>%
#  drop_na(facilities_confirmed_ag)

#Question 2a: Are there certain parts of the country with certain activities more so than others? 
#Solution: 50 state map of each category: Number of prisons with confirmed activities.

#Add state geometry for mapping and link with all categories associated with all states
usf_sf_cat <- us_sf %>% 
  left_join((All_States_finalresults %>% distinct(State) %>%
               crossing(All_States_subcat_pivot_export %>% distinct (Activity_Category)))
             , by = "State") %>%
  drop_na(Activity_Category) %>%
  select(State, Activity_Category) %>%
  rename(`Activity Category` = Activity_Category) %>%
  left_join(All_States_cat, by = c("State", "Activity Category"))

#Map with a count of the prisons in each state that have confirmed ag activities
ggplot(data=
         (usf_sf_cat 
          #%>%
         #   filter(!`Activity Category` == "Other")
          )
         , aes(fill = cat_tot)) +
  geom_sf() +
  theme(panel.grid.major = element_line(colour = "transparent"), 
        panel.background = element_rect(fill = "transparent")) +
  coord_sf(datum = NA) + 
  facet_wrap( ~ `Activity Category`) +
  scale_fill_viridis(name="Facilities", na.value = "white") +
  theme(legend.position="right") +
  labs(title = "State Correctional Facilities with Ag Activities", subtitle = "By Activity Type")

ggsave("map_prisons_confirmed_ag_cat.tiff", plot = last_plot(), device="tiff", dpi = 300, path = "./writing/eda_output/")

#old map code
#gg_all_cat <- gg +
#  geom_map(data=
#             (All_States_cat %>%
#                filter(!`Activity Category` == "Other"))
#              , map=us_map,
#           aes(fill=facilities_confirmed_ag, map_id=State),
#           color="white", size=0.1) +
#  ggtitle("States with Confirmed Ag Activities") +
#  facet_wrap( ~ `Activity Category`) +
#  coord_proj(us_laea_proj) +
#  scale_fill_viridis(name="Facilities") +
#  theme(legend.position="right")+
#  labs(title = "State Correctional Facilities with Ag by Activity Type")

#ggsave("map_prisons_confirmed_ag_cat.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Map of the percentage of prisons within a state in our dataset that have confirmed ag activities by category
ggplot(data=
         (usf_sf_cat
          #%>%
          #  filter(!`Activity Category` == "Other")
          )
       , aes(fill = pct_of_facilities_tot)) +
  geom_sf() +
  theme(panel.grid.major = element_line(colour = "transparent"), 
        panel.background = element_rect(fill = "transparent")) +
  coord_sf(datum = NA) + 
  facet_wrap( ~ `Activity Category`) +
  scale_fill_viridis(name="Percent",  labels=scales::percent, na.value = "white") +
  theme(legend.position="right") +
  labs(title = "Facilities with Ag Activities as Percent of State Correctional Facilities", subtitle = "By Activity Type")

ggsave("map_pct_prisons_conf_ag_cat.tiff", plot = last_plot(), device="tiff", dpi = 300, path = "./writing/eda_output/")

#Old Map
#gg_all_cat_pct <- gg +
#  geom_map(data=(All_States_cat %>%
#                   filter(!`Activity Category` == "Other")),
#           map=us_map,
#           aes(fill=pct_of_facilities_tot, map_id=State),
#           color="white", size=0.1) +
#  ggtitle("Percent of State Correctional Facilities with Ag by Activity Type") +
#  facet_wrap( ~ `Activity Category`) +
#  coord_proj(us_laea_proj) +
#  scale_fill_viridis(name="Percent", labels=scales::percent) +
#  theme(legend.position="right")

#ggsave("map_pct_prisons_conf_ag_cat.png", plot = last_plot(), device="png", path = "./writing/eda_output/")


#Maps by activity - outdated, need to replace with geom_sf scripts
#Prisons with Horticulture Activities by State
#All_States_count_hort <-
#  All_States_count %>%
#  filter(`Activity Category`== "Horticulture & Landscaping")

#Map of prisons with Horticulture Activities
#gg_hort <- gg + 
#  geom_map(data=All_States_count_hort, map=us_map,
#           aes(fill=confirmed_ag_prisons, map_id=State),
#           color="white", size=0.1) +
#  coord_proj(us_laea_proj) +
#  scale_fill_viridis(name="Prisons with Horticulture & Landscaping") +
#  theme(legend.position="right")

#ggsave("map_prisons_conf_hort.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Percentage of prisons with Horticulture Activities
#All_States_pct_hort <- All_States %>%
#  group_by(State, state) %>%
#  summarise(all_prisons = n()) %>%
#  left_join(All_States_count_hort, by = c("State", "state")) %>%
#  mutate(Pct = confirmed_ag_prisons/all_prisons)

#Map
#gg_hort_pct <- gg + 
#  geom_map(data=All_States_pct_hort, map=us_map,
#           aes(fill=Pct, map_id=State),
#           color="white", size=0.1) +
#  coord_proj(us_laea_proj) +
#  scale_fill_viridis(name="Percentage of Prisons with Horticulture & Landscaping") +
#  theme(legend.position="right")

#ggsave("map_pct_prisons_conf_hort.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Prisons with Crops and Silviculture by State
#All_States_count_crops <-
#  All_States_count %>%
#  filter(`Activity Category`== "Crops & Silviculture")

#Map of Prisons with Crops and Silviculture
#gg_crops <- gg + 
#  geom_map(data=All_States_count_crops, map=us_map,
#           aes(fill=confirmed_ag_prisons, map_id=State),
#           color="white", size=0.1) +
#  coord_proj(us_laea_proj) +
#  scale_fill_viridis(name="Prisons with Crops & Silviculture") +
#  theme(legend.position="right")

#ggsave("map_prisons_conf_crops.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Percentage of Prisons with Crops and Silviculture
#All_States_pct_crops <- All_States %>%
#  group_by(State, state) %>%
#  summarise(all_prisons = n()) %>%
#  left_join(All_States_count_crops, by = c("State", "state")) %>%
#  mutate(Pct = confirmed_ag_prisons/all_prisons)

#Map
#gg_crops_pct <- gg + 
#  geom_map(data=All_States_pct_crops, map=us_map,
#           aes(fill=Pct, map_id=State),
#           color="white", size=0.1) +
#  coord_proj(us_laea_proj) +
#  scale_fill_viridis(name="Percentage of Prisons with Crops & Silviculture") +
#  theme(legend.position="right")

#ggsave("map_pct_prisons_conf_crops.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Prisons with Animal Ag by State
#All_States_count_animals <-
#  All_States_count %>%
#  filter(`Activity Category`== "Animal Agriculture")

#Map of Prisons with Animal Ag
#gg_animals <- gg + 
#  geom_map(data=All_States_count_animals, map=us_map,
#           aes(fill=confirmed_ag_prisons, map_id=State),
#           color="white", size=0.1) +
#  coord_proj(us_laea_proj) +
#  scale_fill_viridis(name="Prisons with Animal Agriculture") +
#  theme(legend.position="right")

#ggsave("map_prisons_conf_animals.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Percentage of Prisons with Animal Agriculture
#All_States_pct_animals <- All_States %>%
#  group_by(State, state) %>%
#  summarise(all_prisons = n()) %>%
#  left_join(All_States_count_animals, by = c("State", "state")) %>%
#  mutate(Pct = confirmed_ag_prisons/all_prisons)

#Map
#gg_animals_pct <- gg + 
#  geom_map(data=All_States_pct_animals, map=us_map,
#           aes(fill=Pct, map_id=State),
#           color="white", size=0.1) +
#  coord_proj(us_laea_proj) +
#  scale_fill_viridis(name="Percentage of Prisons with Animal Agriculture") +
#  theme(legend.position="right")

#ggsave("map_pct_prisons_conf_animals.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Prisons with Food Production & Processing by State
#All_States_count_food <-
#  All_States_count %>%
#  filter(`Activity Category`== "Food Processing & Production")

#Map of Prisons with Food Production & Processing by State
#gg_food <- gg + 
#  geom_map(data=All_States_count_food, map=us_map,
#           aes(fill=confirmed_ag_prisons, map_id=State),
#           color="white", size=0.1) +
#  coord_proj(us_laea_proj) +
#  scale_fill_viridis(name="Prisons with Food Processing & Production") +
#  theme(legend.position="right")

#ggsave("map_prisons_conf_food.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Percentage of Prisons with Food Production & Processing
#All_States_pct_food <- All_States %>%
#  group_by(State, state) %>%
#  summarise(all_prisons = n()) %>%
#  left_join(All_States_count_food, by = c("State", "state")) %>%
#  mutate(Pct = confirmed_ag_prisons/all_prisons)

#Map
#gg_food_pct <- gg + 
#  geom_map(data=All_States_pct_food, map=us_map,
#           aes(fill=Pct, map_id=State),
#           color="white", size=0.1) +
#  coord_proj(us_laea_proj) +
#  scale_fill_viridis(name="Percentage of Prisons with Food Processing & Production") +
#  theme(legend.position="right")

#ggsave("map_pct_prisons_conf_food.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Set viridis color palette for regional analyses
manualviridis4 <- viridis_pal()(4)
view(manualviridis4)

#Reorder for qualitative analyses, e.g. bar plots
manualviridis4 <- c("#31688EFF", "#FDE725FF", "#440154FF", "#35B779FF")

#Question 2b. What are the regions?

#Plot activity categories broken down by region
#region1 <- ggplot(data=All_Regions_count, aes(x=region, y=facilities_confirmed_ag)) +
#  geom_col(aes(fill=`Activity Category`)) + 
#  labs(y = "Number of Correctional Facilities", x = "Region", title = "Facilities with Ag Activities by Region")

#ggsave("plot_ag_region1.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Plot activity categories broken down by division
#division1 <- ggplot(data=All_Divisions_count, aes(x=division, y=facilities_confirmed_ag)) +
#  geom_col(aes(fill=`Activity Category`)) + 
#  labs(y = "Number of Correctional Facilities", x = "Division", title = "Facilities with Ag Activities by Regional Division") +
#  coord_flip()

#ggsave("plot_ag_division1.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Plot regions broken down by activity categories
#Using abbreviate for shorter labels, but could alternative use the following
#shortlabs <- c("Animal Ag", "Crops & Silv", "Food Proc & Prod", "Hort & Land")
region2 <- ggplot(data=All_Regions_cat, aes(x=`Activity Category`, y=facilities_cat_tot)) +
  geom_col(aes(fill=`region`)) +
  scale_fill_manual(values = manualviridis4) +
  labs(y= "State Operated Facilities", x = "Activity Type", fill = "Region", title = "State Correctional Facilities with Ag Activities", subtitle = "By Activity Type and Region") +
  scale_x_discrete(labels = label_wrap(12))

ggsave("plot_ag_region2.tiff", plot = last_plot(), device="tiff", dpi = 300, path = "./writing/eda_output/")

#region2b <- ggplot(data=(All_Regions_count %>% filter(!`Activity Category` == "Other")), aes(x=`Activity Category`, y=facilities_confirmed_ag)) +
#  geom_col(position = "dodge" , aes(fill=`region`)) +
#  scale_fill_manual(values = manualviridis4) +
#  labs(y= "State Operated Facilities", x = "Activity Type", fill = "Region", title = "State Correctional Facilities with Ag Activities", subtitle = "By Activity Type and Region") +
#  scale_x_discrete(labels=abbreviate)

#ggsave("plot_ag_region2b.tiff", plot = last_plot(), device="tiff", dpi = 1000, path = "./writing/eda_output/")

#region2c <- ggplot(data=All_Regions_count) +
#  geom_mosaic(aes(weight = facilities_confirmed_ag, x = product(`Activity Category`, region), fill = `Activity Category`))  +
# scale_fill_viridis_d("facilities_confirmed_ag") +
#  labs(x = "Activity Type", y = "State Operated Facilities", title = "State Correctional Facilities with Ag Activities", subtitle = "By Activity Type and Region") 

#ggsave("plot_ag_region2c.tiff", plot = last_plot(), device="tiff", dpi = 1000, path = "./writing/eda_output/")

#region2d <- ggplot(data=All_Regions_count, aes(x=`region`, y=facilities_confirmed_ag)) +
#  geom_col(position = "dodge", aes(fill=`Activity Category`)) +
#  scale_fill_manual(values = manualviridis4) +
#  labs(y= "State Operated Facilities", x = "Region", fill = "Activity Type", title = "State Correctional Facilities with Ag Activities", subtitle = "By Activity Type and Region") +
#  theme_bw()

#ggsave("plot_ag_region2d.tiff", plot = last_plot(), device="tiff", dpi = 1000, path = "./writing/eda_output/")

#Plot divisions broken down by activity categories
division2 <- ggplot(data=All_Divisions_count, aes(x=`Activity Category`, y=facilities_confirmed_ag)) +
  geom_col(aes(fill=`division`)) +
  labs(y= "Number of Correctional Facilities", x = "Activity Type", fill = "Division", title = "Correctional Facilities with Ag Activities")

ggsave("plot_ag_division2.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Calculate percentage of prisons in a regions with activities in a category
All_Regions_pct_category <- All_Regions_count %>%
  left_join((All_Regions_Confirmed_Ag_pct %>%
                select(region, all_prisons)),
            by=c("region")) %>%
  mutate(pct = facilities_confirmed_ag/all_prisons)

#Plot
region3 <- ggplot(All_Regions_pct_category, aes(x = `Activity Category`, y = pct, group = region)) +  
  geom_path(aes(color = region), alpha = 1 , size = 2,
            lineend = 'round', linejoin = 'round') +
  scale_y_continuous(labels=scales::percent) +
  labs(y= "Percentage of Prisons per Region with Confirmed Activities", x = "Activity Category", fill = "Region", title = "Percentage of Prisons per Region with Confirmed Ag Activities")

ggsave("plot_ag_region3.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Calculate percentage of prisons in a division with activities in a category
All_Divisions_pct_category <- All_Divisions_count %>%
  left_join((All_Divisions_Confirmed_Ag_pct %>%
               select(region, division, all_prisons)),
            by=c("region", "division")) %>%
  mutate(pct = confirmed_ag_prisons/all_prisons)

#Plot
division3 <- ggplot(All_Divisions_pct_category, aes(x = `Activity Category`, y = pct, group = division)) +  
  geom_path(aes(color = division), alpha = 1 , size = 2,
            lineend = 'round', linejoin = 'round') +
  scale_y_continuous(labels=scales::percent) +
  labs(y= "Percentage of Prisons per Division with Confirmed Activities", x = "Activity Category", fill = "Division", title = "Percentage of Prisons per Division with Confirmed Ag Activities")

ggsave("plot_ag_division3.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Calculate confirmed activities within a subcategory as a percent of the facilities with confirmed ag in a division
All_Divisions_subcat_count_pct <- All_Divisions_subcat_count %>%
  rename(facilities_confirmed_ag_subcat = facilities_confirmed_ag) %>%
  left_join((All_Divisions_Confirmed_Ag_pct %>%
               select(region, division, facilities_confirmed_ag)), by = c("region", "division")) %>%
  mutate(pct = facilities_confirmed_ag_subcat/facilities_confirmed_ag)

view(All_Divisions_subcat_count_pct %>%
       group_by(`Activity Category`) %>%
       summarise(count = n()))

#Plot - Don't use. Think about what analysis to create.
division4 <- ggplot(All_Divisions_subcat_count_pct, aes(x = `Subcategory`, y = pct, group = division)) +  
  geom_path(aes(color = division), alpha = 1 , size = 2,
            lineend = 'round', linejoin = 'round') +
  facet_wrap( ~ `Activity Category`) +
  scale_y_continuous(labels=scales::percent) +
  labs(y= "Percentage", x = "Activity Subcategory", fill = "Division", title = "Ag Activity Subcategory as a Percentage of Prisons with Confirmed Ag Activities") +
  coord_flip()

ggsave("plot_ag_division4.png", plot = last_plot(), device="png", path = "./writing/eda_output/")


#Question 3. Why? What are the purposes of the activities?

#####Check purposes -feed inmates vs. feeding inmates & change it all to feeding incarcerated individuals

#Subset of all states  confirmed ag data with purpose not null
#Need to make sure everyone has modified Purpose to exclude Culinary Arts
All_States_purpose <-
  All_States_confirmed_ag %>%
  filter(!is.na(`Stated Purpose of Activity`))

#Calculate the max number of purposes at a prison
max_col_purpose <- max(str_count(All_States_purpose$`Stated Purpose of Activity`, ";")+1)

#Remove spaces after semi-colons
All_States_purpose$`Stated Purpose of Activity` <-
  gsub(";\\s+",";", All_States_purpose$`Stated Purpose of Activity`)

#Separate data so that each purpose at a facility has one column
All_States_purpose_separate <-
  All_States_purpose %>%
  separate(col=`Stated Purpose of Activity`, 
           into=c(paste("Purpose",1:max_col_purpose, sep = "_")), sep=";")

#Pivot data so that each purpose at a facility has one row
All_States_purpose_pivot <-
  All_States_purpose_separate %>%
  pivot_longer(cols=c(paste("Purpose",1:max_col_purpose, sep = "_")), names_to = "Temp", values_to = "Purpose") %>%
  select(-Temp) %>%
  drop_na("Purpose") %>%
  mutate(Purpose = str_trim(str_to_lower(Purpose), side = "both")) %>%
  mutate(Purpose = str_replace_all(Purpose, "vocational training", "vocational")) %>%
  mutate(Purpose = str_replace_all(Purpose, "thearapy", "therapeutic")) %>%
  mutate(Purpose = str_replace_all(Purpose, "therapy", "therapeutic")) %>%
  mutate(Purpose = str_replace_all(Purpose, "educational", "education")) %>%
  mutate(Purpose = str_replace_all(Purpose, "education", "educational")) %>%
  mutate(Purpose = str_replace_all(Purpose, "recreational", "recreation")) %>%
  mutate(Purpose = str_replace_all(Purpose, "recreation", "recreational")) %>%
  mutate(Purpose = str_replace_all(Purpose, "feed inmates", "feeding incarcerated individuals")) %>%
  mutate(Purpose = str_replace_all(Purpose, "feeding inmates", "feeding incarcerated individuals")) %>%
  mutate(Purpose = str_replace_all(Purpose, "unknown", "unspecified")) %>%
  mutate(Purpose = str_to_title(Purpose)) %>%
  filter(Purpose != "")

##MAKE CHANGES TO NUMBER IN EQUATION
All_Region_purpose_count <- All_States_purpose_pivot %>%
  group_by(Purpose, region) %>%
  summarise(facilities_with_purpose_reg = n()) %>%
  left_join( All_States_purpose_pivot %>%
      group_by(Purpose) %>%
      summarise(facilities_with_purpose_us = n()), by = "Purpose") %>%
  mutate(pct_purpose = facilities_with_purpose_reg/facilities_with_purpose_us) %>%
  left_join(All_Regions_Confirmed_Ag_pct %>%
              select(facilities_confirmed_ag, region), by = "region") %>%
 mutate(pct_region_of_us =  facilities_confirmed_ag/650) %>%
  mutate(representation_factor = pct_purpose/pct_region_of_us)

view(All_States_purpose_pivot %>%
       filter(Purpose == "cost savings" & region == "South"))

view(All_States_purpose_pivot %>%
       filter(Purpose == "work requirement") %>%
       group_by(region) %>%
       summarise(count =))

#Plot purposes broken down by region
region4 <- ggplot(data=All_States_purpose_pivot, aes(x = fct_rev(fct_infreq(Purpose)))) +
  geom_bar(aes(fill=region)) +
  labs(y = "Number of Correctional Facilities", x = "Purpose", fill = "Region", title = "Purpose of Ag Activities by Region") +
  scale_fill_manual(values=manualviridis4) +
  theme(axis.text = element_text(size=12)) +
  coord_flip()

ggsave("plot_ag_region4.tiff", plot = last_plot(), device = "tiff", dpi = 500, path = "./writing/eda_output/")

#ggplot(data=All_States_purpose_pivot, aes(x = fct_rev(fct_infreq(Purpose)))) +
#  geom_bar(aes(fill=region)) +
#  labs(y = "Number of Correctional Facilities", x = "Purpose", fill = "Region", title = "Purpose of Ag Activities by Region") +
#  scale_fill_manual(values=testcolorsplasma2) +
#  coord_flip()

#ggsave("plot_ag_region4plasmamanual.png", plot = last_plot(), device = "png", path = "./writing/eda_output/")

#Plot regions broken down by purposes - not using
#region5 <- ggplot(data=All_States_purpose_pivot, aes(region, ..count..)) +
#  geom_bar(aes(fill=Purpose)) +
#  labs(y = "Number of Correctional Facilities", x = "Region", title = "Purpose of Ag Activities")

#ggsave("plot_ag_region5.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#States and count of prisons with stated purpose for ag activities
All_States_purpose_count <-
  All_States_purpose_pivot %>%
  group_by(State, state, Purpose) %>%
  summarise(facilities_tot = n())

All_States_purpose_pivot_unite <-
  All_States_purpose_pivot %>%
  unite("activities", Horticulture:Other, remove = FALSE, na.rm = TRUE, sep = " + " ) %>%
  mutate(activities = tolower(activities))

#Map of prisons with confirmed ag activities faceted by stated purpose
gg_purpose <- gg +
  geom_map(data=All_States_purpose_count, map=us_map,
           aes(fill=facilities_tot, map_id=State),
           color="white", size=0.1) +
  facet_wrap( ~ Purpose) +
  coord_proj(us_laea_proj) +
  scale_fill_viridis(name="Facilities with Confirmed Ag Activities: Stated Purpose", option = "plasma") +
  theme(legend.position="right")

ggsave("map_purpose_conf_ag.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Question 4. Who? Demographics

#Security level at facilities where Ag/Food activities are offered broken down by region
security1 <- ggplot(data=All_States_confirmed_ag, aes(SECURELVL, ..count..)) +
  geom_bar(aes(fill=region)) +
  labs(y = "Number of Prisons", x = "Security Level", title = "Security Level at Correctional Facilities with Ag Activities")

ggsave("plot_ag_security1.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Race/ethnicity (White non-Hispanic, Black non-Hispanic, Hispanic, so on) from 2005 census filtered to only state operated facilities that we have matched to All States data
race_ethnicity_census <- All_States_HIFLD_census %>%
      group_by(State) %>%
      summarise(all_race_eth_total = sum(V93, na.rm = TRUE), 
                count = n(),
                `White (non-Hispanic)` = sum(V84, na.rm = TRUE), 
                `Black (non-Hispanic)` = sum(V85, na.rm = TRUE),
                Hispanic = sum(V86, na.rm = TRUE), 
                `Native American` = sum(V87, na.rm = TRUE), 
                Asian = sum(V88, na.rm = TRUE),
                `Native Hawaiian` = sum(V89, na.rm = TRUE), 
                `Two or More Races` = sum(V90, na.rm = TRUE)) 

race_ethnicity_census_pivot <- race_ethnicity_census %>%
      pivot_longer(cols = 4:10, names_to = "Race_Ethnicity", values_to = "Race_Eth_grp_total") %>%
     mutate(pct = Race_Eth_grp_total/all_race_eth_total) %>%
     select(-count)

#Map

gg_race_eth <- gg +
  geom_map(data=race_ethnicity_census_pivot, map=us_map,
           aes(fill=pct, map_id=State),
           color="white", size=0.1) +
  ggtitle("Prison Ag Dataset: Race-Ethnicity Percentages in State Facilities based on 2005 Census") +
  facet_wrap( ~ Race_Ethnicity) +
  coord_proj(us_laea_proj) +
  scale_fill_viridis(option = "magma", name="Percent", labels=scales::percent) +
  theme(legend.position="right")

ggsave("map_race_eth_pct.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Race/ethnicity (White non-Hispanic, Black non-Hispanic, Hispanic, so on) from 2005 census filtered to only state operated facilities that we have matched to All States data and that have confirmed activities
race_ethnicity_census_confirmed_ag <- All_States_HIFLD_census %>%
  select(-`Culinary Arts and Food Service`) %>%
  filter(`Confirmed Activities`==1 & 
           (!is.na(Horticulture) | !is.na(Crops) | !is.na(`Animal Agriculture`) | !is.na(`Food Production`) | !is.na(Other))) %>%
  group_by(State) %>%
  summarise(all_race_eth_total = sum(V93, na.rm = TRUE), 
            count = n(),
            `White (non-Hispanic)` = sum(V84, na.rm = TRUE), 
            `Black (non-Hispanic)` = sum(V85, na.rm = TRUE),
            Hispanic = sum(V86, na.rm = TRUE), 
            `Native American` = sum(V87, na.rm = TRUE), 
            Asian = sum(V88, na.rm = TRUE),
            `Native Hawaiian` = sum(V89, na.rm = TRUE), 
            `Two or More Races` = sum(V90, na.rm = TRUE)) 

race_ethnicity_census_conf_ag_pivot <- race_ethnicity_census_confirmed_ag %>%
  pivot_longer(cols = 4:10, names_to = "Race_Ethnicity", values_to = "Race_Eth_grp_total") %>%
  mutate(pct = Race_Eth_grp_total/all_race_eth_total) %>%
  select(-count)

#Map

gg_race_eth_conf_ag <- gg +
  geom_map(data=race_ethnicity_census_conf_ag_pivot, map=us_map,
           aes(fill=pct, map_id=State),
           color="white", size=0.1) +
  ggtitle("Race-Ethnicity Percentages State Facilities with Confirmed Ag (2005 Census Data)") +
  facet_wrap( ~ Race_Ethnicity) +
  coord_proj(us_laea_proj) +
  scale_fill_viridis(option = "magma", name="Percent", labels=scales::percent) +
  theme(legend.position="right")

ggsave("map_race_eth_conf_ag_pct.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Race/ethnicity (White non-Hispanic, Black non-Hispanic, Hispanic, so on) from 2005 census filtered to only state operated facilities that we have matched to All States data and that have confirmed activities
race_ethnicity_census_no_ag <- All_States_HIFLD_census %>%
  select(-`Culinary Arts and Food Service`) %>%
  filter(`Confirmed Activities` == 0) %>%
  group_by(State) %>%
  summarise(all_race_eth_total = sum(V93, na.rm = TRUE), 
            count = n(),
            `White (non-Hispanic)` = sum(V84, na.rm = TRUE), 
            `Black (non-Hispanic)` = sum(V85, na.rm = TRUE),
            Hispanic = sum(V86, na.rm = TRUE), 
            `Native American` = sum(V87, na.rm = TRUE), 
            Asian = sum(V88, na.rm = TRUE),
            `Native Hawaiian` = sum(V89, na.rm = TRUE), 
            `Two or More Races` = sum(V90, na.rm = TRUE)) 

race_ethnicity_census_no_ag_pivot <- race_ethnicity_census_no_ag %>%
  pivot_longer(cols = 4:10, names_to = "Race_Ethnicity", values_to = "Race_Eth_grp_total") %>%
  mutate(pct = Race_Eth_grp_total/all_race_eth_total) %>%
  select(-count)

#Map

gg_race_eth_no_ag <- gg +
  geom_map(data=race_ethnicity_census_no_ag_pivot, map=us_map,
           aes(fill=pct, map_id=State),
           color="white", size=0.1) +
  ggtitle("Race-Ethnicity Percentages State Facilities without Confirmed Ag (2005 Census Data)") +
  facet_wrap( ~ Race_Ethnicity) +
  coord_proj(us_laea_proj) +
  scale_fill_viridis(option = "magma", name="Percent", labels=scales::percent) +
  theme(legend.position="right")

ggsave("map_race_eth_no_ag_pct.png", plot = last_plot(), device="png", path = "./writing/eda_output/")


#Stopped here. Need to break down by facility and look at which ones do and don't have types of activities.

#Security, Gender, and Race/Ethnicity of All States and HIFLD (SECURELVL) entries with identified links to 2005 Census (All Other Variables)
demographics <- All_States_HIFLD_census %>%
       select(NAME.PrisonAg_cleaned, state, region, division, `Confirmed Activities`, 
              SECURELVL, V23, V94, V95, V96, V97, V98, #Security Levels from HIFLD (SECURELVL) and 2005 Census
              V22, #Gender, "Male", "Female", or "Both"
              V84, #"White", non-Hispanic
              V85, #"Black", non-Hispanic
              V86, #"Hispanic"
              V87, #"American Indian"
              V88, #"Asian"
              V89, #"Native Hawaiian"
              V90, #Two or more races"
              V91, #Additional Categories
              V92, #Additional Categories specified
              V93, #Total inmates
              V206, #Prison Industries
              V207, #Prison Support Services
              V208, #Farming/Agriculture
              V209, #Public Works Assignments,
              V210, #Other
              V210S, #Specify
              V211, #None
              V212, #Inmates on Work Assignments
              V213, #Does Facility Operate Work Release Program
              V214, #Inmates Participating
              V220, #Vocational Training,
              V221, #College Courses
              V222, #Study Release Programs
              V223, #Other
              V223S #Specify
               )

#Facilities by gender by region
gender_region_division <- All_States_Confirmed_Ag %>%
  inner_join(demographics, by = c("NAME.PrisonAg_cleaned", "state", "region", "division")) %>%
  group_by(region, division, V22) %>%
  summarise(confirmed_ag_prisons = n())

#Plot
gg_gender <- ggplot(data=gender_region_division, aes(x=division, y=confirmed_ag_prisons)) +
  geom_col(aes(fill=`V22`)) + 
  labs(y = "Number of Prisons", x = "Division", title = "Prisons with Ag Activities by Gender and Regional Division", fill = "Gender")

ggsave("plot_ag_gender.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Are activity categories (of facilities with confirmed activities and linked to census data) offered at male, female, or both?
gender_activity_category <- All_States_pivot %>%
  inner_join(demographics, by = c("NAME.PrisonAg_cleaned", "state", "region", "division")) %>%
  group_by(region, division, V22, `Activity Category`) %>%
  summarise(confirmed_ag_prisons = n())

#Plot
gg_gender_activities <- ggplot(data=gender_activity_category, aes(x=`Activity Category`, y = confirmed_ag_prisons)) +
  geom_col(aes(fill=`V22`)) +
  labs(y = "Number of Prisons", x = "Activity Category", title = "Prisons with Ag Activities by Gender and Activity Category", fill = "Gender")

ggsave("plot_ag_gender_activities.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Plot
gg_gender_activities_region <- ggplot(data=gender_activity_category, aes(x=`Activity Category`, y = confirmed_ag_prisons)) +
  geom_col(aes(fill=`V22`)) +
  facet_wrap(~ region) +
  labs(y = "Number of Prisons", x = "Activity Category", title = "Prisons with Ag Activities by Gender and Activity Category", fill = "Gender") 

ggsave("plot_ag_gender_activity_region.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#For each activity category and gender classification, how many facilities have those activities as a percentage of the total facilities in our linked census dataset, broken down by gender? 
gender_activity_cat_pct <- All_States_pivot %>%
  inner_join(demographics, by = c("NAME.PrisonAg_cleaned", "state", "region", "division")) %>%
  group_by(V22, `Activity Category`) %>%
  summarise(confirmed_ag_prisons = n()) %>%
  left_join((demographics %>%
  group_by(V22) %>%
  summarise(total_prisons = n())), by = c("V22")) %>%
  mutate(pct = confirmed_ag_prisons/total_prisons)

#Plot
ggplot_gender_cat_pct <- ggplot(gender_activity_cat_pct, aes(x = `Activity Category`, y = pct, group = V22)) +  
  geom_path(aes(color = V22), alpha = 1 , size = 2,
            lineend = 'round', linejoin = 'round') +
  scale_y_continuous(labels=scales::percent) +
  labs(y= "Percentage", x = "Activity Category", fill = "Region", title = "Percentage of Facilities by Gender Offering Each Category of Activities", color = "Gender")

ggsave("plot_ag_gender_cat_pct.png", plot = last_plot(), device="png", path = "./writing/eda_output/")

#Percent in selected race/ethnicity groups at all facilities by region
race_eth_region <- demographics %>%
  drop_na(V84) %>%
  group_by(region, division) %>%
  summarise(White_nonHispanic = sum(V84)) %>%
  left_join(demographics %>%
             drop_na(V85) %>%
             group_by(region, division) %>%
             summarise(Black_nonHispanic = sum(V85)), by = c("region", "division")) %>%
  left_join(demographics %>%
              drop_na(V86) %>%
              group_by(region, division) %>%
              summarise(Hispanic = sum(V86)), by = c("region", "division")) %>%
  left_join(demographics %>%
       drop_na(V93) %>%
       group_by(region, division) %>%
       summarise(total_inmates = sum(V93)), by = c("region", "division")) %>%
  mutate(pct_wnh = White_nonHispanic/total_inmates) %>%
  mutate(pct_bnh = Black_nonHispanic/total_inmates) %>%
  mutate(pct_hisp = Hispanic/total_inmates)
  
view(
  (da24642.0001.filter %>%
    drop_na(V86) %>%
    group_by(V7) %>%
    summarise(hisp = sum(V86), total = sum(V93), count = n()) %>%
    rename(state = V7)) %>%
    left_join((All_States %>%
            distinct(state, region, division)), by = c("state")) %>%
  group_by(region, division) %>%
  summarise(hisp = sum(hisp), total = sum(total), count = sum(count)) %>%
  mutate(pct = hisp/total)
)

view(
  (linkage.census.join_full_LP %>%
     drop_na(V86) %>%
     group_by(state) %>%
     summarise(hisp = sum(V86), total = sum(V93), count = n())) %>%
    left_join((All_States %>%
                 distinct(state, region, division)), by = c("state")) %>%
    group_by(region, division) %>%
    summarise(hisp = sum(hisp), total = sum(total), count = sum(count)) %>%
    mutate(pct = hisp/total)
)

gg_all
gg_all_pct
gg_all_cat
gg_all_cat_pct
region1
division1
region4
security1

#Question: What are the 2005 census facilities with work requirements related to agriculture?
#From 2005 Census Question 38. What types of work assignments are available to inmates in this facility? Option - "Farming/agriculture" (V208)
 
#Agriculture/farming work assignments in 2005 census
da24642.0001.filter.ag <- da24642.0001.filter %>%
  select(id, V1, NAME, CITY, STATE, ZIP, V208) %>%
  left_join(f, by = c("STATE" = "state")) %>%
  rename("State" = state_name) %>%
  drop_na("State")

#Add regions to data
region_state <- data.frame(region = state.region, division = state.division, state= state.abb)
region_state$region <- str_replace(string = region_state$region, pattern = "North Central", replacement = "Midwest")
da24642.0001.filter.ag <- da24642.0001.filter.ag %>%
  left_join(region_state, by = c("STATE" = "state"))  

#How many state-operated facilities had work assignments? 281/1293 
view(da24642.0001.filter.ag %>%
      group_by(V208) %>% 
      summarise(count = n()) %>%
      rename(`Farming/Agriculture Work Assignments` = V208)
     )

view(da24642.0001.filter.ag %>%
       group_by(region, V208) %>% 
       summarise(count = n()) %>%
       rename(`Farming/Agriculture Work Assignments` = V208)
)


#Where are they located?
#By states
da24642.0001.filter.ag.state.count <- da24642.0001.filter.ag %>%
       select(id, V1, NAME, CITY, STATE, ZIP, V208, State, region, division) %>%
       group_by(State, V208) %>% 
       summarise(count = n()) %>%
       rename(`Farming/Agriculture Work Assignments` = V208)

#Combine census ag work requirements with spatial data frame (us_sf) from albers package
us_sf_ag <- us_sf %>%
  left_join(da24642.0001.filter.ag.state.count %>%
              filter(`Farming/Agriculture Work Assignments` == "(1) Yes")) %>%
  mutate(count = replace_na(count, NA)) %>%
  mutate(`Farming/Agriculture Work Assignments` = replace_na(`Farming/Agriculture Work Assignments`, "(1) Yes")) %>%
  bind_rows(
    us_sf %>%
      left_join(da24642.0001.filter.ag.state.count %>%
                  filter(`Farming/Agriculture Work Assignments` == "(0) No")) %>%
      mutate(count = replace_na(count, NA)) %>%
      mutate(`Farming/Agriculture Work Assignments` = replace_na(`Farming/Agriculture Work Assignments`, "(0) No"))
  )

#Map of state counts (Yes/No) for Farming/Ag Work Assignments
ggplot(data=us_sf_ag, aes(fill = count)) +
  geom_sf() +
  theme(panel.grid.major = element_line(colour = "transparent"), 
        panel.background = element_rect(fill = "transparent")) +
  coord_sf(datum = NA) +
  facet_wrap( ~ `Farming/Agriculture Work Assignments`) +
  scale_fill_viridis(name="Facilities") +
  theme(legend.position="right") +
  labs(title = "U.S. Census, 2005", subtitle = "Facilities with Agricultural Work Requirements")

ggsave("map_census_ag_work_reqs.tiff", plot = last_plot(), device = "tiff", dpi = 300, path = "./writing/eda_output/")

#Count overall
da24642.0001.filter.ag.us <- da24642.0001.filter.ag %>%
  select(id, V1, NAME, CITY, STATE, ZIP, V208, region, division) %>%
  group_by(V208) %>% 
  summarise(count = n()) %>%
  rename(`Farming/Agriculture Work Assignments` = V208)

#By regional division
da24642.0001.filter.ag.region <- da24642.0001.filter.ag %>%
       select(id, V1, NAME, CITY, STATE, ZIP, V208, region, division) %>%
       group_by(region, division, V208) %>% 
       summarise(count = n()) %>%
       rename(`Farming/Agriculture Work Assignments` = V208)

#Plot by region
#Plot work reqs broken down by region
ggplot(data=da24642.0001.filter.ag, aes(V208, ..count..)) +
  geom_bar(aes(fill=region)) +
  labs(y = "State Operated Facilities", x = "Farming/Ag Work Requirements", fill = "Region", title = "2005 U.S. Census of Prisons", subtitle = "Farming/Agriculture Work Requirements")

ggsave("plot_census_ag_region1.png", plot = last_plot(), device = "png", path = "./writing/eda_output/")

#same plot with viridis color palette
ggplot(data=da24642.0001.filter.ag, aes(V208)) +
  geom_bar(aes(fill=region)) +
  labs(y = "State Operated Facilities", x = "Farming/Ag Work Requirements", fill = "Region", title = "Census of State and Federal Adult Correctional Facilities, 2005", subtitle = "Farming/Agriculture Work Requirements") +
  scale_fill_viridis(discrete = TRUE)

#Plot regions broken down by work reqs
ggplot(data=da24642.0001.filter.ag, aes(region, ..count..)) +
  geom_bar(aes(fill=V208)) +
  scale_fill_viridis_d(labels = c("No", "Yes")) +
  labs(y = "State Operated Facilities", x = "Region", title = "Census of State and Federal Adult Correctional Facilities, 2005", subtitle = "Farming/Agriculture Work Requirements", fill = "Work Requirements")

ggsave("plot_census_ag_region2.tiff", plot = last_plot(), device="tiff", dpi=300, path = "./writing/eda_output/")

#Question: What are the 2005 census facility functions?
#From 2005 Census Question 4. "What are the functions of this facility? Mark ( X) all that apply."

#Agriculture/farming work assignments in 2005 census
da24642.0001.filter.ag.functions <- da24642.0001.filter %>%
  select(id, V1, NAME, CITY, STATE, ZIP, V208, V25:V36) %>%
  left_join(f, by = c("STATE" = "state")) %>%
  rename("State" = state_name) %>%
  drop_na("State") %>%
  left_join(region_state, by = c("STATE" = "state"))  

#Facilities that do not have a General Adult Population but do have Community Corrections
view(da24642.0001.filter.ag.functions %>%
  filter(V25 == "(0) No" & V30 == "(1) Yes")
)

da24642.0001.filter.ag.functions.pivot <- da24642.0001.filter.ag.functions %>%
  select(-V36) %>%
  rename("General Adult Population"      = V25, 
         "Alcohol/Drug Treatment"        = V26,
         "Reception/Diagnostic"          = V27,
         "Medical Treatment"             = V28,
         "Mental Health"                 = V29,
         "Community Corrections"         = V30,
         "Boot Camp"                     = V31,
         "Primarily Returned to Custody" = V32,
         "Primarily Youthful Offenders"  = V33,
         "Geriatric Care"                = V34,
         "Other"                         = V35
         ) %>%
  pivot_longer(cols = 8:18, names_to = "Function", values_to = "Function at Facility")

da24642.0001.filter.ag.functions.pivot <- da24642.0001.filter.ag.functions %>%
  select(-V36) %>%
  rename("General Adult Population"      = V25, 
         "Alcohol/Drug Treatment"        = V26,
         "Reception/Diagnostic"          = V27,
         "Medical Treatment"             = V28,
         "Mental Health"                 = V29,
         "Community Corrections"         = V30,
         "Boot Camp"                     = V31,
         "Primarily Returned to Custody" = V32,
         "Primarily Youthful Offenders"  = V33,
         "Geriatric Care"                = V34,
         "Other"                         = V35
  ) %>%
  pivot_longer(cols = 8:18, names_to = "Function", values_to = "Function at Facility")
    

#
#Plot work reqs broken down by region
da24642.0001.filter.ag.functions.pivot %>%
  filter(`Function at Facility` == "(1) Yes") %>%
  ggplot(aes(Function, ..count..)) +
  geom_bar(aes(fill=region)) +
  labs(y = "State Operated Correctional Facilities", x = "Functions", fill = "Region", title = "U.S. Census, 2005", subtitle = "Facilities with Designated Functions") +
  coord_flip()+
  scale_fill_viridis_d()

ggsave("plot_census_function_region1.tiff", plot = last_plot(), device = "tiff", dpi=300, path = "./writing/eda_output/")

#Plot functions broken down by work reqs
da24642.0001.filter.ag.functions.pivot %>%
  filter(`Function at Facility` == "(1) Yes") %>%
  ggplot() +
  geom_mosaic(aes(x = product(Function, V208), fill = Function))  +
  scale_fill_viridis_d() +
  labs(x = "Farming/Ag Work Requirements", y = "Facility Functions", title = "2005 U.S. Census of Prisons", subtitle = "Distribution of Facility Functions") +
  theme(legend.position = "none")
  
ggsave("plot_census_ag_function.png", plot = last_plot(), device = "png", path = "./writing/eda_output/")

########Clean and prepare prison agriculture data for Geocentroid project########

#List of correctional facilities with Confirmed Yes (Yes=1) Activities, including Culinary Arts and Food Service
All_States_confirmed_ag_ca <-
  All_States_finalresults %>%
  filter(`Confirmed Activities`==1 & 
           (!is.na(Horticulture) | !is.na(Crops) | !is.na(`Animal Agriculture`) | !is.na(`Food Production`) | !is.na(`Culinary Arts and Food Service`) | !is.na(Other))
  )

#List of Correctional Faciltiies +  activity categories and subcategories for Confirmed Activities (yes = 1) including Culinary arts
All_States_pivot_ca <-
  All_States_finalresults %>%
  replace_with_na(replace = list(Other = c("Sagebrush in Prisons Program"))) %>%
  filter(`Confirmed Activities`==1) %>%
  pivot_longer(cols = c(`Horticulture`,`Crops`,`Animal Agriculture`,`Food Production`, `Culinary Arts and Food Service`,`Other`) , names_to = "Activity Category", values_to = "Subcategory") %>%
  drop_na(Subcategory) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Horticultural class", "Horticulture")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Horticulture programl ", "Horticulture;")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Horticulture program", "Horticulture")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Unspecified horticulture program", "Unspecified horticulture")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Horticulture Program", "Horticulture")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Horticulture", "Horticulture Program")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Bakery", "Baking")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Culinary arts vocational training", "Culinary Arts")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Horses", "Equine")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Master gardener program", "Master Gardener Class")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other Pesticide applicator", "Pesticide Applicator")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other;Pesticide applicator", "Pesticide Applicator")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Pesticide applicator", "Pesticide Applicator")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Aquaponics", "Aquaponics")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Butterfly hatchery", "Butterfly Hatchery")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Cake Decorating Class", "Culinary Arts")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Cooking and nutrition class", "Culinary Arts")) %>% 
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Eggs", "Eggs")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Flower sales", "Flower Sales")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Honey production", "Honey Production")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Horticultural therapy", "Horticultural Therapy")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: House plants", "House Plants")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Hydroponics", "Hydroponics")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Juice Production", "Juice Production")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Mealpacking and donation", "Mealpacking and Donation")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Plant Science", "Plant Science")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Spice Production", "Spice Production")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Sagebrush cultivation", "Sagebrush Cultivation")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Seafood handling", "Seafood Handling")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: State Central Kitchen", "State Central Kitchen")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Vegetable and flower starters", "Vegetable and Flower Starters")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Other: Pesticide management", "Pesticide Management")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Pollinator garden", "Pollinator Habitat")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Pollinator Garden", "Pollinator Habitat")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Pollinator Gardens", "Pollinator Habitat")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Pollinator habitat", "Pollinator Habitat")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Pest Management", "Pesticide Management")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, "Composting\n", "Composting")) %>%
  mutate(Subcategory = trimws(Subcategory, which = "both"))

#Number of correctional facilities by state with confirmed (Yes=1) activities within a category, including Culinary Arts and Food Service
#Plus each facility within a given category as a percent of all facilities in that state that offer ag actvities
#Plus each facility within a given category as a percent of all facilities in a state in our dataset
All_States_cat_ca <-
  All_States_pivot_ca %>%
  group_by(State, state, region, division, `Confirmed Activities`,`Activity Category`) %>%
  summarise(cat_tot = n()) %>% #number of facilities in a state with confirmed activity in a given category
  left_join(All_States_Confirmed_Ag_count_ca, by = c("State", "state", "region", "division")) %>%
  mutate(pct_of_facilities_confirmed_ag = cat_tot/facilities_confirmed_ag) %>% #percentage of confirmed ag facilities with a given activity
  left_join((All_States_finalresults %>%
               group_by(State, state, region, division) %>%
               summarise(facilities_tot = n())), by = c("State", "state", "region", "division")) %>%
  mutate(pct_of_facilities_tot = cat_tot/facilities_tot)


#Solution: Number of prisons with confirmed activities in a subcategory, including Culinary Arts and Food Service

#Calculate the max number of subcategories within a category at a prison
max_col_ca <- max(str_count(All_States_pivot_ca$Subcategory, ";")+1)

#Remove spaces after semi-colons
All_States_pivot_ca$Subcategory <-
  gsub(";\\s+",";", All_States_pivot_ca$Subcategory)

#Split confirmed (Yes=1) subcategories into multiple columns, use semi-colon to parse
All_States_subcat_ca <-
  All_States_pivot_ca %>%
  separate(col=Subcategory, into=c(paste("Sub",1:max_col_ca, sep = "_")), sep=";")

#Pivot subcategory columns into rows
All_States_subcat_pivot_ca <-
  All_States_subcat_ca %>%
  select(-`Stated Purpose of Activity`) %>%
  pivot_longer(cols=c(paste("Sub",1:max_col_ca, sep = "_")), names_to = "Temp", values_to = "Subcategory") %>%
  select(-Temp) %>%
  drop_na("Subcategory") %>%
  mutate(Subcategory = str_to_title(Subcategory)) %>%
  mutate(Subcategory = str_replace_all(Subcategory, " And ", " and ")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, " Or ", " or ")) %>%
  mutate(Subcategory = str_replace_all(Subcategory, " And ", " and "))

#Create table of selected columns from pivot subcategory table for Geocentroid analysis
All_States_subcat_pivot_ca_export <-
  All_States_subcat_pivot_ca %>%
  select(ID.PrisonAg, `Confirmed Activities`, `Activity Category`, Subcategory) %>%
  rename(Confirmed_Activities = `Confirmed Activities`, Activity_Category = `Activity Category`, Activity_Subcategory = `Subcategory`)

#Evaluate distinct values within Subcategory column
#view(All_States_subcat_pivot_ca_export %>%
#       distinct(Subcategory))
#table(All_States_subcat_pivot_ca_export$Subcategory)

#Export
write_csv(All_States_subcat_pivot_ca_export,
          "./writing/eda_output_internal/PrisonAg_Activities.csv",
          append=FALSE)

#Number of prisons by state with confirmed activities within category and subcategory, excluding Culinary Arts and Food Service
All_States_subcat_count_ca <-
  All_States_subcat_pivot_ca %>%
  group_by(state, region, `Confirmed Activities`,`Activity Category`, Subcategory) %>%
  summarise(confirmed_ag_prisons = n())

#Write to CSV
write_csv( All_States_subcat_count_ca,
           "./writing/eda_output/prisons_tot_confirmed_ag_state_subcategory_ca.csv",
           append=FALSE)

view(All_States_subcat_count %>%
       group_by(`Activity Category`, Subcategory) %>%
       summarise(count = n()))

#########Playing with COVID-19 Data##############

test3 <- st_read("data/covid-19_behind_bars_data/data.kml")
plot(test3[1])
plot(test3[2])


##################MISCELLANEOUS EDA##################################

#####ADDITIONAL CLEANING SCRIPTS - for future analyses of census record linkage #######

#Examine the facilities that did not have matches
linkage.census.join_full_N <- linkage.census.join_full %>%
  filter(links == "N") %>%
  anti_join(linkage.census.join_full_LP, by = "id1") %>%
  anti_join(linkage.census.join_full_LP, by = "id2")

#Examine the facilities that did not have matches
linkage.census.join_N <- linkage.census.join %>%
  filter(links == "N") %>%
  anti_join(linkage.census.join_LP, by = "id1") %>%
  anti_join(linkage.census.join_LP, by = "id2")

#Examine the top match of facilities from All States data with linkage N    
linkage.census.join_full_N_top_1 <- linkage.census.join_full_N %>%
  group_by(id2) %>%
  top_n(n = 1, wt = NAME.x)

#Examine the top match of facilities from All States data with linkage N    
linkage.census.join_N_top_1 <- linkage.census.join_N %>%
  group_by(id2) %>%
  top_n(n = 1, wt = NAME.x)

#Filter out non-matches
linkage.census.join_N_matches <- linkage.join_N_top_1 %>%
  filter(!NAME %in% c("",
                      "",
                      "",
                      "",
                      "",
                      "",
                      "",
                      "",
                      "",
                      "",
                      "",
                      "",
                      "",
                      "",
                      "",
                      "")) %>% #mis-matches
  filter(!NAME %in% c("",
                      "",
                      "",
                      "")) #partial matches; there is 1:n relationship; will add back in below

#The next three resulting datasets contains 1:2, and 1:3 and other matches that were originally not identified as matches (links = N), based upon manual review

#Matches for facilities identified as having a 1:3 relationship with HIFLD data
linkage.census.join_N_top_3 <- linkage.join_N %>%
  group_by(id2) %>%
  filter(NAME.x.x %in% c("",
                         "" )) %>% 
  top_n(n = 3, wt = NAME.x)

#Matches for facilities identified as having a 1:2 relationship with HIFLD data
linkage.census.join_N_top_2 <- linkage.join_N %>%
  group_by(id2) %>%
  filter(NAME.x.x %in% c("",  "")) %>% #Central New Mexico Correctional Facility (Main/Level II and Level I) and Southern New Mexico Correctional Facility (Levels II & III)
  top_n(n = 2, wt = NAME.x)

#Match for a facility with a 2:1 relationship with HIFLD data
linkage.census.join_extra <- linkage.join_full %>%
  filter((NAME.x.x == "" & NAME.y.y == "") | 
           (NAME.x.x == "" & NAME.y == ""))

#Combine results so far
linkage.census.join_prelimresults <- 
  bind_rows(linkage.join_LP, linkage.join_N_matches, linkage.join_N_top_3 , linkage.join_N_top_2, linkage.join_extra)


####END OF CENSUS RECORD LINKAGE ADDITIONAL WORK#########


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
  pivot_longer(cols = 17:21, names_to = "Activity_temp", values_to = "Type_temp") %>%
  drop_na(Type_temp) %>%
  select(-Activity_temp)

Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_count <-
  subset(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp2, !is.na(Type)) %>%
  mutate(state = tolower(State)) %>%
  group_by(state) %>%
  count(Type)

###josh - other column distinct values
Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp <-
  Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state %>%
  separate(col = "Other? (Identify if so)", into = c("A","B","C","D","E"), sep = "; ") %>%
  pivot_longer(cols = 11:16, names_to = "Activity_temp", values_to = "Type_temp")
  
Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp2 <-
  Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp %>%
  mutate(Activity = (gsub("A|B|C|D|E", "Other? (Identify if so)", Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp$Program_temp))) %>%
  select(-Activity_temp) %>%
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
#  geom_bar(aes(fill = Activity))
#q
####number of prisons with food & ag programs by state
q2 <- ggplot(data = subset(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp2, !is.na(Type)), aes(state, ..count..)) +
  geom_bar(aes(fill = Activity))
q2 + labs(y = "Number of Prisons", x = "State", title = "Prisons with Food & Ag Activities")
####number of prisons with food & ag programs by subcategory and state
q3 <- ggplot(data = Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_temp3, aes(state, ..count..)) +
  geom_bar(aes(fill = Type_temp)) +
  facet_wrap(~ Program)
q3 + labs(y = "Number of Prison Programs", x = "State", title = "Food & Ag Activities at Prisons",  subtitle = "By Category and Subcategory") +
  guides(fill = guide_legend(title="Subcategories"))

###josh
s <- ggplot(data = subset(Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp, !is.na(Type)), aes(state, ..count..)) +
  geom_bar(aes(fill = Type))
s
t <- ggplot(data = Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp, aes(state, ..count..)) +
  geom_bar(aes(fill = Activity))
t
u <- ggplot(data = subset(Correctional_Facility_Contact_Tracking_Josh_States_SUMMER_state_temp, !is.na(Type)), aes(state, ..count..)) +
  geom_bar(aes(fill = Type)) +
  facet_wrap(~ Activity)
u

map <- map_data("state")
d <- ggplot(Correctional_Facility_Contact_Tracking_Addy_States_SUMMER_state_count, aes(fill = n))
d + geom_map(aes(map_id = state), map = map) +
  expand_limits(x = map$long, y = map$lat) +
  facet_wrap( ~ Type)
d + geom_map(aes(map_id = state), map = map) +
  expand_limits(x = map$long, y = map$lat) +
  facet_wrap( ~ Activity)


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

###HIFLD Prison Boundaries data
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


hifld_rl <- hifld %>%
  select()

rpairs=compare.linkage(RLdata500,RLdata10000,blockfld=c(1,7))

###In progress - creating map of HIFLD data
All_States_sum_pop <- All_States_finalresults %>%
  group_by(State) %>%
  summarise(POPULATION = sum(POPULATION))

gg_hifld <- gg +
  geom_map(data=All_States_sum_pop, map=us_map,
           aes(fill=POPULATION, map_id=State),
           color="white", size=0.1) +
  coord_proj(us_laea_proj) +
  scale_fill_viridis(name="Prison Populations") +
  theme(legend.position="right")

hifld_geometry <- hifld %>%
    select(NAME, STATE, geometry) %>%
    rename(NAME.HIFLD = NAME)

All_States_HIFLD_census_sf <- All_States_HIFLD_census %>%
  select(-geometry) %>%
  left_join((hifld %>%
              select(NAME, STATE, geometry) %>%
              rename(NAME.HIFLD = NAME)), by = c("NAME.HIFLD", "STATE"))

gg_test <- gg +
  geom_sf(All_States_HIFLD_census_sf) +
  coord_proj(us_laea_proj) +
  scale_fill_viridis(name="Prison Populations") +
  theme(legend.position="right")

gg +
  coord_sf(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

gg +
  coord_sf(crs = st_crs(4326))

gg +
  geom_sf(data=hifld_geometry) +
  coord_sf(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

us_sf <- usa_sf("laea")

plot(us_sf)
st_transform(hifld_geometry, crs = us_laea_proj)

ggplot() +
  geom_sf(data = us_sf) +
  geom_sf(data = hifld_geometry, shape = 1)

ggplot() +
  geom_sf(data = hifld_geometry) +
  geom_sf(data = us_sf, size = 0.125)
 
#Other work

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
us_map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap()
us_map + geom_sf(data = hifld, inherit.aes = FALSE) +
  scale_fill_brewer(palette = "OrRd") +
  coord_sf(crs = st_crs(4326))

####Previous work - Map of sites in HIFLD data set - need to check the projection types so that coordinates map correctly
ggplot(data=hifld) +
  geom_sf()

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
us_map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap()
us_map + geom_sf(data = hifld, inherit.aes = FALSE) +
  scale_fill_brewer(palette = "OrRd") +
  coord_sf(crs = st_crs(4326))

#read in imported food/ag activity data - counties
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
  filter(`Activity Category`== "Horticulture")
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

####number of prisons with food & ag activities by state
q <- ggplot(data = All_States_pivot, aes(state, ..count..)) +
  facet_wrap(~ "Activity Category")

q + labs(y = "Number of Prisons", x = "State", title = "Prisons with Food & Ag Activities")
####number of prisons with food & ag activities by subcategory and state
r <- ggplot(data = All_States_pivot, aes(state, ..count..)) +
  geom_bar(aes(fill = Type_temp)) +
  facet_wrap(~ Activity)
r + labs(y = "Number of Prison Programs", x = "State", title = "Food & Ag Activities at Prisons",  subtitle = "By Category and Subcategory") +
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

#Another option for mapping: https://cfss.uchicago.edu/notes/vector-maps/
ggplot() + 
  geom_sf(data = usa_48) + 
  geom_sf(data = airports_sf, shape = 1)

#county_map object is ready to be merged with a table of FIPS-coded US county data using either merge() or left_join().