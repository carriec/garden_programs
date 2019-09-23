#library
library(googledrive)
###library(dplyr) -- included in tidyverse
library(tidyverse)
library(readxl)

#List of files in Google Drive
files <- drive_find()

#Import Becca's spreadsheets
becca <- drive_get(as_id("https://docs.google.com/spreadsheets/d/1Z3bVdLkiYJFrm4UA4lZwftrOLqdRhd0-/"))
drive_download(
      file = as_id("https://docs.google.com/spreadsheets/d/1Z3bVdLkiYJFrm4UA4lZwftrOLqdRhd0-/"), 
      path = "./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx",
      overwrite = TRUE)
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_state <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx")
Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER_county <- read_excel("./data/raw_data/Correctional_Facility_Ag_Hort_Garden_Becca_States_SUMMER.xlsx", sheet = 2)