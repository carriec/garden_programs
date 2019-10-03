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

#Import Addy's spreadsheets
addy <- drive_get(as_id("https://docs.google.com/spreadsheets/d/1dHcloCb0dnGWNJJujC6ZcomXOcL88HKc/"))
drive_download(
  file = as_id("https://docs.google.com/spreadsheets/d/1dHcloCb0dnGWNJJujC6ZcomXOcL88HKc"), 
  path = "./data/raw_data/Correctional_Facility_Contact_Tracking_Addy_States_SUMMER.xlsx",
  overwrite = TRUE)

#Import Josh's spreadsheets
josh <- drive_get(as_id("INSERT HERE"))
drive_download(
  file = as_id("INSERT HERE"), 
  path = "./data/raw_data/Correctional_Facility_Contact_Tracking_Josh_States_SUMMER.xlsx",
  overwrite = TRUE)
