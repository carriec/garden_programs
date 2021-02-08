#library
library(googledrive)
library(tidyverse)
library(readxl)

#List of files in Google Drive
#files <- drive_find()

###########October 6, 2020 and onward Import###########

#Import Becca's spreadsheets
becca <- drive_get(as_id("https://docs.google.com/spreadsheets/d/1Z3bVdLkiYJFrm4UA4lZwftrOLqdRhd0-/"))
drive_download(
      file = as_id("https://docs.google.com/spreadsheets/d/1Z3bVdLkiYJFrm4UA4lZwftrOLqdRhd0-/"), 
      path = "./data/raw_data/2021-02-08/Correctional_Facility_Hort_Programs_Becca.xlsx",
      overwrite = TRUE)

#Import Addy's spreadsheets
addy <- drive_get(as_id("https://docs.google.com/spreadsheets/d/1dHcloCb0dnGWNJJujC6ZcomXOcL88HKc/"))
drive_download(
  file = as_id("https://docs.google.com/spreadsheets/d/1dHcloCb0dnGWNJJujC6ZcomXOcL88HKc"), 
  path = "./data/raw_data/2021-02-08/Correctional_Facility_Contact_Tracking_Addy_States_SUMMER.xlsx",
  overwrite = TRUE)

#Import Josh's spreadsheets
#Note: Since the previous version in February, Josh edited activities in spreadsheet to exclude groundskeeping activities
josh <- drive_get(as_id("https://docs.google.com/spreadsheets/d/1pVtJQkMAeilikn-tXe-zT4_NnaujyaXh"))
drive_download(
  file = as_id("https://docs.google.com/spreadsheets/d/1pVtJQkMAeilikn-tXe-zT4_NnaujyaXh"), 
  path = "./data/raw_data/2021-02-08/Correctional_Facility_Contact_Tracking_Josh_States_NEWEST.xlsx",
  overwrite = TRUE)

#Import Carrie's spreadsheets
carrie <- drive_get(as_id("https://docs.google.com/spreadsheets/d/1xFcgV5AbYpa7LFnanFMSU18KCRUwAvCsaQyx1zUkpio"))
drive_download(
  file = as_id("https://docs.google.com/spreadsheets/d/1xFcgV5AbYpa7LFnanFMSU18KCRUwAvCsaQyx1zUkpio"),
  path = "./data/raw_data/2021-02-08/Correctional_Facility_Contact_Tracking_Carrie_States.xlsx",
  overwrite = TRUE)

#Import Azmal's spreadsheets
azmal <- drive_get(as_id("https://docs.google.com/spreadsheets/d/1zUfdX4gqw76B4t0rv2x_696GzVuBQbI1"))
drive_download(
  file = as_id("https://docs.google.com/spreadsheets/d/1zUfdX4gqw76B4t0rv2x_696GzVuBQbI1"),
  path = "./data/raw_data/2021-02-08/Correctional_Facility_Contact_Tracking_Azmal_States_SUMMER.xlsx",
  overwrite = TRUE)

#Import Evan's spreadsheets
evan <- drive_get(as_id("https://docs.google.com/spreadsheets/d/1CsM5fbXbmm09f9GPhrTPW7NoCCQIlE2B"))
drive_download(
  file = as_id("https://docs.google.com/spreadsheets/d/1CsM5fbXbmm09f9GPhrTPW7NoCCQIlE2B"),
  path = "./data/raw_data/2021-02-08/Correctional_Facility_Contact_Tracking_Evan_States_SUMMER.xlsx",
  overwrite = TRUE)

rm(addy, becca, carrie, azmal, evan, josh)