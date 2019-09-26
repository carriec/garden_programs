#library
library(icpsrdata)
library(googledrive)
library(tidyverse)
library(readxl)


#download data
icpsr_download(file_id = c(36128, 24642, 37135), download_dir = "data/icpsr_data")

#download state codes in FACID for county jail data (included in 36128-0001-Codebook.pdf)
state_codes_facid <- drive_get(as_id("https://docs.google.com/spreadsheets/d/1LN0wMtGDgpKqedDBgnTL6d_35KFOOnX9Qnck_bheAc4/"))
drive_download(
  file = as_id("https://docs.google.com/spreadsheets/d/1LN0wMtGDgpKqedDBgnTL6d_35KFOOnX9Qnck_bheAc4"), 
  path = "./data/raw_data/36128-0001-Codebook-State-Codes-Key",
  overwrite = TRUE)