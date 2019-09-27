#Packages for downloading ICSPR data: Use development version of icpsrdata package until updated in CRAN
install.packages("devtools")
library(devtools)
install_github("fsolt/icpsrdata")

#Packages for downloading food production data Excel spreadsheets from Google Drive
###install.packages("dplyr") -- included in tidyverse
install.packages(c("googledrive", "readxl", "tidyverse"))

#Packages for analysis with census data
###if R installed through homebrew, run the following two commands in the terminal:
###brew install gdal
###brew install pkg-config
install.packages("tidycensus")

#Packages for mapmaking / visualization with ggplot2
install.packages("maps")