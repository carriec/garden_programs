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
install.packages("geojsonR")
install.packages("tmaptools")
install.packages("geojsonio")
install_github("ropensci/geojsonio")

install.packages("maptools")
install.packages("mapproj")
install.packages("rgeos")
install.packages("rgdal")
install.packages("RColorBrewer")
install.packages("ggplot2")

devtools::install_github("hrbrmstr/albersusa")
install.packages("ggalt")
install.packages("viridis")
install.packages("ggthemes")

install.packages("RCurl")

install.packages("xaringan")

install.packages("cowplot") #for making pretty plots with ggplot2

#Package for quickly converting numbers to integers
install.packages("hablar")

#Package for mosaic plot with ggplot2
install.packages("ggmosaic")

#Package for record linkage
install.packages("RecordLinkage")

#Package for working with categorical variables (factors)
install.packages("forcats")

#Package for converting values to null (NA)
install.packages("naniar")
