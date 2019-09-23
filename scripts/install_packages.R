#Use development version of icpsrdata package until updated in CRAN
install.packages("devtools")
library(devtools)
install_github("fsolt/icpsrdata")

#Install packages from CRAN
install.packages(c("googledrive", "dplyr"))
