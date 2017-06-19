#------------------------------------------------------------------------------*
# Run scripts for service distance analysis
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
# Prepare analysis environment ----
#------------------------------------------------------------------------------*

# Load used packages
library(package = "secrlinear")
library(package = "igraph")
library(package = "sf")
library(package = "stringr")
library(package = "lubridate")
library(package = "tidyverse")
library(package = "multidplyr")




#------------------------------------------------------------------------------*
# Run scripts ----
#------------------------------------------------------------------------------*

# Load and prepare locations data
source(file = "scripts/get-geo-data.R")




# End of script
