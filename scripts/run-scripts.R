#------------------------------------------------------------------------------*
# Run scripts for service distance analysis
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
# Prepare analysis environment ----
#------------------------------------------------------------------------------*

# Load used packages
library(package = "secrlinear")
library(package = "igraph")
library(package = "rgeos")
library(package = "sf")
library(package = "stringr")
library(package = "lubridate")
library(package = "tidyverse")
library(package = "multidplyr")




#------------------------------------------------------------------------------*
# Run scripts ----
#------------------------------------------------------------------------------*

# Load and prepare locations and roads data
if(!file.exists("data/processed/geo-data.RData")){
  source(file = "scripts/get-geo-data.R")
}

# Process road network topologies and compute shortest paths
if(!file.exists("data/processed/routes.RData")){
  source(file = "scripts/shortest-paths.R")
}




# End of script
