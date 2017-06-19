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
source(file = "scripts/get-geo-data.R")

# Process road network topologies and compute shortest paths
source(file = "scripts/shortest-paths.R")




# End of script
