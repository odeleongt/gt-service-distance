#------------------------------------------------------------------------------*
# Compute route altitudes
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
# Prepare analysis environment ----
#------------------------------------------------------------------------------*

# check if environment is ready
if(! all(
  c("raster", "sf", "tidyverse", "multidplyr") %in%
  .packages()
)){
  stop("Environment not ready, run scripts/get-geo-data.R")
}

# record original objects
.original <- ls(all.names = TRUE)

# Load routes data
load(file = "data/processed/routes.RData")

# Load altitude data
altitude <- raster(x = "data/dem/raw/guatema.asc")




#------------------------------------------------------------------------------*
# Collect altitude for unique coordinates ----
#------------------------------------------------------------------------------*

# Get unique coordinates
vertex_coords <- service_routes %>%
  count(long, lat) %>%
  select(-n)

# Get cells for vertex coordinates  
vertex_cells <- cellFromXY(altitude, as.matrix(vertex_coords))

# Get altitudes for cells
vertex_altitudes <- vertex_coords %>%
  mutate(
    altitude = altitude[vertex_cells]
  )

# Join altitudes to paths
routes <- service_routes %>%
  left_join(vertex_altitudes)




#------------------------------------------------------------------------------*
# Save results ----
#------------------------------------------------------------------------------*

# Save topologies and shortest routes
save(routes, file = "data/processed/routes-altitude.RData")

# clean up
rm(
  list = setdiff(
    c(ls(all.names = TRUE), ".original"),
    .original
  )
)


# End of script
