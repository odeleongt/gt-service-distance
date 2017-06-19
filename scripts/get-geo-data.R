#------------------------------------------------------------------------------*
#  Load geographic data
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
#  Prepare analysis environment
#------------------------------------------------------------------------------*

# check if environment is ready
if(! all(
  c("rgeos", "sf", "stringr", "lubridate", "tidyverse", "multidplyr") %in%
  .packages()
)){
  stop("Environment not ready, run scripts/get-geo-data.R")
}


# record original objects
.original <- ls(all.names = TRUE)




#------------------------------------------------------------------------------*
#  Get geographical and location data
#------------------------------------------------------------------------------*


# Read shapefile (data for the whole country)
departments <- read_sf("data/shapefiles/borders/departments.shp")
municipalities <- read_sf("data/shapefiles/borders/municipalities.shp")
services <- read_sf("data/shapefiles/gt-services/centros_salud_pdialogo_gtm.shp")
communities <- read_sf("data/shapefiles/communities/communities_ine_2002.shp")
roads <- read_sf("data/shapefiles/roads/gt_roads.shp")




#------------------------------------------------------------------------------*
#  Prepare places data
#------------------------------------------------------------------------------*

# Fix departments data
departments <- departments %>%
  mutate(
    region = iconv(NAME_DPTO, from = "Latin1", to = "ASCII//TRANSLIT")
  ) %>%
  select(region_id = COD_DPTO, region) %>%
  arrange(region_id)


# Fix municipality data
municipalities <- municipalities %>%
  mutate(
    subregion = iconv(MUNICIPIO, from = "Latin1", to = "ASCII//TRANSLIT"),
    region_id = substr(COD_MUNI, 1, 2)
  ) %>%
  left_join(
    select(as.data.frame(departments), region_id, region),
    by = "region_id"
  ) %>%
  select(
    region_id, subregion_id = COD_MUNI, subregion
  ) %>%
  arrange(region_id, subregion_id)


# Fix services data
services <- services %>%
  # Transform to correct projection
  st_transform(services, crs = 4326) %>%
  mutate(
    service = iconv(servicio, from = "Latin1", to = "ASCII//TRANSLIT"),
    service_type = recode(
      tipo_serv,
      "HOSPITAL" = "hospital",
      "CENTRO DE SALUD" = "center",
      "PUESTO DE SALUD" = "post",
      .default = "other"
    )
  ) %>%
  st_join(municipalities, join = st_within) %>%
  select(region_id, subregion_id, service_id = codgeo, service_type, service) %>%
  arrange(region_id, subregion_id, service_id)


# Communities
communities <- communities %>%
  mutate(
    community = str_to_title(LUGAR_POBL)
  ) %>%
  st_join(municipalities, join = st_within) %>%
  select(region_id, subregion_id, community_id = NUEVO_COD, community) %>%
  arrange(region_id, subregion_id, community_id)




#------------------------------------------------------------------------------*
# Prepare roads data using paralell computing ----
#------------------------------------------------------------------------------*

# Manually create cluster
cluster <- create_cluster()

# Attach packages to cluster
cluster_library(cluster, "sf")
cluster_library(cluster, "tidyverse")

# Load shared data into nodes
cluster_assign_value(cluster, "roads", roads)
cluster_assign_value(cluster, "path", paste0(getwd(), "/output/clusters/"))

# Partition departments
departments_partitioned <- departments %>%
  partition(region_id, cluster = cluster)

# Intersetc roads with departments
roads_regions <- departments_partitioned %>%
  do({
    region <- .
    report_file <- paste0(
      path, "road_intersections_", first(region$region_id), "_", Sys.getpid()
    )
    
    # report on progress
    cat(as.character(Sys.time()), " - ", file = report_file, sep = "")
    
    # Get intersections
    result <- data_frame(features = list(st_intersection(x = roads, y = .)))
    
    # report on progress
    cat(as.character(Sys.time()), file = report_file, append = TRUE)
    
    # Return result
    result
  })


# Collect intersected roads
roads <- roads_regions %>%
  collect %>%
  pull(features) %>%
  do.call(what = rbind, args = .) %>%
  mutate(
    road_id = paste0(
      CAM01_, CAM01_ID, CAM02_, CAM02_ID, CAM03_, CAM03_ID, CAM04_, CAM04_ID,
      CAM05_, CAM05_ID, CAM06_, CAM06_ID, CAM07_, CAM07_ID
    )
  ) %>%
  select(region_id, road_id)


# Remove cluster
parallel::stopCluster(cluster)
rm(departments_partitioned, roads_regions, cluster)
gc()



#------------------------------------------------------------------------------*
# Save processed data and finish up ----
#------------------------------------------------------------------------------*

save(
  departments, municipalities, services, communities, roads,
  file = "data/processed/geo-data.RData"
)


# clean up
rm(
  list = setdiff(
    c(ls(all.names = TRUE), ".original"),
    .original
  )
)


# End of script
