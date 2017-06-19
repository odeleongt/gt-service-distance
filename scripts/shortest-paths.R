

#'-----------------------------------------------------------------------------*
#' Prepare analysis environment
#'-----------------------------------------------------------------------------*

# check if environment is ready
if(! all(
  c("igraph", "secrlinear", "rgeos", "sf", "tidyverse", "multidplyr") %in%
  .packages()
)){
  stop("Environment not ready, run scripts/get-geo-data.R")
}


# record original objects
.original <- ls(all.names = TRUE)

# Load geo data
load(file = "data/processed/geo-data.RData")




#'-----------------------------------------------------------------------------*
#' Route data
#'-----------------------------------------------------------------------------*
#' Line to Graph to Route
#' Barry Rowlingson
#' Introduction
#' http://rpubs.com/geospacedman/routing
#'-----------------------------------------------------------------------------*



# Build Topology
# This function builds the igraph object. The fundamental trick is to intersect
# the lines with themselves. This creates vertices at all the end points and
# intersections. The graph is built with Euclidean distance as edge weight,
# and with the x and y coordinates attributed to the vertices.
build_topo <- function(lines) {
  
  g <- gIntersection(lines, lines)
  edges <- do.call(rbind, lapply(g@lines[[1]]@Lines, function(ls) {
    as.vector(t(ls@coords))
  }))
  lengths = sqrt((edges[, 1] - edges[, 3])^2 + (edges[, 2] - edges[, 4])^2)
  
  froms = paste(edges[, 1], edges[, 2])
  tos = paste(edges[, 3], edges[, 4])
  
  graph = graph.edgelist(cbind(froms, tos), directed = FALSE)
  E(graph)$weight = lengths
  
  xy = do.call(rbind, strsplit(V(graph)$name, " "))
  
  V(graph)$x = as.numeric(xy[, 1])
  V(graph)$y = as.numeric(xy[, 2])
  return(graph)
}




#------------------------------------------------------------------------------*
# Prepare topologies using paralell computing ----
#------------------------------------------------------------------------------*

# Use roads as a Spatial object
roads_sp <- as(roads, "Spatial")

# Manually create cluster
cluster <- create_cluster()

# Attach packages to cluster
cluster_library(cluster, "secrlinear")
cluster_library(cluster, "igraph")
cluster_library(cluster, "rgeos")
cluster_library(cluster, "tidyverse")

# Load used funtions into clusters
cluster_assign_value(cluster, "build_topo", build_topo)

# Load shared data into nodes
cluster_assign_value(cluster, "roads_sp", roads_sp)
cluster_assign_value(cluster, "path", paste0(getwd(), "/output/clusters/"))

# Partition departments
regions <- departments %>%
  partition(region_id, cluster = cluster)

# Intersetc roads with departments
topo_regions <- regions %>%
  do({
    region <- first(.$region_id)
    lines <- roads_sp %>% subset(region_id %in% region)
    utm_lines <- spTransform(lines, CRS("+proj=utm +zone=17N +datum=NAD27"))
    report_file <- paste0(path, "topologies")
    
    # report on progress
    cat(
      region, ",", Sys.getpid(), ",", as.character(Sys.time()), ",", "start\n",
      file = report_file, sep = "", append = TRUE
    )
    
    # attempt to free memory
    gc()
    
    # Get topologies
    results <- data_frame(
      wgs_topo = list(build_topo(lines)),
      utm_topo = list(build_topo(utm_lines))
    )
    
    # report on progress
    cat(
      region, ",", Sys.getpid(), ",", as.character(Sys.time()), ",", "end\n",
      file = report_file, sep = "", append = TRUE
    )
    
    # Return result
    results
  })


# Get topologies
topologies <- topo_regions %>%
  collect()

# Stop cluster and clean up
parallel::stopCluster(cluster)
rm(cluster, regions, topo_regions)
gc()




#------------------------------------------------------------------------------*
# Save results ----
#------------------------------------------------------------------------------*

# Save shortest routes
save(
  topologies,
  file = "data/processed/routes.RData"
)


