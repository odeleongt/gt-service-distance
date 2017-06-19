

#------------------------------------------------------------------------------*
#  Prepare analysis environment
#------------------------------------------------------------------------------*

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




#------------------------------------------------------------------------------*
#  Route data
#------------------------------------------------------------------------------*
#  Line to Graph to Route
#  Barry Rowlingson
#  Introduction
#  http://rpubs.com/geospacedman/routing
#------------------------------------------------------------------------------*



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
  lengths <- sqrt((edges[, 1] - edges[, 3])^2 + (edges[, 2] - edges[, 4])^2)
  
  froms <- paste(edges[, 1], edges[, 2])
  tos <- paste(edges[, 3], edges[, 4])
  
  graph <- graph.edgelist(cbind(froms, tos), directed = FALSE)
  E(graph)$weight = lengths
  
  xy <- do.call(rbind, strsplit(V(graph)$name, " "))
  
  V(graph)$x <- as.numeric(xy[, 1])
  V(graph)$y <- as.numeric(xy[, 2])
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
# Prepare shortest routes using paralell computing ----
#------------------------------------------------------------------------------*

# Shortest Path Routing
# This function maps two points (as one-row, two-column matrices) to the nearest
# node on the network then computes the vertices of the shortest path. We
# compute a route roughly across the diagonal of the unit square.
get_route_points <- function(graph, from, to) {
  xyg <- cbind(V(graph)$x, V(graph)$y)
  
  ifrom <- get.knnx(xyg, from, 1)$nn.index[1, 1]
  ito <- get.knnx(xyg, to, 1)$nn.index[1, 1]
  
  p <- get.shortest.paths(graph, ifrom, ito, output = "vpath")
  p[[1]]
}


# Manually create cluster
cluster <- create_cluster()

# Attach packages to cluster
cluster_library(cluster, "FNN")
cluster_library(cluster, "igraph")
cluster_library(cluster, "rgeos")
cluster_library(cluster, "tidyverse")

# Load used funtions into clusters
cluster_assign_value(cluster, "get_route_points", get_route_points)

# Services by region
services_region <- services %>%
  filter(service_type == "hospital") %>%
  cbind(., st_coordinates(.)) %>%
  as.data.frame() %>%
  select(region_id, service_id, site_long = X, site_lat = Y)

# Share communities data with workers
communities %>%
  filter(
    # Exclude errors
    !community_id %in% c(
      "803019", "803049", "803051", "805012", "805014", "805028", "805044",
      "805049", "805112", "805154", "805165", "805185", "805230", " 805458",
      "805458", "805461", "805465"
    ),
    !is.na(region_id)
  ) %>%
  cbind(., st_coordinates(.)) %>%
  as.data.frame() %>%
  select(region_id, subregion_id, community_id, long = X, lat = Y) %>%
  # Tag with respective hospital
  left_join(services_region) %>%
  cluster_assign_value(cluster = cluster, name = "communities", value = .)

# Share path with workers
cluster_assign_value(cluster, "path", paste0(getwd(), "/output/clusters/"))

# Partition topologies
topo_region <- topologies %>%
  partition(region_id, cluster = cluster)

# Get routes for each community
routes_region <- topo_region %>%
  do({
    # topology
    region <- first(.$region_id)
    wgs_topo <- .$wgs_topo[[which(.$region_id == first(region))]]
    utm_topo <- .$utm_topo[[which(.$region_id == first(region))]]
    report_file <- paste0(path, "routes")
    
    # report on progress
    cat(
      region, ",", Sys.getpid(), ",", as.character(Sys.time()), ",", "start\n",
      file = report_file, sep = "", append = TRUE
    )
    
    # attempt to free memory
    gc()
    
    # Compute shortest routes for region
    results <- communities %>%
      filter(region_id == region) %>%
      group_by(community_id, service_id) %>%
      do({
        cat(.$community_id, "\n")
        # Compute route
        route <- get_route_points(
          graph = wgs_topo,
          from = select(., long, lat),
          to = select(., site_long, site_lat)
        ) %>%
          unlist
        
        # Get route data
        data_frame(
          meters = get.edge.attribute(
            graph = utm_topo,
            name = "weight",
            index = E(utm_topo)[route]
          ),
          weight = get.edge.attribute(
            graph = wgs_topo,
            name = "weight",
            index = E(wgs_topo)[route]
          ),
          long = V(wgs_topo)[route]$x,
          lat = V(wgs_topo)[route]$y
        )
      })
    
    # report on progress
    cat(
      region, ",", Sys.getpid(), ",", as.character(Sys.time()), ",", "end\n",
      file = report_file, sep = "", append = TRUE
    )
    
    # Return results
    results
  })

# Collect shortest routes
service_routes <- routes_region %>%
  collect()

# Stop cluster and clean up
parallel::stopCluster(cluster)
rm(cluster, topo_region, routes_region)
gc()




#------------------------------------------------------------------------------*
# Save results ----
#------------------------------------------------------------------------------*

# Save topologies and shortest routes
save(topologies, file = "data/processed/topologies.RData")
save(service_routes, file = "data/processed/routes.RData")


# clean up
rm(
  list = setdiff(
    c(ls(all.names = TRUE), ".original"),
    .original
  )
)


# End of script
