library(tidyverse)
library(sf)
library(tidytransit)
library(mapview)

GTFS_path <- file.path ("Z:",
                        "NVTC General",
                        "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data",
                        "GTFS")


ARTzip <- "2023-02_Arlington.zip"
CUEzip <- "2023-02_CUE.zip"
DASHzip <- "2023-02_DASH.zip"
FFXzip <- "2023-02_Fairfax_Connector.zip"
LCTzip <- "2023-02_Loudoun.zip"
PRTCzip <- "2023-02_OmniRide_PRTC.zip"
VREzip <- "2023-02_VRE.zip"
Metrobuszip <- "2023-02_Metrobus.zip"
Metrorailzip <- "2023-02_Metrorail.zip"


routes <- function(GTFSzip){
  GTFS <- read_gtfs(file.path(GTFS_path, GTFSzip)) %>% gtfs_as_sf(.)

  routes <- inner_join(GTFS$trips, GTFS$routes) %>%
    select(route_id, shape_id) %>% inner_join(.,GTFS$shapes) %>% st_sf() %>%
    group_by(route_id) %>% summarize(n = n()) %>% select(-n) %>%
    mutate(GTFS_date = sub("_.*", "", GTFSzip))
  return(routes)
}

routes <- function(GTFSzip){
  GTFS <- read_gtfs(file.path(GTFS_path, GTFSzip))
  routes <- inner_join(GTFS$trips %>% select(route_id, trip_id, shape_id),
                       GTFS$routes %>% select(route_id, route_short_name, route_long_name)) %>%
    select(-trip_id) %>% distinct() %>%
    inner_join(.,shapes_as_sf(GTFS$shapes)) %>% st_sf() %>%
    group_by(route_id, route_short_name, route_long_name) %>%
    summarize(n = n()) %>%
    select(-n) %>%
    mutate(dist = st_length(geometry),
           GTFS_date = sub("_.*", "", GTFSzip))
  routes$dist <- set_units(routes$dist, mi)
  return(routes)
}


file <- "Z:/NVTC General/Projects and Programs/Transit Resource Center (TRC)/Data/GIS Data/Spatial Repo/R Bus Routes + Stops"

st_write(routes(ARTzip), file.path(file, "ARTRoutes.shp"), delete_dsn = TRUE)
st_write(routes(PRTCzip), file.path(file, "PRTCRoutes.shp"), delete_dsn = TRUE)
st_write(routes(VREzip), file.path(file, "VRERoutes.shp"), delete_dsn = TRUE)
st_write(routes(CUEzip), file.path(file, "CUERoutes.shp"), delete_dsn = TRUE)
st_write(routes(DASHzip), file.path(file, "DASHRoutes.shp"), delete_dsn = TRUE)
st_write(routes(FFXzip), file.path(file, "FFXRoutes.shp"), delete_dsn = TRUE)
st_write(routes(LCTzip), file.path(file, "LCTRoutes.shp"), delete_dsn = TRUE)
st_write(routes(Metrorailzip), file.path(file, "Metrorail.shp"), delete_dsn = TRUE)
st_write(routes(Metrobuszip), file.path(file, "Metrobus.shp"), delete_dsn = TRUE)




