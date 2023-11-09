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


ARTzip <- "2023-07_Arlington.zip"
CUEzip <- "2023-07_CUE.zip"
DASHzip <- "2023-07_DASH.zip"
FFXzip <- "2023-07_Fairfax_Connector.zip"
LCTzip <- "2023-07_Loudoun.zip"
PRTCzip <- "2023-07_OmniRide_PRTC.zip"
VREzip <- "2023-07_VRE.zip"
Metrobuszip <- "2023-07_Metrobus.zip"
Metrorailzip <- "2023-07_Metrorail.zip"




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


file <- "Z:/NVTC General/Projects and Programs/Transit Resource Center (TRC)/Data/GIS Data/R Bus Routes + Stops"

st_write(routes(ARTzip), file.path(file, "ARTRoutes.shp"), delete_dsn = TRUE)
st_write(routes(PRTCzip), file.path(file, "PRTCRoutes.shp"), delete_dsn = TRUE)
st_write(routes(VREzip), file.path(file, "VRERoutes.shp"), delete_dsn = TRUE)
st_write(routes(CUEzip), file.path(file, "CUERoutes.shp"), delete_dsn = TRUE)
st_write(routes(DASHzip), file.path(file, "DASHRoutes.shp"), delete_dsn = TRUE)
st_write(routes(FFXzip), file.path(file, "FFXRoutes.shp"), delete_dsn = TRUE)
st_write(routes(LCTzip), file.path(file, "LCTRoutes.shp"), delete_dsn = TRUE)
st_write(routes(Metrorailzip), file.path(file, "Metrorail.shp"), delete_dsn = TRUE)
st_write(routes(Metrobuszip), file.path(file, "Metrobus.shp"), delete_dsn = TRUE)

rbind(
  routes(ARTzip) %>% mutate(Agency = "ART", Mode = "Bus"),
  routes(PRTCzip) %>% mutate(Agency = "OmniRide", Mode = "Bus"),
  routes(VREzip) %>% mutate(Agency = "VRE", Mode = "Commuter Rail"),
  routes(CUEzip) %>% mutate(Agency = "CUE", Mode = "Bus"),
  routes(DASHzip) %>% mutate(Agency = "DASH", Mode = "Bus"),
  routes(FFXzip) %>% mutate(Agency = "Fairfax Connector", Mode = "Bus"),
  routes(Metrorailzip) %>% mutate(Agency = "WMATA", Mode = "Heavy Rail"),
  routes(Metrobuszip) %>% mutate(Agency = "WMATA", Mode = "Bus")
) %>% st_write(file.path(file, "AllTransitRoutes.shp"), delete_dsn = TRUE)

