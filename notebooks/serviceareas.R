#service areas
library(tidyverse)
library(here)
library(censusapi)
library(tidycensus)
library(sf)
library(units)
library(tidytransit)
library(toolingtransit)
library(mapview)
library(rjson)


GTFS_path <- file.path ("Z:",
                        "NVTC General",
                        "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data",
                        "GTFS")

#test with ART

#shape
PRTC <- read_gtfs(file.path(GTFS_path, "2022-03_OmniRide_PRTC (2).zip"))
PRTC$shapes$shape_pt_lat <- as.numeric(PRTC$shapes$shape_pt_lat)
PRTC$shapes$shape_pt_lon <- as.numeric(PRTC$shapes$shape_pt_lon)
PRTCroutes <- shapes_as_sf(PRTC$shapes) %>%
  left_join(., PRTC$trips) %>%
  group_by(route_id) %>%
  summarize(count = n()) %>% select(route_id) %>%
  mutate(Agency = "PRTC") %>% unite(route_id, Agency, col = NewRouteID, sep = "_", remove = F)

ART <- read_gtfs(file.path(GTFS_path, "2022-04_Arlington.zip"))


mapview(test$shapes)
routes <- function(gtfszip, Agency){
  GTFS <- read_gtfs(file.path(GTFS_path, gtfszip))
  routes <- shapes_as_sf(GTFS$shapes) %>%
    left_join(., GTFS$trips) %>%
    group_by(route_id) %>%
    #arbitrary summariation to join by route_id
    summarize(count = n()) %>%
    select(route_id) %>% mutate(Agency = Agency) %>%
    unite(route_id, Agency, col = NewRouteID, sep = "_")
  return(routes)

}

stops <- function(gtfszip, Agency){
  GTFS <- read_gtfs(file.path(GTFS_path, gtfszip))
  stops <- inner_join(GTFS$stops, GTFS$stop_times) %>%
    inner_join(., GTFS$trips) %>% stops_as_sf() %>%
    group_by(route_id) %>% summarize(count = n()) %>%
    select(route_id) %>% mutate(Agency = Agency) %>%
    unite(route_id, Agency, col = NewRouteID, sep = "_", remove = F)
  return(stops)
}

NovaRouteShapes <- rbind(
  routes("2022-04_Arlington.zip", "ART"),
  routes("2022-03_CUE.zip", "CUE"),
  routes("2022-04_DASH.zip", "DASH"),
  routes("2022-03_Fairfax_Connector.zip", "FFX"),
  routes("2022-07_Loudoun.zip", "LCT"),
  routes("2022-03_VRE.zip", "VRE"),
  PRTCroutes
)
st_write(NovaRouteShapes, "AgencyProfileData/NovarouteShapes.shp")

NovaStopShapes <- rbind(
  stops("2022-04_Arlington.zip", "ART"),
  stops("2022-03_CUE.zip", "CUE"),
  stops("2022-04_DASH.zip", "DASH"),
  stops("2022-03_Fairfax_Connector.zip", "FFX"),
  stops("2022-07_Loudoun.zip", "LCT"),
  stops("2022-03_VRE.zip", "VRE"),
  stops("2022-03_OmniRide_PRTC (2).zip", "PRTC")
)


#write xy as column

st_write(NovaStopShapes, "AgencyProfileData/NovaStopShapes.shp", delete_layer = TRUE)

NovaStopShapes <- st_read("AgencyProfileData/NovaStopShapes.shp")
#set.seed(12)mutate(id = sample(1:9000, 8358))
NovaStopShapes <- st_cast(NovaStopShapes, "POINT") %>% st_write(., "AgencyProfileData/NovaStopShapes.csv", delete_layer = T, layer_options = "GEOMETRY=AS_XY")

st_cast(NovaStopShapes, "POINT") %>%
  mutate(coord = st_coordinates(.)) %>% st_drop_geometry() %>%
  st_write(., "AgencyProfileData/NovaStopShapes.csv", delete_layer = T)
