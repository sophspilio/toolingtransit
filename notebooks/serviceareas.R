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
    unite(route_id, Agency, col = NewRouteID, sep = "_") %>%
    st_cast(., "MULTIPOINT") %>% st_cast("POINT") %>%
    mutate(lat = st_coordinates(.)[,2],
           lon = st_coordinates(.)[,1]) %>%
    st_drop_geometry()
  return(routes)
}



NovaRouteShapes <- rbind(
  routes("2022-04_Arlington.zip", "ART") %>% mutate(Agency = "ART"),
  routes("2022-03_CUE.zip", "CUE") %>% mutate(Agency = "CUE"),
  routes("2022-04_DASH.zip", "DASH") %>% mutate(Agency = "DASH"),
  routes("2022-03_Fairfax_Connector.zip", "FFX") %>% mutate(Agency = "FFX"),
  routes("2022-07_Loudoun.zip", "LCT") %>% mutate(Agency = "LCT"),
  routes("2022-03_VRE.zip", "VRE") %>% mutate(Agency = "VRE")
 # PRTCroutes
)

ART <- routes("2022-04_Arlington.zip", "ART") %>%
  st_cast(., "MULTIPOINT")
ART <- st_cast(ART, "POINT") %>%
  mutate(lat = st_coordinates(.)[,2],
         lon = st_coordinates(.)[,1]) %>%
  st_drop_geometry()


st_write(NovaRouteShapes, "AgencyProfileData/NovaRouteShapes.shp")



#write xy as column

st_write(NovaStopShapes, "AgencyProfileData/NovaStopShapes.shp", delete_layer = TRUE)

NovaStopShapes <- st_read("AgencyProfileData/NovaStopShapes.shp")
#set.seed(12)mutate(id = sample(1:9000, 8358))
NovaStopShapes <- st_cast(NovaStopShapes, "POINT") %>% st_write(., "AgencyProfileData/NovaStopShapes.csv", delete_layer = T, layer_options = "GEOMETRY=AS_XY")

st_cast(NovaStopShapes, "POINT") %>%
  mutate(coord = st_coordinates(.)) %>% st_drop_geometry() %>%
  st_write(., "AgencyProfileData/NovaStopShapes.csv", delete_layer = T)
