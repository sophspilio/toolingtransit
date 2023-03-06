#Nova Stops for Power BI Data Dashboard Service tab
library(tidyverse)
library(sf)
library(tidytransit)
library(toolingtransit)
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


stops <- function(gtfszip, agency) {
  require(tidyverse)
  require(tidytransit)
  require(sf)
  require(units)
  ### All gtfs data is located in this file path
  GTFS_path <- file.path ("Z:",
                          "NVTC General",
                          "Projects and Programs",
                          "Transit Resource Center (TRC)",
                          "Data",
                          "GTFS")

  #establish gtfs data
  GTFS <- read_gtfs(file.path(GTFS_path, gtfszip))
  message(agency)
  if(is.character(GTFS$stops$stop_lat)) {
    GTFS$stops$stop_lat <- as.numeric( GTFS$stops$stop_lat)
    GTFS$stops$stop_lon <- as.numeric(GTFS$stops$stop_lon)
    stops <- inner_join(GTFS$stop_times, GTFS$trips) %>%
      group_by(route_id, stop_id) %>% summarize(trips = n()) %>%
      inner_join(GTFS$stops) %>% inner_join(., GTFS$routes %>% select(route_id, route_long_name, route_short_name)) %>%
      mutate(Agency = agency) %>%  unite(route_id, Agency, sep = "_", col = "newroute_id", remove = F) %>%
      select(newroute_id, route_id, stop_lat, stop_lon, Agency, route_long_name, route_short_name)
  } else {
    stops <- inner_join(GTFS$stop_times, GTFS$trips) %>%
    group_by(route_id, stop_id) %>% summarize(trips = n()) %>%
    inner_join(GTFS$stops) %>% inner_join(., GTFS$routes %>% select(route_id, route_long_name, route_short_name)) %>%
    mutate(Agency = agency) %>% mutate(Mode = ifelse(Agency == "VRE", "CR", "Bus")) %>%
      unite(route_id, Agency, sep = "_", col = "newroute_id", remove = F) %>%
    select(newroute_id, route_id, stop_lat, stop_lon, Agency, route_long_name, route_short_name, Mode)
    }
  return(stops)
}


NovaStops <- rbind(stops(PRTCzip, "PRTC"),
                   stops(VREzip, "VRE"),
                   stops(ARTzip, "ART"),
                   stops(CUEzip, "CUE"),
                   stops(DASHzip, "DASH"),
                   stops(FFXzip, "FFX"),
                   stops(LCTzip, "LCT"))



#Metrobus
Nova <- st_read("data/Nova.shp")
Metrobus <- read_gtfs(file.path(GTFS_path, Metrobuszip))
Metrobus$stops <-  stops_as_sf(Metrobus$stops) %>%
  st_intersection(., Nova) %>%  mutate(stop_lat = st_coordinates(.)[,2],
                                       stop_lon = st_coordinates(.)[,1]) %>%
  st_drop_geometry()

MetrobusStops <-  inner_join(Metrobus$stop_times, Metrobus$trips) %>%
  group_by(route_id, stop_id) %>% summarize(trips = n()) %>%
  inner_join(Metrobus$stops) %>%
  inner_join(., Metrobus$routes %>% select(route_id, route_long_name, route_short_name)) %>%
  mutate(Agency = "WMATA", Mode = "Bus") %>%  unite(route_id, Agency, sep = "_", col = "newroute_id", remove = F) %>%
  select(newroute_id, route_id, stop_lat, stop_lon, Agency, route_long_name, route_short_name, Mode)

#metrorail
Metrorail <- read_gtfs(file.path(GTFS_path, Metrorailzip))

Metrorail$stops <- stops_as_sf(Metrorail$stops) %>% filter(grepl("PF_", stop_id)) %>%
  st_intersection(., Nova) %>% mutate(stop_lat = st_coordinates(.)[,2],
                                      stop_lon = st_coordinates(.)[,1]) %>%
  st_drop_geometry()

Metrorailstops <- inner_join(Metrorail$stop_times, Metrorail$trips) %>%
  group_by(route_id, stop_id) %>% summarize(trips = n()) %>%
  inner_join(Metrorail$stops) %>%
  inner_join(., Metrorail$routes %>% select(route_id, route_long_name, route_short_name)) %>%
  mutate(Agency = "WMATA", Mode = "HR") %>%  unite(route_id, Agency, sep = "_", col = "newroute_id", remove = F) %>%
  select(newroute_id, route_id, stop_lat, stop_lon, Agency, route_long_name, route_short_name, Mode)


# NovaStops and Routes for PowerBI----------------------------------------------------
#create stops file
NovaStops <- rbind(NovaStops, MetrobusStops, Metrorailstops)

NovaStops %>% mutate(route_name = ifelse(Agency == "PRTC", route_short_name,
                                         ifelse(Agency == "VRE", route_long_name,
                                                ifelse(Agency == "CUE", route_long_name, route_id)))) %>%
  ungroup() %>%
  select(newroute_id, Agency, route_name, stop_lat, stop_lon, Mode) %>% arrange(Agency) %>%
  st_write(., "AgencyProfileData/NovaStops_02.17.2023.csv", delete_dsn = TRUE)


#create route file
NovaStops %>% mutate(route_name = ifelse(Agency == "PRTC", route_short_name,
                                         ifelse(Agency == "VRE", route_long_name,
                                                ifelse(Agency == "CUE", route_long_name, route_id)))) %>%
  ungroup() %>%
  distinct(newroute_id, route_id, route_name, Agency, Mode) %>%
  st_write(., "AgencyProfileData/routes.xlsx", delete_dsn = TRUE)

#NovaStopsRoutes file for Acccess to Transit/ Access to Jobs analysis

NovaStops %>% mutate(route_name = ifelse(Agency == "PRTC", route_short_name,
                                         ifelse(Agency == "VRE", route_long_name,
                                                ifelse(Agency == "CUE", route_long_name, route_id)))) %>%
  ungroup() %>% stops_as_sf(.) %>%
  select(newroute_id, Agency, route_name, Mode) %>% arrange(Agency) %>%
  st_write(., "AgencyProfileData/NovaStopsRoutes.shp", delete_dsn = T)

