#Nova Stops for Power BI Data Dashboard Service tab
library(tidyverse)
library(sf)
library(tidytransit)
library(toolingtransit)
library(mapview)
library(units)

GTFS_path <- file.path ("Z:",
                        "NVTC General",
                        "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data",
                        "GTFS",
                        "2023")


ARTzip <- "2023-10_Arlington.zip"
CUEzip <- "2023-10_CUE.zip"
DASHzip <- "2023-10_DASH.zip"
FFXzip <- "2023-10_Fairfax_Connector.zip"
LCTzip <- "2023-10_Loudoun.zip"
PRTCzip <- "2023-10_OmniRide_PRTC.zip"
VREzip <- "2023-10_VRE.zip"
Metrobuszip <- "2023-10_Metrobus.zip"
Metrorailzip <- "2023-10_Metrorail.zip"
Nova <- st_read("data/Nova.shp")

stops <- function(gtfszip, agency) {
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
  st_write(., "AgencyProfileData/NovaStops_10.05.2023.csv", delete_dsn = TRUE)


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




##### NovaBus Stops for shared stops ----

Project_path <- file.path("Z:", "Sophie", "Mapping", "StateoftheStop", "data")

#make stop file with all agencies
ART <- read_gtfs(file.path(GTFS_path, ARTzip)) %>% .$stops %>% mutate(Agency = "ART")
MB <- read_gtfs(file.path(GTFS_path, Metrobuszip))%>% .$stops %>% mutate(Agency = "MB")
PRTC <- read_gtfs(file.path(GTFS_path, PRTCzip))%>% .$stops %>% mutate(Agency = "PRTC")
LCT <- read_gtfs(file.path(GTFS_path, LCTzip))%>% .$stops %>% mutate(Agency = "LCT")
DASH <- read_gtfs(file.path(GTFS_path, DASHzip))%>% .$stops %>% mutate(Agency = "DASH")
FFX <- read_gtfs(file.path(GTFS_path, FFXzip))%>% .$stops %>% mutate(Agency = "FFX")
CUE <- read_gtfs(file.path(GTFS_path, CUEzip))%>% .$stops %>% mutate(Agency = "CUE")

stops <- rbind(ART %>% select(stop_id, stop_name, stop_code, stop_lat, stop_lon, Agency),
      MB %>% select(stop_id, stop_name, stop_code, stop_lat, stop_lon, Agency),
      PRTC %>% select(stop_id, stop_name, stop_code, stop_lat, stop_lon, Agency),
      LCT %>% select(stop_id, stop_name, stop_code, stop_lat, stop_lon, Agency),
      DASH %>% select(stop_id, stop_name, stop_code, stop_lat, stop_lon, Agency),
      FFX %>% select(stop_id, stop_name, stop_code, stop_lat, stop_lon, Agency),
      CUE %>% select(stop_id, stop_name, stop_code, stop_lat, stop_lon, Agency))


#ART DASH and MB stop_codes are all shared
someshared <- rbind(MB %>% select(stop_id, stop_name, stop_code, stop_lat, stop_lon, Agency),
      ART %>% select(stop_id, stop_name, stop_code, stop_lat, stop_lon, Agency),
      DASH %>% select(stop_id, stop_name, stop_code, stop_lat, stop_lon, Agency)) %>%
  group_by(stop_code) %>% mutate(n = n()) %>%
  filter(n > 1) %>% arrange(stop_code)

#export all stops with note of who has shared between MB, ART, DASH
someshared %>% distinct(stop_code) %>% mutate(shared_id = paste0(stop_code, "s")) %>% right_join(., stops) %>%
  st_as_sf(., coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  st_write(., file.path(Project_path, "allstops.shp"))

#density based clustering plus shared stop_codes
allstopsc <- st_read(file.path(Project_path, "allstops.shp")) %>%
  mutate(CLUSTER_ID = ifelse(CLUSTER_ID == -1, NA, CLUSTER_ID))

#agencies with same stop multiple routes
sharedwithinagency <- allstopsc  %>% st_drop_geometry() %>% filter(!is.na(CLUSTER_ID)) %>%
  group_by(CLUSTER_ID, Agency) %>%
  mutate(n = n()) %>%
  filter(n > 1L)


c_id <- allstopsc  %>% st_drop_geometry() %>% filter(!is.na(shared_id),
                                             !stop_id %in% sharedwithinagency$stop_id) %>%
  as_tibble() %>% group_by(shared_id) %>% ungroup() %>%
  select(shared_id, stop_id,  Agency) %>%
  pivot_wider(names_from = Agency, values_from = stop_id)

s_id <- allstopsc  %>% st_drop_geometry() %>% filter(!is.na(CLUSTER_ID),
                                             !stop_id %in% sharedwithinagency$stop_id)  %>%
  as_tibble() %>% group_by(CLUSTER_ID) %>% ungroup() %>%
  select(CLUSTER_ID, stop_id, Agency) %>%
  pivot_wider(names_from = Agency, values_from = stop_id)

sharedstops <- full_join(c_id, s_id) %>% relocate(CLUSTER_ID, .after = shared_id) %>%
  unite(., MB, DASH, ART, PRTC, LCT, FFX, CUE, col = "new_id", sep = "-", na.rm = TRUE, remove = FALSE)


sharedstops %>% select(-CLUSTER_ID, -shared_id) %>%
  st_write(., file.path(Project_path, "SharedStopsLookUp.xlsx"), delete_dsn = TRUE)

inner_join(samplestops, sharedstops, by = c())
