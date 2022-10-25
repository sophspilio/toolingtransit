GTFS_path <- file.path ("Z:",
                        "NVTC General",
                        "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data",
                        "GTFS")
Nova <- st_read("data/Nova.shp")

#Metrobus Stop Times in VA

Metrobus2022 <- read_gtfs(file.path(GTFS_path, "2022_06_Metrobus.zip"))

#need all stoptimes for va routes (including times in md, dc)
#so no spatial filtering just normal filtering using routeid/tripid
VaBus <- st_read("AgencyProfileData/WMATARoutesVA.csv") %>% filter(Mode == "Bus")

Metrobus2022$trips %>% filter(route_id %in% VaBus$route_d) %>% left_join(., Metrobus2022$stop_times)

MetrobusStopsTimes <- Metrobus2022$trips %>% filter(route_id %in% VaBus$route_d) %>%
  left_join(., Metrobus2022$stop_times)

write_csv(MetrobusStopsTimes, "AgencyProfileData/MetrobusStopTimes.csv")

#Metrorail Stop times in VA

Metrorail2022 <- read_gtfs(file.path(GTFS_path, "2022-06_Metrorail.zip"))
MetrorailStopsTimes <- stops_as_sf(Metrorail2022$stops) %>%
  #spatially filter to only the stops in va
  st_intersection(.,Nova) %>%
  st_drop_geometry() %>%
  left_join(., Metrorail2022$stop_times)

write_csv(MetrorailStopsTimes, "AgencyProfileData/MetrorailStopTimes.csv")
