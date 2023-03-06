library(tidyverse)
library(sf)
library(tidytransit)
library(toolingtransit)
library(mapview)
library(units)
library(hms)
library(lubridate)
library(tidycensus)

#SETUP
GTFS_path <- file.path ("Z:",
                        "NVTC General",
                        "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data",
                        "GTFS")

Fairfax <- countycensus("Fairfax County") %>% st_union() %>% st_transform(crs = 4326)

#read gtfs
WMATABus <- read_gtfs(file.path(GTFS_path, "2022-11_Metrobus.zip"))

#only shapeids that intersect Fairfax County
ffxshpids <- shapes_as_sf(WMATABus$shapes) %>% st_intersection(., Fairfax) %>%
  select(shape_id) %>% st_drop_geometry()

#determine mileage of shapeids WITHIN fairfax only
WMATABus$shapes <- shapes_as_sf(WMATABus$shapes)
shapes <- WMATABus$shapes %>% filter(shape_id %in% ffxshpids$shape_id) %>%
  st_intersection(., Fairfax) %>%
  mutate(distffx = st_length(geometry)) %>% st_drop_geometry()
shapes$distffx <- set_units(shapes$distffx, mi)
#determine ENTIRE mileage of shapeids
shapes <- WMATABus$shapes %>% filter(shape_id %in% ffxshpids$shape_id) %>%
  mutate(dist = st_length(geometry)) %>% st_drop_geometry() %>% inner_join(., shapes)
shapes$dist <- set_units(shapes$dist, mi)


#route_id and route type: commuter, coverage, or local
#based on wmata fy21 line performance report
routes <- st_read("Z:/Sophie/Transit Resource Center/FairfaxMetrobusFares.xlsx")

#service ids that don't have 0 days of operation
# from WMATABus$calendar
serv <- c("5", "6", "7", "9")


#determine number of trips for each shapeid
trips <- WMATABus$trips %>% filter(shape_id %in% ffxshpids$shape_id) %>% filter(service_id %in% serv) %>%
  group_by(route_id, shape_id) %>% summarize(ntrips = n()) %>%
  inner_join(., shapes) %>% left_join(., routes, by = c("route_id" = "Route.ID"))




#METHOD 1
#proportion of route x total boardings for route
method1 <-  trips %>%
  #proportion times annual ridership
  mutate(ratioxriders = ratio*Estimated.Annual.Boardings.) %>%
  #sum by route type
  group_by(Route.Type) %>% summarize(ffxboardings = sum(ratioxriders, na.rm = T))

#proportion of trips for each shape id within route id x mileage ratio

trips %>% group_by(route_id) %>% summarize(sumtripsbyshapeid = sum(ntrips)) %>% right_join(., trips) %>%
  mutate(tripratio = ntrips/sumtripsbyshapeid) %>% mutate(ratio = (distffx/dist)*tripratio) %>%
  mutate(ratioxriders = ratio*Estimated.Annual.Boardings.)



#METHOD 2
#total route miles x connector pax/mi
connectorpaxmi <- 0.63
method2 <- trips %>%
  # num of trips per shapeid times fairfax shape id length
  mutate(ndistffx = ntrips*distffx) %>%
  #sum by route type
  group_by(Route.Type) %>% summarize(totalroutemi = sum(ndistffx)) %>% mutate(x = totalroutemi*connectorpaxmi*52)



x <- trips %>% group_by(route_id) %>% summarize(sumtripsbyshapeid = sum(ntrips)) %>% right_join(., trips) %>%
  mutate(tripratio = ntrips/sumtripsbyshapeid) %>% mutate(ratio = (distffx/dist)*tripratio) %>%
  mutate(ratioxriders = ratio*Estimated.Annual.Boardings.,  ndistffx = ntrips*distffx)


st_write(x, "Z:/Sophie/Transit Resource Center/FairfaxMetrobusFares.xlsx", delete_layer = T, layer = "R Analysis")

#looking at 17G:02
arl <- countycensus("Arlington")%>% st_union() %>%
  st_transform(crs = 4326)
fcity <- countycensus("Fairfax city")%>% st_union() %>%
  st_transform(crs = 4326)
i <- stops_as_sf(stops) %>% filter(shape_id == "17G:02")
mapview(i) + mapview(arl)+ mapview(fcity)+mapview(Fairfax)


####
#by shape id, stops within ffx vs outside


ffxstopids <- WMATABus$stops %>% stops_as_sf() %>% st_intersection(., Fairfax)


#all stops associated with a route that goes into fairfax
stops <- inner_join(WMATABus$stop_times, WMATABus$trips) %>% right_join(., shapes) %>%
  filter(route_id %in% routes$Route.ID) %>%
  group_by(route_id, stop_id, shape_id) %>% summarize(trips = n()) %>%
  inner_join(WMATABus$stops)

ffxstops <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 4326) %>% st_intersection(., Fairfax)

mapview(ffxstops, col.regions = "red") + mapview(stops_as_sf(stops), col.regions = "green")


a <- ffxstops %>%  st_drop_geometry() %>% group_by(shape_id) %>% summarize(countffxstops = n())
b <- stops %>%  st_drop_geometry() %>% group_by(shape_id) %>% summarize(counttotalstops = n())

inner_join(a, b) %>% st_write(., "Z:/Sophie/Transit Resource Center/ffxstops.csv")

