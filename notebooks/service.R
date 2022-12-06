#Service calculations for Transit Agency Profiles

#load libraries
library(tidyverse)
library(sf)
library(tidytransit)
library(toolingtransit)
library(mapview)
library(units)
library(hms)
library(lubridate)
GTFS_path <- file.path ("Z:",
                        "NVTC General",
                        "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data",
                        "GTFS")
#set up

service <- function(GTFSzip, Agency, Month, Year){
  #set up
  message(Agency)
  GTFS_path <- file.path ("Z:",
                          "NVTC General",
                          "Projects and Programs",
                          "Transit Resource Center (TRC)",
                          "Data",
                          "GTFS")

  #read gtfs
  GTFS <- read_gtfs(file.path(GTFS_path, GTFSzip))



  #calendar
  cal <- GTFS$calendar %>%
    mutate_at(c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'), as.numeric) %>%
    mutate(daysofop = monday + tuesday + wednesday + thursday + friday + saturday + sunday) %>%
    mutate(days = ifelse(daysofop == 5, "M-F", ifelse(daysofop == 4, "M-Th",
                                                      ifelse(friday == 1, "F",
                                                             ifelse(saturday == 1, "Sa",
                                                                    ifelse(sunday == 1, "Su", NA))))))
  #shapes
  if(is.character(GTFS$stops$stop_lat)) {
    GTFS$shapes$shape_pt_lat <- as.numeric(GTFS$shapes$shape_pt_lat)
    GTFS$shapes$shape_pt_lon <- as.numeric(GTFS$shapes$shape_pt_lon)
    shapes <- shapes_as_sf(GTFS$shapes) %>%
      left_join(., GTFS$trips) %>%
      mutate(dist = st_length(geometry)) %>%
      group_by(route_id, direction_id) %>%
      summarize(dist = mean(dist))
  } else {
    shapes <- shapes_as_sf(GTFS$shapes) %>%
      left_join(., GTFS$trips) %>%
      mutate(dist = st_length(geometry)) %>%
      group_by(route_id, direction_id) %>%
      summarize(dist = mean(dist))
  }


  #make sure to update the units to miles (not meters)
  shapes$dist <- set_units(shapes$dist, mi)


  #create trips file
  trips <- GTFS$trips %>% inner_join(., cal) %>%
    select(route_id, service_id, trip_id, direction_id,
           monday, tuesday, wednesday, thursday, friday, saturday, sunday) %>%
    left_join(., GTFS$stop_times) %>%
    group_by(route_id, trip_id, service_id, direction_id)

  #avg route time
  routes <- trips %>%
    summarize(start = as_hms(min(arrival_time[arrival_time != 0])),
              end = as_hms(max(departure_time[departure_time != 0]))) %>%
    mutate(time = difftime(end, start)) %>%
    group_by(route_id, direction_id) %>% summarize(time = mean(time)) %>%
    group_by(route_id) %>% summarize(routetime = sum(time))
  units(routes$routetime) <- "mins"

  #length(miles) and directional length
  routes <- shapes %>%
    group_by(route_id) %>% summarize(length = sum(dist), direction = n()) %>%
    st_drop_geometry() %>% mutate(length1way = length/direction) %>% select(-direction) %>%
    inner_join(., routes)

  #avg speed
  routes <- routes %>% mutate(speed = length/as.numeric(routetime, unit = "hours"))

  #span of service
  routes <- trips %>%
    summarize(start = as_hms(min(arrival_time)),
              end = as_hms(max(departure_time))) %>%
    group_by(route_id) %>% summarize(span = as.numeric(max(end) - min(start), unit = "hours")) %>%
    inner_join(., routes)

  #avg stops + avg stop spacing
  routes <- trips %>%  summarize(stops = n()) %>%
    group_by(route_id, direction_id) %>% summarize(stops = mean(stops)) %>%
    group_by(route_id) %>% summarize(stops = sum(stops)) %>%
    inner_join(., routes) %>%
    mutate(stopspacing = length/stops)


  #trips per week
  routes <- trips %>% inner_join(.,cal %>% select(service_id,daysofop)) %>%
    group_by(route_id, trip_id, daysofop, direction_id) %>% summarize(n = n()) %>%
    group_by(route_id) %>%  summarize(tripsperweek = sum(daysofop)) %>%
    inner_join(., routes)


  #days in service
  routes <- trips %>% group_by(route_id) %>% summarize(monday = ifelse(sum(monday) >1, 1, 0),
                                                       tuesday = ifelse(sum(tuesday)>1, 1, 0),
                                                       wednesday = ifelse(sum(wednesday)>1, 1, 0),
                                                       thursday = ifelse(sum(thursday)>1, 1, 0),
                                                       friday = ifelse(sum(friday)>1, 1, 0),
                                                       saturday = ifelse(sum(saturday)>1, 1, 0),
                                                       sunday = ifelse(sum(sunday)>1, 1, 0)) %>%
    mutate(daysinserv = monday + tuesday + wednesday + thursday + friday + saturday + sunday) %>%
    inner_join(., routes)


  #average weekday trips
  routes <- trips %>%
    summarize(x = n()) %>% inner_join(., cal %>% select(service_id, daysofop, days)) %>%
    group_by(route_id, service_id, days) %>% summarize(n = n()) %>%
    mutate(t = ifelse(days == "M-F", n,
                      ifelse(days == "Su", NA,
                             ifelse(days == "Sa", NA, n)))) %>%
    group_by(route_id) %>% summarize(avgwkdaytrips = mean(t, na.rm = T)) %>%
    inner_join(., routes)


  #average wkday, sat, sun, trips per week
  routes <- GTFS$trips %>%
    inner_join(., cal) %>%
    select(route_id, service_id, trip_id, direction_id,
           monday, tuesday, wednesday, thursday, friday, saturday, sunday, daysofop) %>%
    group_by(route_id) %>% summarize(avgwkdaytrips = sum(monday == 1),
                                     avgsattrips = sum(saturday == 1),
                                     avgsuntrips = sum(sunday == 1)) %>%
    mutate(tripsperweek = (avgwkdaytrips*5)+avgsattrips+avgsuntrips) %>%
    inner_join(., routes)


  routes <- routes %>% mutate(Agency = Agency, Month = Month, Year = Year)
  return(routes)
}

serviceALL <- rbind(service("2022-11_OmniRide_PRTC.zip", "PRTC", 11, 2022),
                     service("2022-11_VRE.zip", "VRE", 11, 2022),
                     service("2022-11_Arlington.zip", "ART", 11, 2022),
                     service("2022-11_CUE.zip", "CUE", 11, 2022),
                     service("2022-11_DASH.zip", "DASH", 11, 2022),
                     service("2022-11_Fairfax_Connector.zip", "FFX", 11, 2022),
                     service("2022-11_Loudoun.zip", "LCT", 11, 2022))


# WMATA Bus -------------------------------------------------------------------


WMATAroutesVA <- st_read("AgencyProfileData/WMATARoutesVA.csv")

GTFS_path <- file.path ("Z:",
                        "NVTC General",
                        "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data",
                        "GTFS")


#read gtfs
WMATABus <- read_gtfs(file.path(GTFS_path, "2022-11_Metrobus.zip"))
cal <- WMATABus$calendar %>%
  mutate(daysofop = monday + tuesday + wednesday + thursday + friday + saturday + sunday) %>%
  mutate(days = ifelse(daysofop == 5, "M-F", ifelse(daysofop == 4, "M-Th",
                                                    ifelse(friday == 1, "F",
                                                           ifelse(saturday == 1, "Sa",
                                                                  ifelse(sunday == 1, "Su", NA))))))

#length of route
shapes <- shapes_as_sf(WMATABus$shapes) %>%
  right_join(., WMATABus$trips %>% filter(route_id%in% WMATAroutesVA$route_d)) %>%
  mutate(dist = st_length(geometry)) %>%
  group_by(route_id, direction_id) %>%
  summarize(dist = mean(dist))
#make sure to update the units to miles (not meters)
shapes$dist <- set_units(shapes$dist, mi)


#create route file
#avg route time
trips <- WMATABus$trips %>%
  select(route_id, service_id, trip_id, direction_id) %>%
  left_join(., WMATABus$stop_times) %>%
  group_by(route_id, trip_id, service_id, direction_id)
routes <- trips %>%
  summarize(start = as_hms(min(arrival_time)),
            end = as_hms(max(departure_time))) %>%
  mutate(time = difftime(end, start)) %>%
  group_by(route_id, direction_id) %>% summarize(time = mean(time)) %>%
  group_by(route_id) %>% summarize(routetime = sum(time))
units(routes$routetime) <- "mins"

#length(miles) and directional length
routes <- shapes %>% group_by(route_id) %>% summarize(length = sum(dist), direction = n()) %>%
  st_drop_geometry() %>% mutate(length1way = length/direction) %>% select(-direction) %>%
  inner_join(., routes)

#avg speed
routes <- routes %>% mutate(speed = length/as.numeric(routetime, unit = "hours"))

#span of service
routes <- trips %>%
  summarize(start = as_hms(min(arrival_time)),
            end = as_hms(max(departure_time))) %>%
  group_by(route_id) %>% summarize(span = as.numeric(max(end) - min(start), unit = "hours")) %>%
  inner_join(., routes)

#avg stops + avg stop spacing
routes <- trips %>% summarize(stops = n()) %>%
  group_by(route_id, direction_id) %>% summarize(stops = mean(stops)) %>%
  group_by(route_id) %>% summarize(stops = sum(stops)) %>%
  inner_join(., routes) %>%
  mutate(stopspacing = length/stops)


#trips per week
routes <- trips %>% inner_join(.,cal %>% select(service_id,daysofop)) %>%
  group_by(route_id, trip_id, daysofop, direction_id) %>% summarize(n = n()) %>%
  group_by(route_id) %>%  summarize(tripsperweek = sum(daysofop))%>%
  inner_join(., routes)


#days in service
routes <- trips %>% inner_join(.,cal %>% select(service_id,daysofop)) %>%
  group_by(route_id, service_id,daysofop) %>% summarize(n = n()) %>%
  group_by(route_id) %>% summarize(daysinserv = sum(daysofop)) %>%
  inner_join(., routes)


#average weekday trips
routes <-trips %>%
  summarize(x = n()) %>% inner_join(., cal %>% select(service_id,daysofop, days)) %>%
  group_by(route_id, service_id, days) %>% summarize(n = n()) %>%
  mutate(t = ifelse(days == "M-F", n,
                    ifelse(days == "Su", NA,
                           ifelse(days == "Sa", NA, n)))) %>%
  group_by(route_id) %>% summarize(avgwkdaytrips = mean(t, na.rm = T)) %>%
  inner_join(., routes)


#average sat trips
routes <- trips %>%
  summarize(x = n()) %>% inner_join(., cal %>% select(service_id,daysofop, days)) %>%
  group_by(route_id, service_id, days) %>% summarize(n = n())  %>%
  mutate(t = ifelse(days == "M-F", NA,
                    ifelse(days == "Su", NA,
                           ifelse(days == "M-Th", NA,
                                  ifelse(days == "F", NA, n))))) %>%
  group_by(route_id) %>% summarize(avgsattrips = sum(t, na.rm = T)) %>%
  inner_join(., routes)

#average sun trips
routes <- trips %>%
  summarize(x = n()) %>% inner_join(., cal %>% select(service_id,daysofop, days)) %>%
  group_by(route_id, service_id, days) %>% summarize(n = n()) %>%
  mutate(t = ifelse(days == "M-F", NA,
                    ifelse(days == "Sa", NA,
                           ifelse(days == "M-Th", NA,
                                  ifelse(days == "F", NA, n))))) %>%
  group_by(route_id) %>% summarize(avgsuntrips = sum(t, na.rm = T)) %>%
  inner_join(., routes)

MetrobusService <- routes


# WMATA Rail --------------------------------------------------------------

WMATARail <- read_gtfs(file.path(GTFS_path, "2022-11_Metrorail.zip"))

Nova <- st_read("data/Nova.shp")

vashpids <- shapes_as_sf(WMATARail$shapes) %>% st_intersection(., Nova) %>%
  select(shape_id) %>% st_drop_geometry()
st_write(vashpids, "AgencyProfileData/VaRailShapeIDs.csv")


WMATARail$shapes <- shapes_as_sf(WMATARail$shapes)
shapes <- WMATARail$shapes %>% filter(shape_id %in% vashpids$shape_id) %>%
  inner_join(., WMATARail$trips %>% filter(shape_id %in% vashpids$shape_id)) %>%
  mutate(dist = st_length(geometry)) %>%
  group_by(route_id, direction_id) %>%
  summarize(dist = mean(dist))
#make sure to update the units to miles (not meters)
shapes$dist <- set_units(shapes$dist, mi)


#create calendar
cal <- WMATARail$calendar_dates %>% mutate(DoW = str_to_lower(wday(date, label = T, abbr = F))) %>%
  group_by(service_id, DoW) %>% summarize(n = 1) %>%
  pivot_wider(names_from = DoW, values_from = n) %>% replace(is.na(.), 0) %>%
  mutate(daysofop = monday + tuesday + wednesday + thursday + friday + saturday + sunday)


#trips
trips <- WMATARail$trips %>% filter(shape_id %in% vashpids$shape_id) %>%
  inner_join(., cal) %>%
  select(route_id, service_id, trip_id, direction_id,
         monday, tuesday, wednesday, thursday, friday, saturday, sunday, daysofop) %>%
  left_join(., WMATARail$stop_times) %>%
  group_by(route_id, trip_id, service_id, direction_id)


#routes
routes <- trips %>%
  summarize(start = as_hms(min(arrival_time)),
            end = as_hms(max(departure_time))) %>%
  mutate(time = difftime(end, start)) %>%
  group_by(route_id, direction_id) %>% summarize(time = mean(time)) %>%
  group_by(route_id) %>% summarize(routetime = sum(time))
units(routes$routetime) <- "mins"


#length(miles) and directional length
routes <- shapes %>% group_by(route_id) %>% summarize(length = sum(dist), direction = n()) %>%
  st_drop_geometry() %>% mutate(length1way = length/direction) %>% select(-direction) %>%
  inner_join(., routes)

#avg speed
routes <- routes %>% mutate(speed = length/as.numeric(routetime, unit = "hours"))

#span of service
routes <- trips %>%
  summarize(start = as_hms(min(arrival_time)),
            end = as_hms(max(departure_time))) %>%
  group_by(route_id) %>% summarize(span = as.numeric(mx(end) - min(start), unit = "hours")) %>%
  inner_join(., routes)

#avg stops + avg stop spacing
routes <- trips %>% summarize(stops = n()) %>%
  group_by(route_id, direction_id) %>% summarize(stops = mean(stops)) %>%
  group_by(route_id) %>% summarize(stops = sum(stops)) %>%
  inner_join(., routes) %>%
  mutate(stopspacing = length/stops)


#days in service
routes <- trips %>% group_by(route_id) %>% summarize(monday = ifelse(sum(monday) >1, 1, 0),
                                                     tuesday = ifelse(sum(tuesday)>1, 1, 0),
                                                     wednesday = ifelse(sum(wednesday)>1, 1, 0),
                                                     thursday = ifelse(sum(thursday)>1, 1, 0),
                                                     friday = ifelse(sum(friday)>1, 1, 0),
                                                     saturday = ifelse(sum(saturday)>1, 1, 0),
                                                     sunday = ifelse(sum(sunday)>1, 1, 0)) %>%
  mutate(daysinserv = monday + tuesday + wednesday + thursday + friday + saturday + sunday) %>%
  inner_join(., routes)


#average weekday, sat, sun, trips per week
routes <- WMATARail$trips %>% filter(shape_id %in% vashpids$shape_id) %>%
  inner_join(., cal) %>%
  select(route_id, service_id, trip_id, direction_id,
         monday, tuesday, wednesday, thursday, friday, saturday, sunday, daysofop) %>%
  group_by(route_id) %>% summarize(avgwkdaytrips = sum(monday == 1),
                                   avgsattrips = sum(saturday == 1),
                                   avgsuntrips = sum(sunday == 1)) %>%
  mutate(tripsperweek = (avgwkdaytrips*5)+avgsattrips+avgsuntrips) %>%
  inner_join(., routes)

# Combine all data --------------------------------------------------------

all <- MetrobusService %>% mutate(Agency = "WMATA", Month = 11, Year = 2022)  %>%
  rbind(., serviceALL) %>%
  unite("newroute_id", route_id, Agency, sep = "_", remove = F) %>%
  mutate(routetime = as.double(routetime)) %>%
  select(Agency, Year, Month, newroute_id, route_id,
         daysinserv, span, length, length1way, stops, stopspacing, routetime,
         speed, tripsperweek, avgwkdaytrips, avgsattrips, avgsuntrips) %>% arrange(Agency)

st_write(all, "AgencyProfileData/service_2022-11.xlsx")


# Agency Level Stops Trips and Routes for AgencyName------------------------------------

agencylevel <- function(GTFSzip, Agency){
  message(Agency)
  GTFS <- read_gtfs(file.path(GTFS_path, GTFSzip))
  x <-  cbind(GTFS$stops %>% count(),
              GTFS$routes %>% count(),
              GTFS$trips %>%
                select(route_id, service_id, trip_id, direction_id) %>%
                left_join(., GTFS$stop_times) %>%
                group_by(route_id, trip_id, service_id, direction_id) %>%
                inner_join(.,GTFS$calendar %>%
                             mutate_at(c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'), as.numeric) %>%
                             mutate(daysofop = monday + tuesday + wednesday + thursday + friday + saturday + sunday)) %>%
                group_by(route_id, trip_id, daysofop, direction_id) %>% summarize(n = n()) %>%
                group_by(route_id) %>%  summarize(tripsperweek = sum(daysofop)) %>% summarize(sum = sum(tripsperweek)),
              Agency)
  names(x) <- c("stops", "routes","tripsperweek", "agency")
  return(x)
}
agencylevel <- rbind(agencylevel("2022-11_OmniRide_PRTC.zip", "PRTC"),
                     agencylevel("2022-11_VRE.zip", "VRE"),
                     agencylevel("2022-11_Arlington.zip", "ART"),
                     agencylevel("2022-11_CUE.zip", "CUE"),
                     agencylevel("2022-11_DASH.zip", "DASH"),
                     agencylevel("2022-11_Fairfax_Connector.zip", "FFX"),
                     agencylevel("2022-11_Loudoun.zip", "LCT"))
#wmata
WMATABus <- read_gtfs(file.path(GTFS_path, "2022_11_Metrobus.zip"))
WMATAroutesVA <- st_read("AgencyProfileData/WMATARoutesVA.csv")
count(WMATAroutesVA)
Nova <- st_read("data/Nova.shp")
mbstops <- st_intersection(stops_as_sf(WMATABus$stops), Nova) %>% count()
#rail stops
Metrorail <- read_gtfs(file.path(GTFS_path, "2022-11_Metrorail.zip"))

railstops <- stops_as_sf(Metrorail$stops) %>% filter(grepl("STN_", stop_id)) %>%
  st_intersection(., Nova)


