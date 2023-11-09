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
                        "GTFS",
                        "2023")

Nova <- st_read("data/Nova.shp")
#MAKE SURE TO UPDATE GTFS ZIP NAMES AS NEW DATA IS DOWNLOADED!!
ARTzip <- "2023-10_Arlington.zip"
CUEzip <- "2023-10_CUE.zip"
DASHzip <- "2023-10_DASH.zip"
FFXzip <- "2023-10_Fairfax_Connector.zip"
LCTzip <- "2023-10_Loudoun.zip"
PRTCzip <- "2023-10_OmniRide_PRTC.zip"
VREzip <- "2023-10_VRE.zip"
Metrobuszip <- "2023-10_Metrobus.zip"
Metrorailzip <- "2023-10_Metrorail.zip"


#service
service <- function(GTFSzip, Agency, Month, Year){
  #set up
  message(Agency)
  GTFS_path <- file.path ("Z:",
                          "NVTC General",
                          "Projects and Programs",
                          "Transit Resource Center (TRC)",
                          "Data",
                          "GTFS",
                          "2023")

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


  routes <- routes %>% mutate(Agency = Agency, Month = Month, Year = Year)
  return(routes)
}

serviceALL <- rbind(service(PRTCzip, "PRTC", 07, 2023),
                     service(VREzip, "VRE", 07, 2023),
                     service(ARTzip, "ART", 07, 2023),
                     service(CUEzip, "CUE", 07, 2023),
                     service(DASHzip, "DASH", 07, 2023),
                     service(FFXzip, "FFX", 07, 2023),
                     service(LCTzip, "LCT", 07, 2023))

# WMATA Setup -------------------------------------------------------------------
#read gtfs
WMATABus <- read_gtfs(file.path(GTFS_path, Metrobuszip))
WMATARail <- read_gtfs(file.path(GTFS_path, Metrorailzip))

#Metrobus in VA
VAMetrobus <- inner_join(shapes_as_sf(WMATABus$shapes),
                         WMATABus$trips %>% select(shape_id, route_id) %>% distinct()) %>%
  st_intersection(Nova) %>% st_drop_geometry() %>% select(route_id) %>% distinct() %>%
  mutate(Agency = "WMATA", Mode = "Bus")

#Metrorail in VA
VAMetrorail <- inner_join(shapes_as_sf(WMATARail$shapes),
                          WMATARail$trips %>% select(shape_id, route_id) %>% distinct()) %>%
  st_intersection(Nova) %>% st_drop_geometry() %>% select(route_id) %>% distinct() %>%
  mutate(Agency = "WMATA", Mode = "HR")


WMATAroutesVA <- rbind(VAMetrobus, VAMetrorail)

# WMATA Bus ---------------------------------------------------------------

cal <- WMATABus$calendar %>%
  mutate(daysofop = monday + tuesday + wednesday + thursday + friday + saturday + sunday) %>%
  mutate(days = ifelse(daysofop == 5, "M-F", ifelse(daysofop == 4, "M-Th",
                                                    ifelse(friday == 1, "F",
                                                           ifelse(saturday == 1, "Sa",
                                                                  ifelse(sunday == 1, "Su", NA))))))


#length of route
shapes <- shapes_as_sf(WMATABus$shapes) %>%
  right_join(., WMATABus$trips %>% filter(route_id%in% WMATAroutesVA$route_id)) %>%
  mutate(dist = st_length(geometry)) %>%
  group_by(route_id, direction_id) %>%
  summarize(dist = mean(dist))
#make sure to update the units to miles (not meters)
shapes$dist <- set_units(shapes$dist, mi)


#create route file
#avg route time
trips <- WMATABus$trips %>% inner_join(., cal) %>%
  select(route_id, service_id, trip_id, direction_id,
         monday, tuesday, wednesday, thursday, friday, saturday, sunday) %>%
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
#routes <- trips %>% inner_join(.,cal %>% select(service_id,daysofop)) %>%
 # group_by(route_id, service_id,daysofop) %>% summarize(n = n()) %>%
  #group_by(route_id) %>% summarize(daysinserv = sum(daysofop)) %>%
  #inner_join(., routes)

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
routes <-trips %>%
  summarize(x = n()) %>% inner_join(., cal) %>%
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

vashpids <- shapes_as_sf(WMATARail$shapes) %>% st_intersection(., Nova) %>%
  select(shape_id) %>% st_drop_geometry()
#st_write(vashpids, "AgencyProfileData/VaRailShapeIDs.csv")


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
  mutate(daysofop = monday + tuesday + wednesday + thursday + friday + saturday + sunday) %>%
  mutate(days = ifelse(daysofop == 5, "M-F", ifelse(monday == 1 & tuesday == 1 & wednesday == 1 & thursday == 1, "M-Th",
                                                  ifelse(friday == 1, "F",
                                                         ifelse(saturday == 1, "Sa",
                                                                ifelse(sunday == 1, "Su", NA))))))

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
  group_by(route_id) %>% summarize(span = as.numeric(max(end) - min(start), unit = "hours")) %>%
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



#average weekday trips
routes <- WMATARail$trips %>% filter(shape_id %in% vashpids$shape_id) %>%
  inner_join(., cal %>% select(service_id, daysofop, days)) %>%
  group_by(route_id, service_id, direction_id) %>%
  summarize(wkday = sum(days == "M-Th")) %>% filter(wkday > 0) %>%
  group_by(route_id) %>% summarize(avgwkdaytrips = mean(wkday)) %>%
  inner_join(., routes)

#average sat trips
routes <- WMATARail$trips %>% filter(shape_id %in% vashpids$shape_id) %>%
  inner_join(., cal %>% select(service_id, daysofop, days)) %>%
  group_by(route_id, service_id, direction_id) %>%
  summarize(sat = sum(days == "Sa")) %>% filter(sat > 0) %>%
  group_by(route_id) %>% summarize(avgsattrips = mean(sat)) %>%
  inner_join(., routes)


#average sun trips + trips per week
routes <- WMATARail$trips %>% filter(shape_id %in% vashpids$shape_id) %>%
  inner_join(., cal %>% select(service_id, daysofop, days)) %>%
  group_by(route_id, service_id, direction_id) %>%
  summarize(sun = sum(days == "Su")) %>% filter(sun > 0) %>%
  group_by(route_id) %>% summarize(avgsuntrips = mean(sun)) %>%
  inner_join(., routes) %>%
  mutate(tripsperweek = avgsattrips + avgsuntrips + (avgwkdaytrips * 5))


MetrorailService <- routes

# Combine all data --------------------------------------------------------
Metroservice <- rbind(MetrobusService, MetrorailService) %>%
  mutate(Agency = "WMATA", Month = 07, Year = 2023)
all <- rbind(Metroservice, serviceALL) %>%
  unite("newroute_id", route_id, Agency, sep = "_", remove = F) %>%
  mutate(routetime = as.double(routetime)) %>%
  select(Agency, Year, Month, newroute_id, route_id,
         daysinserv, span, length, length1way, stops, stopspacing, routetime,
         speed, tripsperweek, avgwkdaytrips, avgsattrips, avgsuntrips) %>% arrange(Agency)

st_write(all, "AgencyProfileData/service_2023-10.xlsx", delete_dsn = TRUE)


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
agencylevel <- rbind(agencylevel(PRTCzip, "PRTC"),
                     agencylevel(VREzip, "VRE"),
                     agencylevel(ARTzip, "ART"),
                     agencylevel(CUEzip, "CUE"),
                     agencylevel(DASHzip, "DASH"),
                     agencylevel(FFXzip, "FFX"),
                     agencylevel(LCTzip, "LCT"))

#WMATA

Nova <- st_read("data/Nova.shp")

#Metrobus: Stops, Routes, Trips
MBstops <- st_intersection(stops_as_sf(WMATABus$stops), Nova) %>% st_drop_geometry() %>% count()
MBroutes <- inner_join(shapes_as_sf(WMATABus$shapes),
                         WMATABus$trips %>% select(shape_id, route_id) %>% distinct()) %>%
  st_intersection(Nova) %>% st_drop_geometry() %>% select(route_id) %>% count()

#stops
rbind(st_intersection(stops_as_sf(WMATABus$stops), Nova) %>% st_drop_geometry() %>% count(),
      stops_as_sf(WMATARail$stops) %>% filter(grepl("STN_", stop_id)) %>%
        st_intersection(., Nova) %>% st_drop_geometry() %>% count()
      )

#routes
rbind(inner_join(shapes_as_sf(WMATABus$shapes),
                 WMATABus$trips %>% select(shape_id, route_id) %>% distinct()) %>%
        st_intersection(Nova) %>% st_drop_geometry() %>% select(route_id) %>% distinct() %>% count(),
      inner_join(WMATARail$shapes,
                 WMATARail$trips %>% select(shape_id, route_id) %>% distinct()) %>%
        st_intersection(Nova) %>% st_drop_geometry() %>% select(route_id) %>% distinct() %>% count()

)

rbind("Metrobus", "Metrorail")

cbind(
  #type
  rbind("Metrobus", "Metrorail"),
  #stops
  rbind(st_intersection(stops_as_sf(WMATABus$stops), Nova) %>% st_drop_geometry() %>% count(),
        stops_as_sf(WMATARail$stops) %>% filter(grepl("STN_", stop_id)) %>%
          st_intersection(., Nova) %>% st_drop_geometry() %>% count()),
  #routes
  rbind(inner_join(shapes_as_sf(WMATABus$shapes),
                   WMATABus$trips %>% select(shape_id, route_id) %>% distinct()) %>%
          st_intersection(Nova) %>% st_drop_geometry() %>% select(route_id) %>% distinct() %>% count(),
        inner_join(WMATARail$shapes,
                   WMATARail$trips %>% select(shape_id, route_id) %>% distinct()) %>%
          st_intersection(Nova) %>% st_drop_geometry() %>% select(route_id) %>% distinct() %>% count()

  )
  ) %>%
  rename(type = 1, stops = 2, routes = 3) %>% suppressWarnings()
#trips from service calcs above

#Metrorail: Stops, Routes, Trips
railstops <- stops_as_sf(WMATARail$stops) %>% filter(grepl("STN_", stop_id)) %>%
  st_intersection(., Nova)
railstops %>% count()

