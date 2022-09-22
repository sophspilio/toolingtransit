#Service calculations for Transit Agency Profiles

#load libraries
library(tidyverse)
library(here)
library(censusapi)
library(tidycensus)
library(sf)
library(units)
library(tidytransit)
library(toolingtransit)
library(mapview)
library(hms)


#test with ART

#shape
ART <- read_gtfs(file.path(GTFS_path, "2022-04_Arlington.zip"))
#find length of route
shapes <- shapes_as_sf(ART$shapes) %>%
  left_join(., ART$trips) %>%
  mutate(dist = st_length(geometry)) %>%
  group_by(route_id, direction_id) %>%
  summarize(dist = mean(dist))
#make sure to update the units to miles (not meters)
shapes$dist <- set_units(shapes$dist, mi)

#trips
ART$calendar <- ART$calendar %>%
  mutate(daysofop = monday + tuesday + wednesday + thursday + friday + saturday + sunday)

trips <- ART$trips %>% left_join(., ART$calendar) %>% select(route_id, service_id, trip_id, direction_id) %>% left_join(., ART$stop_times) %>%
  #start,end, and difftime
  group_by(route_id, trip_id, service_id, direction_id) %>%
  summarize(start = as_hms(min(arrival_time)),
            end = as_hms(max(departure_time)),
            stops = max(stop_sequence))%>%
  mutate(time = difftime(end, start)) %>%
  #days of operation
  left_join(., ART$calendar %>% select(service_id, daysofop))



#length(miles)
routes <- shapes %>% group_by(route_id) %>% summarize(length = sum(dist)) %>% st_drop_geometry()

#span of service
routes <- trips %>% group_by(route_id) %>% summarize(span = as.numeric(max(end) - min(start), unit = "hours")) %>% inner_join(., routes)

#avg directional length
routes <- shapes %>% group_by(route_id) %>%
  #determine if route is bidirectional
  summarize(n = n()) %>% st_drop_geometry() %>%
  #determine length of one direction of route
  inner_join(., routes) %>% mutate(length1way = length/n)


#avg stops + avg stop spacing
routes <- trips %>% group_by(route_id, direction_id) %>% summarize(stops = mean(stops)) %>%
  group_by(route_id) %>% summarize(stops = sum(stops)) %>%
  inner_join(., routes) %>%
  mutate(stopspacing = length/stops)


#avg route time
routes <- trips %>% group_by(route_id, direction_id) %>% summarize(time = mean(time)) %>%
  group_by(route_id) %>% summarize(routetime = sum(time)) %>% inner_join(., routes) %>%
  #avg speed (mph)
  mutate(speed = length/as.numeric(routetime, unit = "hours"))


#trips per week
routes <- trips %>%  select(route_id, service_id, daysofop) %>% group_by(route_id) %>% summarize(tripsperweek = sum(daysofop)) %>% inner_join(., routes)

#days in service
trips

#average weekday trips
ART$calendar
