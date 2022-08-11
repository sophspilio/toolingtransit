# How the pandemic changed transit quantity in Northern Virginia -----

## Set up workspace

library(tidyverse)
library(here)
library(censusapi)
library(tidycensus)
library(sf)
library(units)
library(tidytransit)
library(toolingtransit)
library(mapview)
library(lubridate)

#reload variables
CensusDataBG <- st_read("data/CensusData.shp")
CensusDataTracts <- st_read("AgencyProfileData/CensusDataTracts.shp")
NovaStops_2019 <- st_read("data/NovaStops_2019.shp")
NovaStops_2022 <- st_read("AgencyProfileData/NovaStops_2022.shp")
Nova <- st_read("data/Nova.shp")




# 2019 data prep ---------------------------------------------------------------

#function to join together all stops with the routes and count stops per week
route_n_stop <- function(gtfszip, agency){
  GTFS_path <- file.path ("Z:",
                          "NVTC General",
                          "Projects and Programs",
                          "Transit Resource Center (TRC)",
                          "Data",
                          "GTFS")
  GTFS <- read_gtfs(file.path(GTFS_path, gtfszip))
  #need service and number of days that service_ids use (multiplier)
  Service <- GTFS$calendar %>%
    mutate(multiplier = monday + tuesday + wednesday + thursday + friday + saturday + sunday)
  #stop_times has both the trip_id and stop_id
  Stops <- GTFS$stop_times %>% select(trip_id, stop_id)
  #routes has trip_id and route_id
  #don't filter by service_id except for VRE
  if (agency == "VRE") {
    Trip <- GTFS$trips %>% filter(service_id == "Regular") %>%
      select(trip_id, route_id, service_id) %>%
      left_join(., Service %>% select(service_id, multiplier))
  } else {
    Trip <- GTFS$trips %>%
      select(trip_id, route_id, service_id) %>%
      left_join(., Service %>% select(service_id, multiplier))
  }
  #what we need is route_id and stop_id, if we join we can bring it all together
  #then select just the columns we need
  Join <- inner_join(Stops, Trip) %>%
    #find count of trips by route_id, stop_id
    group_by(route_id, stop_id, service_id, multiplier) %>% summarize(trips = n()) %>%
    #if service_id is mutli-day, multiply by number of days of service
    mutate(tripfreq = trips*multiplier ) %>%
    #find sum of trip count for each route_id/stop_id
    group_by(route_id, stop_id) %>% summarize(tripfreq = sum(tripfreq)) %>%
    #add column for agency name
    mutate(agency = agency) %>%
    select(route_id, stop_id, tripfreq, agency)
  return(Join)
}
#run route_n_stop for all agencies
ARTjoin_2019 <- route_n_stop("2019-10_Arlington.zip", "ART")
CUEjoin_2019 <- route_n_stop("2020-12_CUE.zip", "CUE")
DASHjoin_2019 <- route_n_stop("2019-10_DASH.zip", "DASH")


#Fairfax must be done separately as the service_ids vary
FFX <- read_gtfs(file.path(GTFS_path, "2019-09_Fairfax_Connector.zip"))
FFXStops <-  FFX$stop_times %>% select(trip_id, stop_id)
FFXService <- FFX$calendar %>%
  mutate(multiplier = monday + tuesday + wednesday + thursday + friday + saturday + sunday)

FFXTrip <- FFX$trips %>% filter(service_id == 2 | service_id == 3 | service_id == 4) %>%
  select(trip_id, route_id, service_id) %>%
  left_join(., FFXService %>% select(service_id, multiplier))

FFXJoin19 <- inner_join(FFXStops, FFXTrip) %>%
  #find count of trips by route_id, stop_id
  group_by(route_id, stop_id, service_id, multiplier) %>% summarize(trips = n()) %>%
  #if service_id is mutli-day, multiply by number of days of service
  mutate(tripfreq = trips*multiplier ) %>%
  #find sum of trip count for each route_id/stop_id
  group_by(route_id, stop_id) %>% summarize(tripfreq = sum(tripfreq)) %>%
  #add column for agency name
  mutate(agency = "FFX") %>%
  select(route_id, stop_id, tripfreq, agency)


# Metrobus must be done separately
Metrobus2019 <- read_gtfs(file.path(GTFS_path, "2019-10_WMATA.zip"))

#need service and number of days that service_ids use (multiplier)
MetrobusService19 <- Metrobus2019$calendar %>%
  mutate(DOW = wday(date, label = T)) %>%
  group_by(service_id, DOW) %>% summarize(sum  = sum(exception_type)) %>%
  filter(service_id %in% c(28,29,30,7)) %>%
  summarize(multiplier = n())


# stops: spatial  filter to nova
MetrobusStops <- stops_as_sf(Metrobus2019$stops) %>%
  #spatially filter to only the stops in va
  st_intersection(.,Nova) %>%
  st_drop_geometry() %>%
  left_join(., Metrobus2019$stop_times) %>% select(trip_id, stop_id)

# trips
MetrobusTrips <- Metrobus2019$trips %>% filter(service_id %in% c(28,29,30,7)) %>%
  #remove trips for rail
  filter(!route_id %in% c("RED", "BLUE", "ORANGE", "SILVER", "YELLOW", "GREEN")) %>%
  select(trip_id, route_id, service_id) %>%
  left_join(., MetrobusService19 %>% select(service_id, multiplier))

#join stops and trips
MetrobusJoin <- inner_join(MetrobusStops, MetrobusTrips) %>%
  #find count of trips by route_id, stop_id
  group_by(route_id, stop_id, service_id, multiplier) %>% summarize(trips = n()) %>%
  #if service_id is mutli-day, multiply by number of days of service
  mutate(tripfreq = trips*multiplier ) %>%
  #find sum of trip count for each route_id/stop_id
  group_by(route_id, stop_id) %>% summarize(tripfreq = sum(tripfreq)) %>%
  #add column for agency name
  mutate(agency = "WMATA") %>%
  select(route_id, stop_id, tripfreq, agency)

#join all agencies together
#reminder: no LCT or VRE or WMATA rail
Alljoin_2019 <- rbind(ARTjoin_2019,
                      CUEjoin_2019,
                      DASHjoin_2019,
                      FFXJoin19,
                      MetrobusJoin) %>%
  mutate(Agency = agency) %>%
  unite(stop_id, agency, col = "newstop_id")

#join with stop data to create spatial dataset (NovaStops data is spatial, Alljoin is not)
#create combined route and stop ids
NovaStopsRoutes_2019 <- inner_join(Alljoin_2019, NovaStops_2019) %>%
  st_as_sf() %>%
  mutate(agency = Agency) %>%
  unite(route_id, Agency, sep = "_", col = "newroute_id")

st_write(NovaStopsRoutes_2019, "data/NovaStopsRoutes_2019.shp", append = FALSE)

NovaStopsRoutes_2019 <- st_read("data/NovaStopsRoutes_2019.shp")
# 2022 data prep --------------------------------------------------------------------
#remove VRE, Metrorail, Loudoun from RoutesStops file
NovaStopsRoutes_2022 <- st_read("AgencyProfileData/NovaStopsRoutes_2022.shp") %>%
  filter(AgncyNm != "VRE", AgncyNm != "LCT", Mode != "HR")



# Amount of Service 2019-2022 ---------------------------------------------


#function to find the TSI for each census block group in a given area
amount_service <- function(county, stopsfile){
  #create county shape and transform
  CountyShp <- county %>% select(geometry)

  #summarize stop frequency at the route level
  Ag_County <- stopsfile %>% group_by(newrt_d) %>% summarize(stopfreq = mean(tripfrq))

  #buffer the stops, conditional if its a commuter rail (1 mile radius) or bus (1/4 mi)
  Ag_CountyBuff <- Ag_County %>% st_buffer(.,400) %>%
    mutate(area = st_area(.))
  #set units to square miles

  Ag_CountyBuff$area <- set_units(Ag_CountyBuff$area, mi^2)
  #calculate area times stop frequency
  #select only numeric values for interpolation
  Ag_CountyBuff <- Ag_CountyBuff %>% mutate(calcTSI = area*stopfreq) %>% select(calcTSI)

  #interpolate "TSI" for each census block group
  CountyInterpol <- st_interpolate_aw(Ag_CountyBuff, CountyShp, extensive = T) %>%
    mutate(area = st_area(.))
  CountyInterpol$area <- set_units(CountyInterpol$area, mi^2)


  #calculate TSI
  CountyInterpol <- CountyInterpol %>% mutate(TSI = calcTSI/area) %>% select(TSI, area)
  #join with census data to have associated GEOIDs
  #AmountService <- st_join(CountyInterpol, CountyShp, join = st_equals)

  return(CountyInterpol)
}

#Nova Counties excluding Loudoun
counties <- countycensus(c("Arlington", "Fairfax city", "Fairfax County", "Alexandria city", "Falls Church city")) %>% st_transform(crs = 4326)

#run for 2022
Nova22amountservice <- amount_service(counties, NovaStopsRoutes_2022)
st_write(Nova22amountservice, "data/Nova22amountserviceAGAIN.shp")

#run for 2019
Nova19amountservice <- amount_service(counties, NovaStopsRoute_2019)
st_write(Nova19amountservice, "data/Nova19amountservice.shp")
