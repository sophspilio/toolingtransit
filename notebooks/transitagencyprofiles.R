#Transit Agency Profiles

library(tidyverse)
library(here)
library(censusapi)
library(tidycensus)
library(sf)
library(units)
library(tidytransit)
library(toolingtransit)
library(mapview)
#reload data
CensusDataBG <- st_read("data/CensusData.shp")
CensusDataTracts <- st_read("AgencyProfileData/CensusDataTracts.shp")
NovaStopsRoutes_2022 <- st_read("AgencyProfileData/NovaStopsRoutes_2022.shp")
NovaStops_2022 <- st_read("AgencyProfileData/NovaStops_2022.shp")
Nova <- st_read("data/Nova.shp")
# Census Tracts -----------------------------------------------------------

#function to get demographic data for CENSUS TRACTS
countycensus_tract <- function(County) {
  #load required libraries
  require(tidyverse)
  require(censusapi)
  require(sf)
  require(units)
  #pull census acs data for 2020, select variables for county
  CensusCnty <- get_acs(
    geography = "tract",
    year = 2020,
    variables = c(TotalPopulation = "B01003_001",
                  TotalCommuters = "B08301_001",
                  DriveAlone = "B08301_003",
                  PublicTransport = "B08301_010",
                  HHCount = "B25044_001",
                  PovertyRatioTotal = "C17002_001",
                  PovertyOver200Pct = "C17002_008",
                  HHOwn0Veh = "B25044_003",
                  HHRent0Veh = "B25044_010",
                  WhitePop = "B02001_002"),
    state = "VA",
    county = County,
    geometry = TRUE,
    output = "wide"
  ) %>%
    #calculate new variables
    mutate(Count0Car = HHOwn0VehE + HHRent0VehE,
           NonWhitePop = TotalPopulationE - WhitePopE,
           Under200PctPoverty = PovertyRatioTotalE - PovertyOver200PctE,
           area = st_area(.)) %>%
    #select only necessary variables (remove margin of error columns)
    select(TotalPopulationE, HHCountE, Count0Car, TotalCommutersE, PublicTransportE, NonWhitePop, Under200PctPoverty, area, GEOID)
  #set units for the area as square miles
  CensusCnty$area <- set_units(CensusCnty$area, mi^2)
  return(CensusCnty)
}

NOVA_Counties <-  c("Arlington",
                    "Fairfax County",
                    "Fairfax city",
                    "Loudoun",
                    "Alexandria City",
                    "Falls Church City")

#create census data for the whole region using CENSUS TRACTS
CensusDataTracts <- countycensus_tract(NOVA_Counties)
st_write(CensusDataTracts, "AgencyProfileData/CensusDataTracts.shp")


# NovaStops 2022 ---------------------------------------------------------------
#  WITH LOUDOUN AND VRE, NO WMATA

#Join 2022 stops
NovaStops_2022 <- rbind(
  stoploc("2022-04_Arlington.zip", "ART"),
  stoploc("2022-03_CUE.zip", "CUE"),
  stoploc("2022-04_DASH.zip", "DASH"),
  stoploc("2022-03_Fairfax_Connector.zip", "FFX"),
  stoploc("2022-07_Loudoun.zip", "LCT"),
  stoploc("2022-03_VRE.zip", "VRE")
)

#create columns for Agency name and for a newstop id that includes agency
NovaStops_2022 <- NovaStops_2022 %>% mutate(AgencyName = Agency) %>% unite(stop_id, Agency, col = "newstop_id")


# NovaStopsRoutes 2022 ----------------------------------------------------
# WITH LOUDOUN and VRE, NO WMATA

#function to find number of trips for each stop and join with routes
route_n_stop <- function(gtfszip, agency){
  GTFS_path <- file.path ("Z:",
                          "NVTC General",
                          "Projects and Programs",
                          "Transit Resource Center (TRC)",
                          "Data",
                          "GTFS")
  GTFS <- read_gtfs(file.path(GTFS_path, gtfszip))
  #stop_times has both the trip_id and stop_id
  StopTrip <- GTFS$stop_times %>% select(trip_id, stop_id)
  #routes has trip_id and route_id
  RouteTrip <- GTFS$trips %>% select(trip_id, route_id)
  #what we need is route_id and stop_id, if we join we can bring it all together
  #then select just the columns we need
  Join <- full_join(StopTrip, RouteTrip) %>%
    nest(trips = trip_id) %>%
    rowwise() %>%
    mutate(tripfreq = nrow(trips)) %>% select(route_id, stop_id, tripfreq) %>% mutate(agency = agency)
  return(Join)
}

#run above function for each agency
ARTjoin_2022 <- route_n_stop("2022-04_Arlington.zip", "ART")
CUEjoin_2022 <- route_n_stop("2022-03_CUE.zip", "CUE")
DASHjoin_2022 <- route_n_stop("2022-04_DASH.zip", "DASH")
FFXjoin_2022 <- route_n_stop("2022-03_Fairfax_Connector.zip", "FFX")
Loujoin_2022 <- route_n_stop("2022-03_Loudoun.zip", "LCT")
VREjoin_2022 <- route_n_stop("2022-03_VRE.zip", "VRE")

#join all agencies
Alljoin_2022 <- rbind(ARTjoin_2022, CUEjoin_2022, DASHjoin_2022, FFXjoin_2022, VREjoin_2022, Loujoin_2022) %>% unite(stop_id, agency, col = "newstop_id")

#join with stop data to create spatial dataset (NovaStops data is spatial, Alljoin is not)
#create combined route and stop ids
NovaStopsRoutes_2022 <- merge(Alljoin_2022, NovaStops_2022) %>%
  st_as_sf() %>%
  mutate(Agency = AgencyName) %>% mutate(rte_id = route_id) %>%
  unite(route_id, Agency, sep = "_", col = "newroute_id")%>%
  mutate(Mode = ifelse(AgencyName == 'VRE', "CR", "Bus"))


#save data separately since this includes LCT and VRE and excludes WMATA
st_write(NovaStopsRoutes_2022, "AgencyProfileData/NovaStopsRoutes_2022.shp", append= FALSE)
st_write(NovaStops_2022, "AgencyProfileData/NovaStops_2022.shp")


# NovaStops 2022 ----------------------------------------------------------
# INCLUDES WMATA rail and bus
GTFS_path <- file.path ("Z:",
                        "NVTC General",
                        "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data",
                        "GTFS")
Metrobus2022 <- read_gtfs(file.path(GTFS_path, "2022-04_WMATA.zip"))
MetrobusStops <- stops_as_sf(Metrobus2022$stops) %>%
  st_intersection(., Nova) %>%
  select(stop_id) %>%
  mutate(Agency = "WMATA") %>% unite(stop_id, Agency, col = "newstop_id") %>%
  mutate(AgencyName = "WMATA")

Metrorailstops <- read.csv(file.path(GTFS_path, "2022-06_WMATA-Railstops.csv")) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% filter(grepl("ENT_", stop_id)) %>%
  select(ï..VirginiaStation, stop_id)
# which stops do i use?


NovaStops_2022wmata <- rbind(NovaStops_2022, MetrobusStops)

#rail will be brougt in separately at the end

# NovaStopsRoutes w WMATA -------------------------------------------------

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


#run above function for each agency (wmata will be run separately)
ARTjoin_2022 <- route_n_stop("2022-04_Arlington.zip", "ART")
CUEjoin_2022 <- route_n_stop("2022-03_CUE.zip", "CUE")
DASHjoin_2022 <- route_n_stop("2022-04_DASH.zip", "DASH")
FFXjoin_2022 <- route_n_stop("2022-03_Fairfax_Connector.zip", "FFX")
LCTjoin_2022 <- route_n_stop("2022-07_Loudoun.zip", "LCT") #ISSUES wTIH TRIPFREQ
VREjoin_2022 <- route_n_stop("2022-03_VRE.zip", "VRE")

#Metrobus route_n_stop join
Metrobus2022 <- read_gtfs(file.path(GTFS_path, "2022-04_WMATA.zip"))

#need service and number of days that service_ids use (multiplier)
MetrobusService <- Metrobus2022$calendar %>%
  mutate(multiplier = monday + tuesday + wednesday + thursday + friday + saturday + sunday)

# stops: spatial  filter to nova
MetrobusStops <- stops_as_sf(Metrobus2022$stops) %>%
  #spatially filter to only the stops in va
  st_intersection(.,Nova) %>%
  st_drop_geometry() %>%
  left_join(., Metrobus2022$stop_times) %>% select(trip_id, stop_id)

# trips
MetrobusTrips <- Metrobus2022$trips %>% #filter(service_id == Metrobus_service04) %>%
  select(trip_id, route_id, service_id) %>%
  left_join(., MetrobusService %>% select(service_id, multiplier))

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

#Metrorail route_n_stop join done outside of R
Metrorailjoin_2022 <- read.csv("AgencyProfileData/Metrorail.csv", sep = ",")%>%
  pivot_longer(cols = 2:4, names_to = "route_id", values_to = "tripfreq") %>%
  rename(station = ï..stop_id) %>% relocate(station, .after = route_id) %>%
  mutate(agency = "WMATA")

#stop locations, using entrance ids,
Metrorailstops <- read.csv(file.path(GTFS_path, "2022-06_WMATA-Railstops.csv")) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  filter(grepl("ENT_", stop_id)) %>%
  select(ï..VirginiaStation, stop_id) %>% rename(station = ï..VirginiaStation) %>%
  inner_join(., Metrorailjoin_2022) %>% mutate(Mode = "HR", Agency = "WMATA") %>%
  filter(tripfreq != 0) %>%
  unite(station, stop_id, agency, sep = "_", col = "newstop_id") %>%
  unite(route_id, Agency, sep = "_", col = "newroute_id") %>% mutate(AgencyName = "WMATA")


#join all agencies (except rail, this is done after)
Alljoin_2022 <- rbind(ARTjoin_2022,
                      CUEjoin_2022,
                      DASHjoin_2022,
                      FFXjoin_2022,
                      VREjoin_2022,
                      LCTjoin_2022,
                      MetrobusJoin) %>%
  unite(stop_id, agency, col = "newstop_id")

#join with stop data to create spatial dataset (NovaStops data is spatial, Alljoin is not)
#create combined route and stop ids
NovaStopsRoutes_2022 <- merge(Alljoin_2022, NovaStops_2022wmata) %>%
  st_as_sf() %>%
  mutate(Agency = AgencyName) %>%
  unite(route_id, Agency, sep = "_", col = "newroute_id")%>%
  mutate(Mode = ifelse(AgencyName == 'VRE', "CR", "Bus")) %>%
  #merge with rail  stops
  rbind(., Metrorailstops)

st_write(NovaStopsRoutes_2022, "AgencyProfileData/NovaStopsRoutes_2022.shp", append= FALSE)

# Access to Transit: by Route ---------------------------------------------

#function to find access to transit for each route
#TO DO, ALSO FIGURE OUT HOW TO INCORPORATE JOB DATA, NEED IT BY CENSUS TRACT DONT FORGET
#make function so that it does for each route, make loop to go through each route from list of routes
#use newroute_id from NovaStopsRoutes?
#1 mile for commuter rail
access2transitbus <- function(route) {
  #pull just the stops within a 1/4 mile radius of county
  BuffJoin <- NovaStopsRoutes_2022 %>% filter(newroute_id == route) %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid()
  #join employment data with demographic data # THIS IS ON PAUSE, WILL COME BACK TO THIS
  #CensusJobs <- st_join(Census, JobsbyBG)
  #interpolate census data to these stops
  TransitCensus <- st_interpolate_aw(
    CensusDataBG,
    BuffJoin,
    extensive = T
  ) %>% st_drop_geometry() %>% as.data.frame()
  return(TransitCensus)
}

access2transitvre <- function(route) {
  #pull just the stops within a 1/4 mile radius of county
  BuffJoin <- NovaStopsRoutes_2022 %>% filter(newroute_id == route) %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid()
  #join employment data with demographic data # THIS IS ON PAUSE, WILL COME BACK TO THIS
  #CensusJobs <- st_join(Census, JobsbyBG)
  #interpolate census data to these stops
  TransitCensus <- st_interpolate_aw(
    CensusDataBG,
    BuffJoin,
    extensive = T
  ) %>% st_drop_geometry() %>% as.data.frame()
  return(TransitCensus)
}


#list of all routes for ART, DASH, CUE, FFX, LCT, VRE
routes <- NovaStopsRoutes_2022 %>% distinct(newroute_id, AgencyName, Mode, rte_id) %>% as.data.frame()
st_write(routes, "AgencyProfileData/routes.csv")

busroutes <- routes %>% filter(Mode == "Bus")
railroutes <- routes %>% filter(Mode == "CR")

#loop through all nova routes to find access to transit at route level
#do bus and rail separately (since they have different buffer radii), then combine
NovaAccessTransit_Bus <- lapply(busroutes$newroute_id, access2transitbus) %>%
  do.call(rbind.data.frame, .) %>% cbind(busroutes, .)

NovaAccessTransit_VRE <- lapply(railroutes$newroute_id, access2transitvre) %>%
  do.call(rbind.data.frame, .) %>% cbind(railroutes, .)

#combine
NovaAccessTransit <- rbind(NovaAccessTransit_VRE, NovaAccessTransit_Bus)

#save as a csv
st_write(NovaAccessTransit, "AgencyProfileData/NovaAccessTransit.csv", append = FALSE)


# Amount of Service -------------------------------------------------------


# Using Census Tracts
amount_service <- function(newroute_id){
  #create county shape and transform
  CountyShp <- CensusDataTracts %>% select(GEOID) %>% st_transform(crs = 4326)

  #summarize stop frequency at the route level
  Ag_County <- NovaStopsRoutes_2022 %>% filter(newrt_d == newroute_id) %>%
    group_by(newrt_d, Mode) %>% summarize(stopfreq = mean(tripfrq))

  #buffer the stops, conditional if its a commuter rail (1 mile radius) or bus (1/4 mi)
  Ag_CountyBuff <- Ag_County %>% st_buffer(., ifelse(.$Mode == "CR", 1600,400)) %>%
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
  AmountService <- st_join(CountyInterpol, CountyShp, join = st_equals) %>%
    mutate(newroute_id = newroute_id)

  return(AmountService)
}

routes <- NovaStopsRoutes_2022 %>% distinct(newrt_d, AgncyNm, Mode) %>% as.data.frame()
#run loop through all routes
nova_amount <- lapply(routes$newrt_d, amount_service) %>%
  do.call(rbind.data.frame, .)


st_write(nova_amount, "AgencyProfileData/NovaAmountTransit.xlsx")
st_write(nova_amount, "AgencyProfileData/NovaAmountTransit.shp")

