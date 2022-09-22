#Transit Agency Profiles

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

#reload data
CensusDataBG <- st_read("data/CensusData.shp")
CensusDataTracts <- st_read("AgencyProfileData/CensusDataTracts.shp") %>% st_transform(crs = 4326)
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

#a bit more code for Census by block group or census tract
# this code  keeps the NAME column for better identification as dimension table
# a la Power BI
#Census Tracts

CensusTracts <- get_acs(
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
  county = c("Arlington",
             "Fairfax County",
             "Fairfax city",
             "Loudoun",
             "Alexandria City",
             "Falls Church City"),
  geometry = TRUE,
  output = "wide") %>%mutate(Count0Car = HHOwn0VehE + HHRent0VehE,
                             NonWhitePop = TotalPopulationE - WhitePopE,
                             Under200PctPoverty = PovertyRatioTotalE - PovertyOver200PctE,
                             area = st_area(.)) %>%
  #select only necessary variables (remove margin of error columns)
  select(NAME, GEOID, TotalPopulationE,
         HHCountE, Count0Car, TotalCommutersE,
         PublicTransportE, NonWhitePop, Under200PctPoverty, area)

#set units for the area as square miles
CensusTracts$area <- set_units(CensusTracts$area, mi^2)


#Census Block Groups
CensusBGs <- get_acs(
  geography = "block group",
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
  county = c("Arlington",
             "Fairfax County",
             "Fairfax city",
             "Loudoun",
             "Alexandria City",
             "Falls Church City"),
  geometry = TRUE,
  output = "wide") %>%mutate(Count0Car = HHOwn0VehE + HHRent0VehE,
                             NonWhitePop = TotalPopulationE - WhitePopE,
                             Under200PctPoverty = PovertyRatioTotalE - PovertyOver200PctE,
                             area = st_area(.)) %>%
  #select only necessary variables (remove margin of error columns)
  select(NAME, GEOID, TotalPopulationE,
         HHCountE, Count0Car, TotalCommutersE,
         PublicTransportE, NonWhitePop, Under200PctPoverty, area)

#set units for the area as square miles
CensusBGs$area <- set_units(CensusBGs$area, mi^2)




# Routes Dim Table --------------------------------------------------------



#route with route name
route_list <- function(gtfszip, agency){
  GTFS <- read_gtfs(file.path(GTFS_path, gtfszip))
  routes <- GTFS$routes %>% select(route_id, route_short_name) %>% mutate(Agency = agency)
  return(routes)
}


#add Metrobus separately
## need to separate out routes that go into va, will use stop file bc its spatial for this
Metrobus2022 <- read_gtfs(file.path(GTFS_path, "2022-04_WMATA.zip"))

# stops: spatial  filter to nova
MetrobusStops <- stops_as_sf(Metrobus2022$stops) %>%
  #spatially filter to only the stops in va
  st_intersection(.,Nova) %>%
  st_drop_geometry() %>%
  left_join(., Metrobus2022$stop_times) %>% select(trip_id, stop_id)

# trips
MetrobusTrips <- Metrobus2022$trips %>% #filter(service_id == Metrobus_service04) %>%
  select(trip_id, route_id, service_id)

#join stops and trips
MetrobusJoin <- inner_join(MetrobusStops, MetrobusTrips) %>%
  #join with route file to get route short name
  left_join(., Metrobus2022$routes) %>% select(route_id, route_short_name) %>% distinct()



#get short names for rail
Metrorail2022 <- read_gtfs(file.path(GTFS_path, "2022-06_Metrorail.zip"))
Metrorailroutes <- Metrorail2022$routes %>% filter(route_id == "BLUE" |
                                                     route_id == "ORANGE" |
                                                     route_id == "SILVER" |
                                                     route_id == "YELLOW") %>%
  select(route_id, route_short_name)


#make master route list
routes <- rbind(route_list("2022-04_Arlington.zip", "ART") %>% mutate(Mode = "Bus"),
                route_list("2022-03_CUE.zip", "CUE") %>% mutate(Mode = "Bus"),
                route_list("2022-04_DASH.zip", "DASH") %>% mutate(Mode = "Bus"),
                route_list("2022-03_Fairfax_Connector.zip", "FFX") %>% mutate(Mode = "Bus"),
                route_list("2022-07_Loudoun.zip", "LCT") %>% mutate(Mode = "Bus"),
                route_list("2022-03_VRE.zip", "VRE") %>% mutate(Mode = "CR"),
                route_list("2022-03_OmniRide_PRTC (2).zip", "PRTC") %>% mutate(Mode = "Bus"),
                MetrobusJoin %>% mutate(Agency = "WMATA") %>% mutate(Mode = "Bus"),
                Metrorailroutes %>% mutate(Agency = "WMATA") %>% mutate(Mode = "HR")) %>%
  unite(route_id, Agency, col = NewRouteID, sep = "_", remove = F)





# NovaStops 2022 ---------------------------------------------------------------
#  WITH LOUDOUN AND VRE, NO WMATA

#Join 2022 stops
NovaStops_2022 <- rbind(
  stoploc("2022-04_Arlington.zip", "ART"),
  stoploc("2022-03_CUE.zip", "CUE"),
  stoploc("2022-04_DASH.zip", "DASH"),
  stoploc("2022-03_Fairfax_Connector.zip", "FFX"),
  stoploc("2022-07_Loudoun.zip", "LCT"),
  stoploc("2022-03_VRE.zip", "VRE"),
  stoploc("2022-03_OmniRide_PRTC (2).zip", "PRTC")
)

#create columns for Agency name and for a newstop id that includes agency
NovaStops_2022 <- NovaStops_2022 %>%
  unite(stop_id, Agency, col = "newstop_id", remove = F)
Metrobus2022 <- read_gtfs(file.path(GTFS_path, "2022-04_WMATA.zip"))
MetrobusStops <- stops_as_sf(Metrobus2022$stops) %>%
  st_intersection(., Nova) %>%
  select(stop_id) %>%
  mutate(Agency = "WMATA") %>% unite(stop_id, Agency, col = "newstop_id", remove = F)

#bind with the other nova agency stop data from above
NovaStops_2022wmata <- rbind(NovaStops_2022, MetrobusStops)


# NovaStopsRoutes 2022 ----------------------------------------------------
# WITH LOUDOUN and VRE

#function to find number of trips for each stop and join with routes
route_n_stop <- function(gtfszip, agency){
  GTFS_path <- file.path ("Z:",
                          "NVTC General",
                          "Projects and Programs",
                          "Transit Resource Center (TRC)",
                          "Data",
                          "GTFS")
  GTFS <- read_gtfs(file.path(GTFS_path, gtfszip))
  #need service and number of days that service_ids use (multiplier)

  #an extra step for PRTC bc the days of the week aren't numeric
  GTFS$calendar[2:8] <- lapply(GTFS$calendar[2:8], FUN = function(y){as.integer(y)})
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
    mutate(Agency = agency) %>%
    select(route_id, stop_id, tripfreq, Agency)
  return(Join)
}

GTFS_path <- file.path ("Z:",
                        "NVTC General",
                        "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data",
                        "GTFS")


#rail will be brougt in separately at the end


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
  mutate(Agency = "WMATA") %>%
  select(route_id, stop_id, tripfreq, Agency)

#Metrorail route_n_stop join done outside of R
Metrorailjoin_2022 <- read.csv("AgencyProfileData/Metrorail.csv", sep = ",")%>%
  pivot_longer(cols = 2:5, names_to = "route_id", values_to = "tripfreq") %>%
  rename(station = ï..stop_id) %>% relocate(station, .after = route_id) %>%
  mutate(agency = "WMATA")

#stop locations, using entrance ids,
Metrorailstops <- read.csv(file.path(GTFS_path, "2022-06_WMATA-Railstops.csv")) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  filter(grepl("ENT_", stop_id)) %>%
  select(ï..VirginiaStation, stop_id) %>% rename(station = ï..VirginiaStation) %>%
  inner_join(., Metrorailjoin_2022) %>% mutate(Mode = "HR", Agency = "WMATA") %>%
  filter(tripfreq != 0) %>%
  unite(station, stop_id, agency, sep = "_", col = "newstop_id", remove = F) %>%
  select(-station, -agency) %>%
  unite(route_id, Agency, sep = "_", col = "newroute_id", remove = F)


#join all agencies (except rail, this is done after)
Alljoin_2022 <- rbind(route_n_stop("2022-04_Arlington.zip", "ART"),
                      route_n_stop("2022-03_CUE.zip", "CUE"),
                      route_n_stop("2022-04_DASH.zip", "DASH"),
                      route_n_stop("2022-03_Fairfax_Connector.zip", "FFX"),
                      route_n_stop("2022-03_Loudoun.zip", "LCT"),
                      route_n_stop("2022-03_VRE.zip", "VRE"),
                      route_n_stop("2022-03_OmniRide_PRTC (2).zip", "PRTC"),
                      MetrobusJoin) %>%
  unite(stop_id, Agency, col = "newstop_id", remove = F)

#join with stop data to create spatial dataset (NovaStops data is spatial, Alljoin is not)
#create combined route and stop ids
NovaStopsRoutes_2022 <- merge(Alljoin_2022, NovaStops_2022wmata) %>%
  st_as_sf() %>%
  unite(route_id, Agency, sep = "_", col = "newroute_id", remove = F)%>%
  mutate(Mode = ifelse(Agency == 'VRE', "CR", "Bus")) %>%
  #merge with rail  stops
  rbind(., Metrorailstops)

st_write(NovaStopsRoutes_2022, "AgencyProfileData/NovaStopsRoutes_2022.shp", append= FALSE)

# Access to Transit: by Route ---------------------------------------------

#function to find access to transit for each route

#make function so that it does for each route, make loop to go through each route from list of routes



#remove all wmata
NovaStopsRoutes_NOWMATA <- NovaStopsRoutes_2022 %>% filter(Agency != "WMATA")

#CensusTracts need to include PRTC jursidictions
NOVA_Plus <- c("Arlington",
               "Fairfax County",
               "Fairfax city",
               "Loudoun",
               "Alexandria City",
               "Falls Church City",
               "Prince William County",
               "Stafford County",
               "Spotsylvania County",
               "Fredericksburg City",
               "Manassas City",
               "Manassas Park City")
CensusDataTracts <- countycensus_tract(NOVA_Plus) %>%
  st_transform(crs = 4326) %>%
  select(-GEOID)

Arl <- countycensus_tract("Arlington") %>% st_transform(crs = 4326) %>% select(-GEOID)
Alx <- countycensus_tract("Alexandria city") %>% st_transform(crs = 4326) %>% select(-GEOID)
CityFFX <- countycensus_tract("Fairfax city") %>% st_transform(crs = 4326) %>% select(-GEOID)
Lou <- countycensus_tract("Loudoun") %>% st_transform(crs = 4326) %>% select(-GEOID)
fc <- countycensus_tract("Falls Church city") %>% st_transform(crs = 4326) %>% select(-GEOID)
ffx <- countycensus_tract("Fairfax County") %>% st_transform(crs = 4326) %>% select(-GEOID)

access2transit <- function(Mode, buff, Census) {

  BuffJoin <- NovaStopsRoutes_2022 %>% filter(Mode == Mode) %>%
    st_buffer(dist = buff) %>% st_union() %>% st_make_valid()

  #interpolate census data to these stops
  TransitCensus <- st_interpolate_aw(
    Census,
    BuffJoin,
    extensive = T
  ) %>% st_drop_geometry() %>% as.data.frame()
  return(TransitCensus)
}

#bus = 1/4 mile (400m)
#hr = 1/2 mile (800m)
#para = 3/4 mile (1200m)
#cr = 1 mile (1600m)

#bus
ARLaccess <- access2transit("Bus", 400, Arl)
ALXaccess <- access2transit("Bus", 400, Alx)
CityFFXaccess <- access2transit("Bus", 400, CityFFX)
LOUaccess <- access2transit("Bus", 400, Lou)
st_write(LOUaccess, "AgencyProfileData/Louaccess.xlsx")
fcaccess <- access2transit("Bus", 400, fc)
st_write(fcaccess, "AgencyProfileData/fcaccess.xlsx")
ffxaccess <- access2transit("Bus", 400, ffx)
st_write(ffxaccess, "AgencyProfileData/ffxaccess.xlsx")

#heavy rail
ARlHR <- access2transit("HR", 800, Arl)
st_write(ARlHR, "AgencyProfileData/ArlAccess.xlsx", append = T)
AlxHR <- access2transit("HR", 800, Alx)
st_write(AlxHR, "AgencyProfileData/AlxAccess.xlsx", append = T)
CityFFXHR <- access2transit("HR", 800, CityFFX)
st_write(CityFFXHR, "AgencyProfileData/CityFFXAccess.xlsx", append = T)
LouHR <- access2transit("HR", 800, Lou)
st_write(LouHR, "AgencyProfileData/Louaccess.xlsx", append = T)
fcHR <- access2transit("HR", 800, fc)
st_write(fcHR, "AgencyProfileData/fcaccess.xlsx", append = T)
ffxHR <- access2transit("HR", 800, ffx)
st_write(ffxHR, "AgencyProfileData/ffxaccess.xlsx", append =T)

#commuter rail
ARlCR <- access2transit("CR", 1600, Arl)
st_write(ARlCR, "AgencyProfileData/ArlAccess.xlsx", append = T)
AlxCR <- access2transit("CR", 1600, Alx)
st_write(AlxCR, "AgencyProfileData/AlxAccess.xlsx", append = T)
CityFFXCR <- access2transit("CR", 1600, CityFFX)
st_write(CityFFXCR, "AgencyProfileData/CityFFXAccess.xlsx", append = T)
LouCR <- access2transit("CR", 1600, Lou)
st_write(LouCR, "AgencyProfileData/Louaccess.xlsx", append = T)
fcCR <- access2transit("CR", 1600, fc)
st_write(fcCR, "AgencyProfileData/fcaccess.xlsx", append = T)
ffxCR <- access2transit("CR", 1600, ffx)
st_write(ffxCR, "AgencyProfileData/ffxaccess.xlsx", append =T)






#paratransit
access2paratransit <- function(Census) {

  BuffJoin <- NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid()

  #interpolate census data to these stops
  TransitCensus <- st_interpolate_aw(
    Census,
    BuffJoin,
    extensive = T
  ) %>% st_drop_geometry() %>% as.data.frame()
  return(TransitCensus)
}




ARlpara <- access2paratransit(Arl)
st_write(ARlpara, "AgencyProfileData/ArlAccess.xlsx", append = T)
Alxpara <- access2paratransit( Alx)
st_write(Alxpara, "AgencyProfileData/AlxAccess.xlsx", append = T)
CityFFXpara <- access2paratransit(CityFFX)
st_write(CityFFXpara, "AgencyProfileData/CityFFXAccess.xlsx", append = T)
Loupara <- access2paratransit(Lou)
st_write(Loupara, "AgencyProfileData/Louaccess.xlsx", append = T)
fcpara <- access2paratransit(fc)
st_write(fcpara, "AgencyProfileData/fcaccess.xlsx", append = T)
ffxpara <- access2paratransit(ffx)
st_write(ffxpara, "AgencyProfileData/ffxaccess.xlsx", append =T)



#unit of analysis is route and agency
#1/4 mile for bus
access2transitbus <- function(route) {
  #pull just the stops within a 1/4 mile radius of county
  BuffJoin <- NovaStopsRoutes_NOWMATA %>% filter(newroute_id == route) %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid()
  #join employment data with demographic data # THIS IS ON PAUSE, WILL COME BACK TO THIS
  #CensusJobs <- st_join(Census, JobsbyBG)
  #interpolate census data to these stops
  TransitCensus <- st_interpolate_aw(
    CensusDataTracts,
    BuffJoin,
    extensive = T
  ) %>% st_drop_geometry() %>% as.data.frame()
  return(TransitCensus)
}
#1 mile for commuter rail
access2transitvre <- function(route) {
  #pull just the stops within a 1/4 mile radius of county
  BuffJoin <- NovaStopsRoutes_2022NOWWMATA %>% filter(newroute_id == route) %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid()
  #join employment data with demographic data # THIS IS ON PAUSE, WILL COME BACK TO THIS
  #CensusJobs <- st_join(Census, JobsbyBG)
  #interpolate census data to these stops
  TransitCensus <- st_interpolate_aw(
    CensusDataTracts,
    BuffJoin,
    extensive = T
  ) %>% st_drop_geometry() %>% as.data.frame()
  return(TransitCensus)
}

#list of all routes for ART, DASH, CUE, FFX, LCT, VRE, PRTC
routes <- NovaStopsRoutes_NOWMATA %>% distinct(newroute_id, Agency, Mode) %>% as.data.frame()
st_write(routes, "AgencyProfileData/routes.csv")

busroutes <- routes %>% filter(Mode == "Bus")
crroutes <- routes %>% filter(Mode == "CR")



#loop through all nova routes to find access to transit at route level
#do bus and rail separately (since they have different buffer radii), then combine
NovaAccessTransit_Bus <- lapply(busroutes$newroute_id, access2transitbus) %>%
  do.call(rbind.data.frame, .) %>% cbind(busroutes, .)

NovaAccessTransit_VRE <- lapply(crroutes$newroute_id, access2transitvre) %>%
  do.call(rbind.data.frame, .) %>% cbind(crroutes, .)


#combine
NovaAccessTransit <- rbind(NovaAccessTransit_VRE, NovaAccessTransit_Bus)

#save as a xlsx
st_write(NovaAccessTransit, "AgencyProfileData/NovaAccessTransit.xlsx", append = FALSE)
#run again for each agency

access2transitagency <- function(agency, buffer) {
  #pull just the stops within a 1/4 mile radius of county
  BuffJoin <- NovaStopsRoutes_NOWMATA %>% filter(Agency == agency) %>%
    st_buffer(dist = buffer) %>% st_union() %>% st_make_valid()
  #join employment data with demographic data # THIS IS ON PAUSE, WILL COME BACK TO THIS
  #CensusJobs <- st_join(Census, JobsbyBG)
  #interpolate census data to these stops
  TransitCensus <- st_interpolate_aw(
    CensusDataTracts,
    BuffJoin,
    extensive = T
  ) %>% st_drop_geometry() %>% as.data.frame()
  return(TransitCensus)
}

NovaAccessTransitbyAgency <- rbind(access2transitagency("ART", 400) %>% mutate(Agency = "ART"),
                                   access2transitagency("CUE", 400) %>% mutate(Agency = "CUE"),
                                   access2transitagency("DASH", 400) %>% mutate(Agency = "DASH"),
                                   access2transitagency("FFX", 400) %>% mutate(Agency = "FFX"),
                                   access2transitagency("LCT", 400) %>% mutate(Agency = "LCT"),
                                   access2transitagency("VRE", 1600) %>% mutate(Agency = "VRE"),
                                   access2transitagency("PRTC", 400) %>% mutate(Agency = "PRTC"))

st_write(NovaAccessTransitbyAgency, "AgencyProfileData/NovaAccessTransitbyAgency.xlsx")


# Amount of Service -------------------------------------------------------

# Using Census Tracts
NOVA_Plus <- c("Arlington",
               "Fairfax County",
               "Fairfax city",
               "Loudoun",
               "Alexandria City",
               "Falls Church City",
               "Prince William County",
               "Stafford County",
               "Spotsylvania County",
               "Fredericksburg City",
               "Manassas City",
               "Manassas Park City")

CensusDataTracts <- countycensus_tract(NOVA_Plus) %>%
  st_transform(crs = 4326)



amount_service <- function(newroute_id){
  message(newroute_id)
  #create county shape and transform
  CountyShp <- CensusDataTracts %>% select(GEOID)

  #summarize stop frequency at the route level
  Ag_County <- NovaStopsRoutes_2022 %>% filter(newrt_d == newroute_id) %>%
    group_by(newrt_d, Mode) %>% summarize(stopfreq = mean(tripfrq))

  #buffer the stops, conditional if its a commuter rail (1 mile radius) or bus (1/4 mi)
  Ag_CountyBuff <- Ag_County %>% st_buffer(., ifelse(.$Mode == "CR", 1600,
                                                     ifelse(.$Mode == "HR", 800,400))) %>%
    mutate(area = st_area(geometry))
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


NovaStopsRoutes_2022 <- st_read("AgencyProfileData/NovaStopsRoutes_2022.shp")
routes <- NovaStopsRoutes_2022 %>% distinct(newrt_d, Agency, Mode) %>% as.data.frame()


#separate out for each county
Arl <- countycensus_tract("Arlington") %>% st_transform(crs = 4326)
Alx <- countycensus_tract("Alexandria city") %>% st_transform(crs = 4326)
FallsChurch <- countycensus_tract("Falls Church city") %>% st_transform(crs = 4326)
FfxCity <- countycensus_tract("Fairfax city") %>% st_transform(crs = 4326)
Loudoun <- countycensus_tract("Loudoun") %>% st_transform(crs = 4326)
ffx <- countycensus_tract("Fairfax County") %>% st_transform(crs = 4326)

nova_amount <- lapply(routes$newrt_d, amount_service) %>%
  do.call(rbind.data.frame, .)

arl_amount <- lapply(routes$newrt_d, amount_service) %>%
  do.call(rbind.data.frame, .)
alx_amount <- lapply(routes$newrt_d, amount_service) %>%
  do.call(rbind.data.frame, .)
fc_amount <- lapply(routes$newrt_d, amount_service) %>%
  do.call(rbind.data.frame, .)
ffxcity_amount <- lapply(routes$newrt_d, amount_service) %>%
  do.call(rbind.data.frame, .)
Lou_amount <- lapply(routes$newrt_d, amount_service) %>%
  do.call(rbind.data.frame, .)
ffx_amount <- lapply(routes$newrt_d, amount_service) %>%
  do.call(rbind.data.frame, .)

 #run loop through all routes
nova_amount <- rbind(arl_amount,
                     alx_amount,
                     fc_amount,
                     ffxcity_amount,
                     Lou_amount,
                     ffx_amount)


st_write(nova_amount, "AgencyProfileData/NovaAmountTransit.xlsx")
st_write(nova_amount, "AgencyProfileData/NovaAmountTransit.shp", append = F)


