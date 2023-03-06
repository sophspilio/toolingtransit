#load libraries
library(tidyverse)
library(censusapi)
library(tidycensus)
library(sf)
library(units)
library(tidytransit)
library(toolingtransit)
library(mapview)


# Set Up ------------------------------------------------------------------
GTFS_path <- file.path ("Z:",
                        "NVTC General", "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data", "GTFS")

NovaStopsRoutes <- st_read("AgencyProfileData/NovaStopsRoutes.shp")



#county census data
countycensus_tract <- function(County) {
  #load required libraries
  require(tidyverse)
  require(censusapi)
  require(tidycensus)
  require(sf)
  require(units)
  #pull census acs data for 2020, select variables for county
  CensusCnty <- get_acs(
    geography = "tract",
    year = 2021,
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
Arl <- countycensus_tract("Arlington") %>% st_transform(crs = 4326) %>% select(-GEOID)
Alx <- countycensus_tract("Alexandria city") %>% st_transform(crs = 4326) %>% select(-GEOID)
CityFFX <- countycensus_tract("Fairfax city") %>% st_transform(crs = 4326) %>% select(-GEOID)
Lou <- countycensus_tract("Loudoun") %>% st_transform(crs = 4326) %>% select(-GEOID)
fc <- countycensus_tract("Falls Church city") %>% st_transform(crs = 4326) %>% select(-GEOID)
ffx <- countycensus_tract("Fairfax County") %>% st_transform(crs = 4326) %>% select(-GEOID)

#finding service ids for weekdays
service <- function(GTFS, week){
  GTFS <- read_gtfs(file.path(GTFS_path, GTFS))
  wkdays <- GTFS$calendar %>%
    filter(tuesday == 1 & wednesday == 1) %>% .$service_id
  wkends <- GTFS$calendar %>% filter(saturday == 1 | sunday == 1) %>% .$service_id

  week_id <- cbind(wkdays, wkends) %>% as_tibble()
  if (week == "wkdays") {
    return(week_id$wkdays)
  } else {
    return(week_id$wkends)
  }
}


 freq <- function(GTFS, agency, start, end, weekday){
  GTFSfreq <- read_gtfs(file.path(GTFS_path, GTFS)) %>%
    get_route_frequency(., start_time = start, end_time = end, service_ids = service(GTFS, weekday)) %>%
    mutate(Agency = agency) %>% unite(newrt_id, c(route_id, Agency), sep = "_") %>%
    select(newrt_id, median_headways, mean_headways)
  return(GTFSfreq)
}

# 1. High Frequency Routes (7-8am) -------------------------------------------


#headways for all Nova routes peak period
NovaFreq <- rbind(freq("2022-11_Arlington.zip", "ART", "07:00:00", "08:00:00", "wkdays"),
                  #freq("2022-11_CUE.zip", "CUE", "07:00:00", "08:00:00", 1),
                  freq("2022-11_DASH.zip", "DASH", "07:00:00", "08:00:00", "wkdays"),
                  freq("2022-11_Fairfax_Connector.zip", "FFX", "07:00:00", "08:00:00", "wkdays"),
                  freq("2022-11_Loudoun.zip", "LCT", "07:00:00", "08:00:00", "wkdays"),
                  freq("2022-11_OmniRide_PRTC.zip", "PRTC", "07:00:00", "08:00:00", "wkdays"),
                  freq("2022-12_Metrobus.zip", "WMATA", "07:00:00", "08:00:00", "wkdays"))

#change units from seconds to minutes
NovaFreq$median_headways <- NovaFreq$median_headways %>%
  set_units("secs") %>% set_units("min") %>% drop_units()
NovaFreq$mean_headways <- NovaFreq$mean_headways %>%
  set_units("secs") %>% set_units("min") %>% drop_units()

#weekday high frequency routes (less than or equal to 15 min headways)
HighFreq <- NovaFreq %>% filter(median_headways <= 15) %>%
  #two loops, not actually 15 min
  filter(newrt_id != "3944_PRTC")

# 2. Late Night Weekday Service (9-10pm) ----------------------------------

LateNight <- rbind(freq("2022-11_Arlington.zip", "ART", "21:00:00", "22:00:00", "wkdays"),
                  #freq("2022-11_CUE.zip", "CUE", "21:00:00", "22:00:00", 1),
                  freq("2022-11_DASH.zip", "DASH", "21:00:00", "22:00:00", "wkdays"),
                  freq("2022-11_Fairfax_Connector.zip", "FFX", "21:00:00", "22:00:00", "wkdays"),
                  freq("2022-11_Loudoun.zip", "LCT", "21:00:00", "22:00:00", "wkdays"),
                  freq("2022-11_OmniRide_PRTC.zip", "PRTC", "21:00:00", "22:00:00", "wkdays"),
                  freq("2022-12_Metrobus.zip", "WMATA", "21:00:00", "22:00:00", "wkdays"))

# 3. Weekend and Weekday Service ------------------------------------------------------
Weekend <- rbind(freq("2022-11_Arlington.zip", "ART", "06:00:00", "23:00:00", "wkends"),
                 #freq("2022-11_CUE.zip", "CUE"),
                 freq("2022-11_DASH.zip", "DASH", "06:00:00", "23:00:00", "wkends"),
                 freq("2022-11_Fairfax_Connector.zip", "FFX", "06:00:00", "23:00:00", "wkends"),
                 freq("2022-11_Loudoun.zip", "LCT", "06:00:00", "23:00:00", "wkends"),
                 freq("2022-11_OmniRide_PRTC.zip","PRTC", "06:00:00", "23:00:00", "wkends"),
                 freq("2022-12_Metrobus.zip", "WMATA", "06:00:00", "23:00:00", "wkends")
                 )

Weekday <- rbind(freq("2022-11_Arlington.zip", "ART", "06:00:00", "23:00:00", "wkdays"),
                 #freq("2022-11_CUE.zip", "CUE"),
                 freq("2022-11_DASH.zip", "DASH", "06:00:00", "23:00:00", "wkdays"),
                 freq("2022-11_Fairfax_Connector.zip", "FFX", "06:00:00", "23:00:00", "wkdays"),
                 freq("2022-11_Loudoun.zip", "LCT", "06:00:00", "23:00:00", "wkdays"),
                 freq("2022-11_OmniRide_PRTC.zip", "PRTC", "06:00:00", "23:00:00", "wkdays"),
                 freq("2022-12_Metrobus.zip", "WMATA", "06:00:00", "23:00:00", "wkdays")
)

# Access to Transit  ------------------------------------------------------

#create function for access to transit within jurisdictions
#with options for different types of access (highfreq, latenight, weekend)
Access <- function(county, mode, type = "All", buff){
  if(mode == "Bus"){
    if(type == "High Frequency"){
      access <- st_interpolate_aw(
        county,
        NovaStopsRoutes %>% filter(Mode == mode, newrt_d %in% HighFreq$newrt_id) %>%
          st_buffer(dist = buff) %>% st_union() %>% st_make_valid(),
        extensive = TRUE) %>% mutate(Mode = mode, Type = type)
      return(access)
    }else if (type == "Late Night"){
      access <- st_interpolate_aw(
        county,
        NovaStopsRoutes %>% filter(Mode == mode, newrt_d %in% LateNight$newrt_id) %>%
          st_buffer(dist = buff) %>% st_union() %>% st_make_valid(),
        extensive = TRUE) %>% mutate(Mode = mode, Type = type)
      return(access)
    } else if (type == "Weekend"){
      access <- st_interpolate_aw(
        county,
        NovaStopsRoutes %>% filter(Mode == mode, newrt_d %in% Weekend$newrt_id) %>%
          st_buffer(dist = buff) %>% st_union() %>% st_make_valid(),
        extensive = TRUE) %>% mutate(Mode = mode, Type = type)
      return(access)
    } else if (type == "Weekday"){
      access <- st_interpolate_aw(
        county,
        NovaStopsRoutes %>% filter(Mode == mode, newrt_d %in% Weekday$newrt_id) %>%
          st_buffer(dist = buff) %>% st_union() %>% st_make_valid(),
        extensive = TRUE) %>% mutate(Mode = mode, Type = type)
      return(access)
    } else {
      access <- st_interpolate_aw(
        county,
        NovaStopsRoutes %>% filter(Mode == mode) %>%
          st_buffer(dist = buff) %>% st_union() %>% st_make_valid(),
        extensive = TRUE) %>% mutate(Mode = mode, Type = type)
      return(access)
    }
  }else if(mode == "Paratransit") {
      access <- st_interpolate_aw(
        county,
        NovaStopsRoutes %>%
          st_buffer(dist = buff) %>% st_union() %>% st_make_valid(),
        extensive = TRUE) %>% mutate(Mode = mode, Type  = type)
      return(access)
  } else {
    access <- st_interpolate_aw(
      county,
      NovaStopsRoutes %>% filter(Mode == mode) %>%
        st_buffer(dist = buff) %>% st_union() %>% st_make_valid(),
      extensive = TRUE) %>% mutate(Mode = mode, Type = type)
    return(access)
  }
}

#Arlington
ArlAccess <- rbind(Access(Arl, "Bus", buff = 400),
                   Access(Arl, "Bus", type = "High Frequency", 400),#access to high frequency bus
                   Access(Arl, "Bus", type = "Late Night", 400),#access to late night weekday bus
                   Access(Arl, "Bus", type = "Weekend", 400), #access to weekend bus service
                   Access(Arl, "Bus", type = "Weekday", 400), #access to weekday bus service
                   Access(Arl, "CR", buff = 1600),
                   Access(Arl, "HR", buff = 800),
                   Access(Arl, "Paratransit", buff = 1200)) %>%
  mutate(Jurisdiction = "Arlington County")

#Alexandria
AlxAccess <- rbind(Access(Alx, "Bus", buff = 400),
                   Access(Alx, "Bus", type = "High Frequency", 400),#access to high frequency bus
                   Access(Alx, "Bus", type = "Late Night", 400),#access to late night weekday bus
                   Access(Alx, "Bus", type = "Weekend", 400), #access to weekend bus service
                   Access(Alx, "Bus", type = "Weekday", 400), #access to weekday bus service
                   Access(Alx, "CR", buff = 1600),
                   Access(Alx, "HR", buff = 800),
                   Access(Alx, "Paratransit", buff = 1200)) %>%
  mutate(Jurisdiction = "City of Alexandria")

#City of Falls Church
fcAccess <- rbind(Access(fc, "Bus", buff = 400),
                   Access(fc, "Bus", type = "High Frequency", 400),#access to high frequency bus
                   Access(fc, "Bus", type = "Late Night", 400),#access to late night weekday bus
                   Access(fc, "Bus", type = "Weekend", 400), #access to weekend bus service
                   Access(fc, "Bus", type = "Weekday", 400), #access to weekday bus service
                   #Access(fc, "CR", buff = 1600),
                   Access(fc, "HR", buff = 800),
                   Access(fc, "Paratransit", buff = 1200)) %>%
  mutate(Jurisdiction = "City of Falls Church")

#City of Fairfax
CityFFXAccess <- rbind(Access(CityFFX, "Bus", buff = 400),
                  #Access(CityFFX, "Bus", type = "High Frequency", 400),#access to high frequency bus
                  #Access(CityFFX, "Bus", type = "Late Night", 400),#access to late night weekday bus
                  #Access(CityFFX, "Bus", type = "Weekend", 400), #access to weekend bus service
                  #Access(CityFFX, "Bus", type = "Weekday", 400), #access to weekday bus service
                  #Access(CityFFX, "CR", buff = 1600),
                  #Access(CityFFX, "HR", buff = 800),
                  Access(CityFFX, "Paratransit", buff = 1200)) %>%
  mutate(Jurisdiction = "City of Fairfax")

#Fairfax County
ffxAccess <- rbind(Access(ffx, "Bus", buff = 400),
                       Access(ffx, "Bus", type = "High Frequency", 400),#access to high frequency bus
                       Access(ffx, "Bus", type = "Late Night", 400),#access to late night weekday bus
                       Access(ffx, "Bus", type = "Weekend", 400), #access to weekend bus service
                       Access(ffx, "Bus", type = "Weekday", 400), #access to weekday bus service
                       Access(ffx, "CR", buff = 1600),
                       Access(ffx, "HR", buff = 800),
                       Access(ffx, "Paratransit", buff = 1200)) %>%
  mutate(Jurisdiction = "Fairfax County")

#Loudoun County
LouAccess <- rbind(Access(Lou, "Bus", buff = 400),
                   Access(Lou, "Bus", type = "High Frequency", 400),#access to high frequency bus
                   Access(Lou, "Bus", type = "Late Night", 400),#access to late night weekday bus
                   Access(Lou, "Bus", type = "Weekend", 400), #access to weekend bus service
                   Access(Lou, "Bus", type = "Weekday", 400), #access to weekday bus service
                   #Access(Lou, "CR", buff = 1600),
                   Access(Lou, "HR", buff = 800),
                   Access(Lou, "Paratransit", buff = 1200)) %>%
  mutate(Jurisdiction = "Loudoun County")

#combine all access
NovaAcessJurisdictions <- rbind(LouAccess,
                                ArlAccess,
                                AlxAccess,
                                ffxAccess,
                                CityFFXAccess,
                                fcAccess)
st_write(NovaAcessJurisdictions, "AgencyProfileData/NovaAccessJurisdictions.xlsx",
         delete_dsn = T)


#Bus Access shapefiles
#bus access
#all bus access
a <- NovaStopsRoutes %>% filter(Mode == "Bus") %>%
  st_buffer(dist = 400) %>% st_union() %>% st_make_valid()
#high freq bus
b <- NovaStopsRoutes %>% filter(Mode == "Bus", newrt_d %in% HighFreq$newrt_id) %>%
  st_buffer(dist = 400) %>% st_union()

c <- NovaStopsRoutes %>% filter(Mode == "Bus", newrt_d %in% LateNight$newrt_id) %>%
  st_buffer(dist = 400) %>% st_union()

d <- NovaStopsRoutes %>% filter(Mode == "Bus", newrt_d %in% Weekend$newrt_id) %>%
  st_buffer(dist = 400) %>% st_union()

st_write(b, "data/HighFreqBusAccess.shp", delete_dsn = TRUE)
st_write(c, "data/LateNightBusAccess.shp")
st_write(d, "data/WeekendBusAccess.shp")
