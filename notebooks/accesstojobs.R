## Access to Jobs within Transit radius
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

NovaStopsRoutes <- st_read("AgencyProfileData/NovaStopsRoutes.shp")
#bus = 1/4 mile (400m)
#hr = 1/2 mile (800m)
#para = 3/4 mile (1200m)
#cr = 1 mile (1600m)

countycensus_tract <- function(County) {
  #load required libraries
  require(tidyverse)
  require(censusapi)
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
    select(NAME, TotalPopulationE, HHCountE, Count0Car, TotalCommutersE, PublicTransportE, NonWhitePop, Under200PctPoverty, area, GEOID)
  #set units for the area as square miles
  CensusCnty$area <- set_units(CensusCnty$area, mi^2)
  return(CensusCnty)
}


#CensusTracts need to include PRTC jursidictions
NOVA <- c("Arlington",
               "Fairfax County",
               "Fairfax city",
               "Loudoun",
               "Alexandria City",
               "Falls Church City")
CensusDataTracts <- countycensus_tract(NOVA) %>%
  st_transform(crs = 4326) %>%
  select(-GEOID)
Nova <- CensusDataTracts %>% st_union()

#Create Job Census Tract data

### ADD EMPLOYMENT DATA
Jobs <- st_read("Z:/NVTC General/Projects and Programs/Transit Resource Center (TRC)/Data/MWCOG Forecasts/DRAFT_COG_Cooperative_Forecast_9.2/DRAFT_COG_Cooperative_Forecast_9.2.shp") %>%
  st_transform(crs = 4326) %>% st_intersection(Nova) %>% select(NAME, EMP2015, EMP2020, EMP2025)

#interpolate the employement data to census tracts then find centroid
JobsTracts <- st_interpolate_aw(Jobs %>% select(-NAME), CensusDataTracts, extensive = T) %>%
  st_centroid(.)


### Join Job data to census data so you have job data in census tract form WITH county  identified
JobJoin <- st_join(CensusDataTracts, JobsTracts) %>% na.omit()


#create individual polygons for each jurisdiction
Arl <- JobJoin %>% filter(grepl('Arl', NAME)) %>% select(-NAME)
Alx <- JobJoin %>% filter(grepl('Alexandria', NAME)) %>% select(-NAME)
CityFFX <- JobJoin %>% filter(grepl('Fairfax city', NAME)) %>% select(-NAME)
Lou <- JobJoin %>% filter(grepl('Loudoun', NAME)) %>% select(-NAME)
fc <- JobJoin %>% filter(grepl('Falls Church', NAME)) %>% select(-NAME)
ffx <- JobJoin %>% filter(grepl('Fairfax County', NAME)) %>% select(-NAME)

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

#arlington
ArlJobs<- rbind(Access(Arl, "Bus", buff = 400),
                Access(Arl, "CR", buff = 1600),
                Access(Arl, "HR", buff = 800),
                Access(Arl, "Paratransit", buff = 1200)) %>%
  mutate(Jurisdiction = "Arlington County")

#alexandria
AlxJobs <- rbind(Access(Alx, "Bus", buff = 400),
                   Access(Alx, "CR", buff = 1600),
                   Access(Alx, "HR", buff = 800),
                   Access(Alx, "Paratransit", buff = 1200)) %>%
  mutate(Jurisdiction = "City of Alexandria")

#City of Falls Church
fcJobs <- rbind(Access(fc, "Bus", buff = 400),
                  #Access(fc, "CR", buff = 1600),
                  Access(fc, "HR", buff = 800),
                  Access(fc, "Paratransit", buff = 1200)) %>%
  mutate(Jurisdiction = "City of Falls Church")

#city of ffx
CityFFXJobs <- rbind(Access(CityFFX, "Bus", buff = 400),
                       Access(CityFFX, "Paratransit", buff = 1200)) %>%
  mutate(Jurisdiction = "City of Fairfax")

#Fairfax County
ffxJobs <- rbind(Access(ffx, "Bus", buff = 400),
                   Access(ffx, "CR", buff = 1600),
                   Access(ffx, "HR", buff = 800),
                   Access(ffx, "Paratransit", buff = 1200)) %>%
  mutate(Jurisdiction = "Fairfax County")

#Loudoun County
LouJobs <- rbind(Access(Lou, "Bus", buff = 400),
                   #Access(Lou, "CR", buff = 1600),
                   Access(Lou, "HR", buff = 800),
                   Access(Lou, "Paratransit", buff = 1200)) %>%
  mutate(Jurisdiction = "Loudoun County")


#join all
NovaJobs <- rbind(ArlJobs, AlxJobs, ffxJobs, LouJobs, CityFFXJobs, fcJobs) %>%
  select(Jurisdiction, Mode, EMP2015, EMP2020, EMP2025)
st_write(NovaJobs, "AgencyProfileData/NovaJobs.xlsx", delete_dsn = T)


#total jobs by jurisdiction (censusjobtotal)
JobsSum <- rbind(Arl %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "Arlington County"),
                Alx %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "City of Alexandria"),
                Lou %>% st_drop_geometry() %>% summarize_all(sum) %>% mutate(County = "Loudoun County"),
                ffx %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "Fairfax County"),
                fc %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "City of Falls Church"),
                CityFFX %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "City of Fairfax"))

st_write(JobsSum, "AgencyProfileData/JobsSum.xlsx", delete_dsn = T)








