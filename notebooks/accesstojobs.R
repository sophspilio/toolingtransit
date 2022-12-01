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

NovaStopsRoutes_2022 <- st_read("AgencyProfileData/NovaStopsRoutes.shp")
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
JobsTracts <- st_interpolate_aw(Jobs %>% select(-NAME), CensusDataTracts, extensive = T) %>% st_centroid(.)


### Join Job data to census data so you have job data in census tract form WITH county  identified
JobJoin <- st_join(CensusDataTracts, JobsTracts) %>% na.omit()


#create individual polygons for each jurisdiction
Arl <- JobJoin %>% filter(grepl('Arl', NAME)) %>% select(-NAME)
Alx <- JobJoin %>% filter(grepl('Alexandria', NAME)) %>% select(-NAME)
CityFFX <- JobJoin %>% filter(grepl('Fairfax city', NAME)) %>% select(-NAME)
Lou <- JobJoin %>% filter(grepl('Loudoun', NAME)) %>% select(-NAME)
fc <- JobJoin %>% filter(grepl('Falls Church', NAME)) %>% select(-NAME)
ffx <- JobJoin %>% filter(grepl('Fairfax County', NAME)) %>% select(-NAME)



#arlington
ARlCR <- st_interpolate_aw(
  Arl,
  NovaStopsRoutes_2022 %>% filter(Mode == "CR") %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "CR")
ARlBus <- st_interpolate_aw(
  Arl,
  NovaStopsRoutes_2022 %>% filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")
ARlHR <- st_interpolate_aw(
  Arl,
  NovaStopsRoutes_2022 %>% filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "HR")
ARlpara <- st_interpolate_aw(
  Arl,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

ArlJobs <- rbind(ARlBus, ARlHR, ARlCR, ARlpara) %>% mutate(Jurisdiction = "Arlington County")


#Alexandria
AlxCR <- st_interpolate_aw(
  Alx,
  NovaStopsRoutes_2022 %>% filter(Mode == "CR") %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "CR")
AlxBus <- st_interpolate_aw(
  Alx,
  NovaStopsRoutes_2022 %>% filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")
AlxHR <- st_interpolate_aw(
  Alx,
  NovaStopsRoutes_2022 %>% filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "HR")
Alxpara <- st_interpolate_aw(
  Alx,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

AlxJobs <- rbind(AlxBus, AlxHR, AlxCR, Alxpara) %>% mutate(Jurisdiction = "City of Alexandria")

#Fairfax
#Fairfax jobs data includes falls church and city of fairfax as well as
# fairfax county
ffxCR <- st_interpolate_aw(
  ffx,
  NovaStopsRoutes_2022 %>% filter(Mode == "CR") %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "CR")
ffxBus <- st_interpolate_aw(
  ffx,
  NovaStopsRoutes_2022 %>% filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")
ffxHR <- st_interpolate_aw(
  ffx,
  NovaStopsRoutes_2022 %>% filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "HR")
ffxpara <- st_interpolate_aw(
  ffx,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

ffxJobs <- rbind(ffxBus, ffxHR, ffxpara, ffxCR) %>%
  mutate(Jurisdiction = "Fairfax County")

#Loudoun
#no commuter rail or heavy rail in loudoun

louBus <- st_interpolate_aw(
  Lou,
  NovaStopsRoutes_2022 %>% filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")
louHR <- st_interpolate_aw(
  Lou,
  NovaStopsRoutes_2022 %>% filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "HR")


loupara <- st_interpolate_aw(
  Lou,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

LouJobs <- rbind(louBus, louHR, loupara) %>% mutate(Jurisdiction = "Loudoun County")

#Falls Church
fcBus <- st_interpolate_aw(
  fc,
  NovaStopsRoutes_2022 %>%  filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")
fcHR <- st_interpolate_aw(
  fc,
  NovaStopsRoutes_2022 %>%  filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "HR")
fcpara <- st_interpolate_aw(
  fc,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

fcJobs <- rbind(fcBus, fcHR, fcpara) %>% mutate(Jurisdiction = "City of Falls Church")

#City of Fairfax
cityffxBus <- st_interpolate_aw(
  CityFFX,
  NovaStopsRoutes_2022 %>%  filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")

cityffxpara <- st_interpolate_aw(
  CityFFX,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

cityffxJobs <- rbind(cityffxBus, cityffxpara) %>% mutate(Jurisdiction = "City of Fairfax")

#join all
NovaJobs <- rbind(ArlJobs, AlxJobs, ffxJobs, LouJobs, cityffxJobs, fcJobs) %>%
  select(Jurisdiction, Mode, EMP2015, EMP2020, EMP2025)
st_write(NovaJobs, "AgencyProfileData/NovaJobs.xlsx", delete_dsn = T)


#total jobs by jurisdiction (censusjobtotal)
JobsSum <- rbind(Arl %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "Arlington County"),
                Alx %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "City of Alexandria"),
                Lou %>% st_drop_geometry() %>% summarize_all(sum) %>% mutate(County = "Loudoun County"),
                ffx %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "Fairfax County"),
                fc %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "City of Falls Church"),
                CityFFX %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "City of Fairfax"))

st_write(JobsSum, "AgencyProfileData/JobsSum.xlsx")








