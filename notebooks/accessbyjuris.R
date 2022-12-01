#load libraries
library(tidyverse)
library(censusapi)
library(tidycensus)
library(sf)
library(units)
library(tidytransit)
library(toolingtransit)
library(mapview)
#CensusTracts need to include PRTC jursidictions
NOVA <- c("Arlington",
               "Fairfax County",
               "Fairfax city",
               "Loudoun",
               "Alexandria City",
               "Falls Church City")


NovaStopsRoutes_2022 <- st_read("AgencyProfileData/NovaStopsRoutes.shp")

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
CensusDataTracts <- countycensus_tract(NOVA) %>%
  st_transform(crs = 4326) %>%
  select(-GEOID)

Arl <- countycensus_tract("Arlington") %>% st_transform(crs = 4326) %>% select(-GEOID)
Alx <- countycensus_tract("Alexandria city") %>% st_transform(crs = 4326) %>% select(-GEOID)
CityFFX <- countycensus_tract("Fairfax city") %>% st_transform(crs = 4326) %>% select(-GEOID)
Lou <- countycensus_tract("Loudoun") %>% st_transform(crs = 4326) %>% select(-GEOID)
fc <- countycensus_tract("Falls Church city") %>% st_transform(crs = 4326) %>% select(-GEOID)
ffx <- countycensus_tract("Fairfax County") %>% st_transform(crs = 4326) %>% select(-GEOID)


#bus = 1/4 mile (400m)
#hr = 1/2 mile (800m)
#para = 3/4 mile (1200m)
#cr = 1 mile (1600m)

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

ArlAccess <- rbind(ARlBus, ARlHR, ARlCR, ARlpara) %>% mutate(Jurisdiction = "Arlington County")
#falls church
fcCR <- st_interpolate_aw(
  fc,
  NovaStopsRoutes_2022 %>% filter(Mode == "CR") %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "CR")
fcHR <- st_interpolate_aw(
  fc,
  NovaStopsRoutes_2022 %>% filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "HR")
fcBus <- st_interpolate_aw(
  fc,
  NovaStopsRoutes_2022 %>% filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")
fcpara <- st_interpolate_aw(
  fc,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

fcAccess <- rbind(fcHR, fcBus, fcpara) %>% mutate(Jurisdiction = "City of Falls Church")
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

AlxAccess <- rbind(AlxBus, AlxHR, AlxCR, Alxpara) %>% mutate(Jurisdiction = "City of Alexandria")
#City of Fairfax
CityFFXCR <- st_interpolate_aw(
  CityFFX,
  NovaStopsRoutes_2022 %>% filter(Mode == "CR") %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "CR")
CityFFXBus <- st_interpolate_aw(
  CityFFX,
  NovaStopsRoutes_2022 %>% filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")
CityFFXHR <- st_interpolate_aw(
  CityFFX,
  NovaStopsRoutes_2022 %>% filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "HR")
CityFFXpara <- st_interpolate_aw(
  CityFFX,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

CityFFXAccess <- rbind(CityFFXBus, CityFFXpara) %>% mutate(Jurisdiction = "City of Fairfax")
#Fairfax
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

ffxAccess <- rbind(ffxBus, ffxHR, ffxpara, ffxCR) %>% mutate(Jurisdiction = "Fairfax County")
#Loudoun
louCR <- st_interpolate_aw(
  Lou,
  NovaStopsRoutes_2022 %>% filter(Mode == "CR") %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "CR")
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

LouAccess <- rbind(louHR, louBus, loupara) %>% mutate(Jurisdiction = "Loudoun County")

NovaAcessJurisdictions <- rbind(LouAccess,
                                ArlAccess,
                                AlxAccess,
                                ffxAccess,
                                CityFFXAccess,
                                fcAccess)
st_write(NovaAcessJurisdictions, "AgencyProfileData/NovaAccessJurisdictions.xlsx", delete_dsn = T)


NovaCensus <- rbind(Arl %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "Arlington"),
                    Alx %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "Alexandria"),
                    Lou %>% st_drop_geometry() %>% summarize_all(sum) %>% mutate(County = "Loudoun"),
                    ffx %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "Fairfax"),
                    fc %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "Falls Church"),
                    CityFFX %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "CityFairfax"))

st_write(NovaCensus, "AgencyProfileData/NovaCensusSum.xlsx")


