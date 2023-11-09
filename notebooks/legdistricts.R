#setup
library(tidyverse)
library(readxl)
library(openxlsx)
library(tidytransit)
library(sf)
library(mapview)
library(tidycensus)
library(censusapi)

Data_path <- file.path ("Z:",
                        "Sophie", "Mapping",
                        "LegDistricts", "data")

TRC_path <- file.path ("Z:",
                        "NVTC General", "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data")


pop <- get_acs(
  geography = "block group",
  year = 2021,
  variables = c(TotalPopulation = "B01003_001",
                Households = "B25044_001"),
  state = "VA",
  county = c("Arlington",
             "Fairfax County",
             "Fairfax city",
             "Loudoun",
             "Alexandria City",
             "Falls Church City",
             "Prince William County",
             "Fauquier County"),
  geometry = TRUE,
  output = "wide")

## va senate----

senate <- st_read(file.path(Data_path, "SCV Final 2021 Redistricting Plans/SCV Final SD.shp"))

Nova <- st_read("data/Nova.shp") %>% st_transform(st_crs(senate))

senate <- senate %>% st_intersection(., Nova)

stops <- st_read("Z:/NVTC General/Projects and Programs/Transit Resource Center (TRC)/Data/GIS Data/R Bus Routes + Stops/NovaStopsRoutes.shp") %>%
  st_transform(st_crs(senate))

x <- st_join(stops, st_make_valid(senate), join = st_within)

#bus routes through district
senate_bus <- x %>% filter(Mode == "Bus") %>% group_by(DISTRICT, Agency, rout_nm) %>% summarize(x = n()) %>%
  group_by(DISTRICT, Agency) %>% summarize(n = n()) %>% st_drop_geometry() %>%
  mutate(Agency = ifelse(Agency == "WMATA", "MetroBus", Agency)) %>%
  pivot_wider(., names_from = "Agency", values_from = "n")

colnames(senate_bus) <- c("District", "Fairfax.Connector", "Loudoun.County.Transit",
                          "MetroBus", "Fairfax.CUE", "DASH", "OmniRide", "ART")



#rail through districts
senate_rail <- x %>% filter(Mode != "Bus") %>% group_by(DISTRICT, Agency) %>% summarize(n = n()) %>%
  mutate(Agency = ifelse(Agency == "WMATA", "MetroRail", Agency)) %>% st_drop_geometry() %>%
  pivot_wider(., names_from = "Agency", values_from = "n")

senate_transit <- left_join(senate_bus, senate_rail) %>% mutate(Total.Bus = sum(FFX, LCT, MetroBus, CUE, DASH, PRTC, ART, na.rm = TRUE))

st_write(senate_transit, file.path(Data_path, "senate_transit.csv"))

#households and employment in district

senate_pt <- st_point_on_surface(senate %>% st_make_valid)


senate_pop <- st_interpolate_aw(pop %>% select(TotalPopulationE, HouseholdsE),
                                st_make_valid(senate),
                                extensive = TRUE) %>%
  st_join(., senate_pt,  join = st_contains)


jobs <- st_read(file.path(TRC_path,
                          "MWCOG Forecasts/DRAFT_COG_Cooperative_Forecast_9.2/DRAFT_COG_Cooperative_Forecast_9.2.shp")) %>%
  st_transform(st_crs(senate_pop))

senate_jobs <- st_interpolate_aw(jobs %>% select(EMP2020),
                  st_make_valid(senate),
                  extensive = TRUE) %>%
  st_join(., senate_pt,  join = st_contains)

senate_demographics <- inner_join(st_drop_geometry(senate_jobs), st_drop_geometry(senate_pop))
st_write(senate_demographics, file.path(Data_path, "senate_demographics.csv"))

## va house of delegates----
house <- st_read(file.path(Data_path, "SCV Final 2021 Redistricting Plans/SCV Final HOD.shp"))

Nova <- st_read("data/Nova.shp") %>% st_transform(st_crs(house))

stops <- st_read("Z:/NVTC General/Projects and Programs/Transit Resource Center (TRC)/Data/GIS Data/R Bus Routes + Stops/NovaStopsRoutes.shp") %>%
  st_transform(st_crs(house))

x <- st_join(stops, st_make_valid(house), join = st_within)

house_bus <- x %>% filter(Mode == "Bus") %>% group_by(DISTRICT, Agency, rout_nm) %>% summarize(x = n()) %>%
  group_by(DISTRICT, Agency) %>% summarize(n = n()) %>% st_drop_geometry() %>%
  mutate(Agency = ifelse(Agency == "WMATA", "MetroBus", Agency)) %>%
  pivot_wider(., names_from = "Agency", values_from = "n")


house_rail <- x %>% filter(Mode != "Bus") %>% group_by(DISTRICT, Agency) %>% summarize(n = n()) %>%
  mutate(Agency = ifelse(Agency == "WMATA", "MetroRail", Agency)) %>% st_drop_geometry() %>%
  pivot_wider(., names_from = "Agency", values_from = "n")

house_transit <- left_join(house_bus, house_rail) %>% mutate(Total.Bus = sum(FFX, LCT, MetroBus, CUE, DASH, PRTC, ART, na.rm = TRUE))

st_write(house_transit, file.path(Data_path, "house_transit.csv"))

#household and employment
house_pt <- st_point_on_surface(house %>% st_make_valid)

house_pop <- st_interpolate_aw(pop %>% select(TotalPopulationE, HouseholdsE),
                                st_make_valid(house),
                                extensive = TRUE) %>%
  st_join(., house_pt,  join = st_contains)

house_jobs <- st_interpolate_aw(jobs %>% select(EMP2020),
                                 st_make_valid(house),
                                 extensive = TRUE) %>%
  st_join(., house_pt,  join = st_contains)

house_demographics <- inner_join(st_drop_geometry(house_jobs), st_drop_geometry(house_pop))

st_write(house_demographics, file.path(Data_path, "house_demographics.csv"))


## us house
ushouse <- st_read(file.path(Data_path, "SCV Final 2021 Redistricting Plans/SCV Final CD.shp"))

Nova <- st_read("data/Nova.shp") %>% st_transform(st_crs(ushouse))

stops <- st_read("Z:/NVTC General/Projects and Programs/Transit Resource Center (TRC)/Data/GIS Data/R Bus Routes + Stops/NovaStopsRoutes.shp") %>%
  st_transform(st_crs(ushouse))

x <- st_join(stops, st_make_valid(ushouse), join = st_within)

ushouse_bus <- x %>% filter(Mode == "Bus") %>% group_by(DISTRICT, Agency, rout_nm) %>% summarize(x = n()) %>%
  group_by(DISTRICT, Agency) %>% summarize(n = n()) %>% st_drop_geometry() %>%
  mutate(Agency = ifelse(Agency == "WMATA", "MetroBus", Agency)) %>%
  pivot_wider(., names_from = "Agency", values_from = "n")


ushouse_rail <- x %>% filter(Mode != "Bus") %>% group_by(DISTRICT, Agency) %>% summarize(n = n()) %>%
  mutate(Agency = ifelse(Agency == "WMATA", "MetroRail", Agency)) %>% st_drop_geometry() %>%
  pivot_wider(., names_from = "Agency", values_from = "n")

ushouse_transit <- left_join(ushouse_bus, ushouse_rail) %>% mutate(Total.Bus = sum(FFX, LCT, MetroBus, CUE, DASH, PRTC, ART, na.rm = TRUE))

st_write(ushouse_transit, file.path(Data_path, "ushouse_transit.csv"))



#household and employment
ushouse_pt <- st_point_on_surface(ushouse %>% st_make_valid)

ushouse_pop <- st_interpolate_aw(pop %>% select(TotalPopulationE, HouseholdsE),
                               st_make_valid(ushouse),
                               extensive = TRUE) %>%
  st_join(., ushouse_pt,  join = st_contains)

jobs <- st_read(file.path(TRC_path,
                          "MWCOG Forecasts/DRAFT_COG_Cooperative_Forecast_9.2/DRAFT_COG_Cooperative_Forecast_9.2.shp")) %>%
  st_transform(st_crs(ushouse_pop))

ushouse_jobs <- st_interpolate_aw(jobs %>% select(EMP2020),
                                st_make_valid(ushouse),
                                extensive = TRUE) %>%
  st_join(., ushouse_pt,  join = st_contains)

ushouse_demographics <- inner_join(st_drop_geometry(ushouse_jobs), st_drop_geometry(ushouse_pop))

st_write(ushouse_demographics, file.path(Data_path, "ushouse_demographics.csv"))

##VRE ----

vrestops <- st_read(file.path(TRC_path, "GIS Data/R Bus Routes + Stops/VREStops.shp"))
#house
vrehouse <- st_intersection(vrestops, st_transform(house ,crs = 4326))

vrehousedf <- inner_join(aggregate(LongName~DISTRICT,data=vrehouse,FUN = function(x) paste0(x,collapse = ', ')),
           vrehouse %>% st_drop_geometry() %>% group_by(DISTRICT) %>% summarize(countstops = n()))

st_write(vrehousedf, file.path(Data_path, "VRELeghouse.csv"))

#senate
vresenate <- st_intersection(vrestops, st_transform(senate, crs = 4326))

vresenatedf <- inner_join(aggregate(LongName~DISTRICT,data = vresenate,FUN = function(x) paste0(x,collapse = ', ')),
           vresenate %>% st_drop_geometry() %>% group_by(DISTRICT) %>% summarize(countstops = n()))

st_write(vresenatedf, file.path(Data_path, "VRELegsenate.csv"))

## Posters ----
pop <- get_acs(
  geography = "county",
  year = 2021,
  variables = c(TotalPopulation = "B01003_001",
                Households = "B25044_001"),
  state = "VA",
  county = c("Arlington",
             "Fairfax County",
             "Fairfax city",
             "Loudoun",
             "Alexandria City",
             "Falls Church City",
             "Prince William County",
             #"Fauquier County",
             "Stafford County",
             "Fredericksburg city",
             "Spotsylvania County",
             "Manassas city",
             "Manassas Park city"),
  geometry = TRUE,
  output = "wide")


## New Elected Handouts----
senate <- st_read(file.path(Data_path, "StateSenateDistricts.shp"))
house <- st_read(file.path(Data_path, "HouseofDelegates.shp"))
counties <- st_read(file.path(Data_path, "LegMapsCounties.shp")) %>% st_transform(crs = st_crs(senate))

sen_county <- st_join(senate, counties) %>% select(NAME, DISTRICT) %>% st_drop_geometry() %>%
  group_by(DISTRICT) %>% summarize(counties = paste0(NAME, collapse = ", "))
st_write(sen_county, file.path(Data_path, "SenateCounties.xlsx"), delete_dsn = TRUE)


house_county <- st_join(house, counties) %>% select(NAME, DISTRICT) %>%
  st_drop_geometry() %>% group_by(DISTRICT) %>% summarize(counties = paste0(NAME, collapse = ", "))
st_write(house_county, file.path(Data_path, "HouseCounties.xlsx"))
