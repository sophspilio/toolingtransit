#State of the Stop Calcs
library(tidyverse)
library(tidytransit)
library(sf)
library(mapview)
library(tidycensus)

path <- file.path("Z:","Sophie","Mapping","StateoftheStop","data")


NovaShape <- get_acs(geography = "county",
year = 2021,
variables = c(TotalPopulation = "B01003_001"),
state = "VA",
county = c("Arlington", "Alexandria", "Fairfax county", "Fairfax city",
           "Falls Church city", "Loudoun", "Prince William", "Manassas city",
           "Manassas Park City"),
geometry = TRUE,
output = "wide"
) %>% st_transform(crs = 4326)


samplestops <- read_csv(file.path(path, "samplestops.csv")) %>%
  st_as_sf(., coords = c("stop_lon", "stop_lat"), crs = 4326)

## Roadway Ownership/Maintenance ----

#roads

ffxcode <- tibble(short = 0:14,
                  long = c("private", "municipal", "county", "state",
                           "federal", "vdot", "na", "fairfax dpwes",
                           "fairfax park authority", "mwaa", "fairfax public schools",
                           "nvpra", "na", "transurban", "i-66 express mobility"))

ffxcode <- tibble(short = 0:14,
                  long = c("other", "local", "local", "state",
                           "other", "state", NA, "local",
                           "local", "other", "local",
                           "other", NA, "other", "other"))

ffx <- st_read(file.path(path, "FFXRoadway_Centerlines.shp")) %>%
  select(FULLNAME, MAINT_RES) %>%
  mutate(MAINT_RES = ifelse(MAINT_RES %in% ffxcode$short, ffxcode$long)) %>%
  st_intersection(., NovaShape %>% filter(NAME == "Fairfax County, Virginia"))

pw <- st_read(file.path(path, "PWRoads.shp")) %>%
  select(STREET_NAM, ROAD_MAINT) %>%
  st_intersection(., NovaShape %>% filter(NAME == "Prince William County, Virginia"))

lou <- st_read(file.path(path, "LoudounRoads.shp")) %>%
  select(ST_STR_NAM, CE_OWNER) %>% st_transform(crs = 4326) %>%
  st_intersection(., NovaShape %>% filter(NAME == "Loudoun County, Virginia"))


roadsbuff <- rbind(ffx %>% rename(name = FULLNAME, maint_own = MAINT_RES),
      pw %>% rename(name = STREET_NAM, maint_own = ROAD_MAINT),
      lou %>% rename(name = ST_STR_NAM, maint_own = CE_OWNER)) %>%
  st_buffer(dist = 10)

stops_roads <- st_join(samplestops, roadsbuff %>% select(name, maint_own), join = st_within)


x <- stops_roads %>% mutate(maint_own = ifelse(NAME == "Falls Church city, Virginia", "local",
                       ifelse(NAME == "Alexandria city, Virginia", "local",
                              ifelse(NAME == "Arlington County, Virginia" & grepl("RICHMOND", stop_name), "state",
                                     ifelse(NAME == "Arlington County, Virginia" & grepl("WASHINGTON BLVD", stop_name), "state",
                                            ifelse(NAME == "Arlington County, Virginia" & grepl("ARLINGTON BLVD", stop_name),"state",
                                                    ifelse(NAME == "Arlington County, Virginia" & grepl("GLEBE", stop_name), "state",
                                                            ifelse(NAME == "Arlington County, Virginia" & grepl("OLD DOMINION", stop_name),"state",
                                                                   ifelse(NAME == "Arlington County, Virginia" & grepl("CHAIN BRIDGE", stop_name), "state",
                                                                          ifelse(NAME == "Arlington County, Virginia", "local",
                                                                                 ifelse(maint_own == "na", NA, maint_own))))))))))) %>%
  mutate(maint_own = tolower(maint_own)) %>% distinct()

st_write(x %>% select(-TotalPopulationM, -TotalPopulationE, -name),
         "Z:/Fellow/Rachel Inman 2023/roadsownership_bystop.xlsx", delete_layer = TRUE)

st_write(x, file.path(path, "stops_roads.shp"), delete_layer = TRUE)

rbind(ffx %>% rename(name = FULLNAME, maint_own = MAINT_RES),
      pw %>% rename(name = STREET_NAM, maint_own = ROAD_MAINT),
      lou %>% rename(name = ST_STR_NAM, maint_own = CE_OWNER)) %>%
  st_write(file.path(path, "someroads.shp"))

x %>% group_by(maint_own) %>% summarize(n = n())




## Census Data around Stops----
County <- c("Arlington", "Alexandria", "Fairfax County", "Falls Church City", "Fairfax City", "Loudoun",
            "Prince William", "Manassas Park City", "Manassas City")

census <- function(yr){
  Census <- get_acs(
    geography = "block group",
    year = yr,
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
    output = "wide") %>%

    #calculate new variables
    mutate(Count0Car = HHOwn0VehE + HHRent0VehE,
           NonWhitePop = TotalPopulationE - WhitePopE,
           Under200PctPoverty = PovertyRatioTotalE - PovertyOver200PctE
    ) %>%
    #select only necessary variables (remove margin of error columns)
    select(TotalPopulationE, HHCountE, Count0Car, TotalCommutersE, PublicTransportE, NonWhitePop, Under200PctPoverty, GEOID) %>%
    st_transform(crs=4326)
}

Census2021 <- census(2021)
Census2017 <- census(2017)
Census2013 <- census(2013)

#quarter mile buffer around sample stops
stops_buff <- st_buffer(samplestops, dist=400)

#interpolate census data to each stop buffer (1/4 mile)
census2021_stops <- st_interpolate_aw(Census2021 %>%
                                        select(-GEOID), stops_buff, extensive = TRUE) %>%
  st_join(stops_buff, ., join = st_equals)

census2017_stops <- st_interpolate_aw(Census2017 %>%
                                        select(-GEOID), stops_buff, extensive = TRUE) %>%
  st_join(stops_buff, ., join = st_equals)

census2013_stops <- st_interpolate_aw(Census2013 %>%
                                        select(-GEOID), stops_buff, extensive = TRUE) %>%
  st_join(stops_buff, ., join = st_equals)

#employment data from MWCOG
jobs <- st_read("Z:/NVTC General/Projects and Programs/Transit Resource Center (TRC)/
                Data/MWCOG Forecasts/DRAFT_COG_Cooperative_Forecast_9.2/
                DRAFT_COG_Cooperative_Forecast_9.2.shp") %>%
  st_transform(crs = 4326)

#interpolate job data to each stop buffer (1/4 mile)
jobs_stops <- st_interpolate_aw(jobs %>%
                                  select(EMP2020, EMP2015), stops_buff, extensive = TRUE) %>%
  st_join(stops_buff, ., join = st_equals)

st_write(census2013_stops, "Z:/Fellow/Rachel Inman 2023/stops_demographics.xlsx",
         layer = "Census2013")

st_write(census2017_stops, "Z:/Fellow/Rachel Inman 2023/stops_demographics.xlsx",
         layer = "Census2017")

st_write(census2021_stops, "Z:/Fellow/Rachel Inman 2023/stops_demographics.xlsx",
         layer = "Census2021")

st_write(jobs_stops, "Z:/Fellow/Rachel Inman 2023/stops_demographics.xlsx",
         layer = "Jobs")



## StopIDs over time-----
gtfs_path <- file.path("Z:", "Fellow", "Rachel Inman 2023", "bus stop amenities", "GTFS")

samplestops <- read_csv(file.path(path, "samplestops.csv")) %>%
  st_as_sf(., coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  select(agency, stop_id, stop_code, stop_name)

#ART
art14 <- read_gtfs(file.path(gtfs_path, "ART/art gtfs/ART gtfs (2014).zip"))
art18 <- read_gtfs(file.path(gtfs_path, "ART/art gtfs/ART gtfs (2018).zip"))
art22 <- read_gtfs(file.path(gtfs_path, "ART/art gtfs/2022-04_Arlington.zip"))
art23 <- read_gtfs(file.path(gtfs_path, "ART/art gtfs/ART gtfs (2023).zip"))

art23s <- samplestops %>% filter(agency == "ART" & stop_id %in% art23$stops$stop_id)


art23_22 <- st_join(art23s, stops_as_sf(art22$stops) %>% select(stop_id, stop_name), join = st_nearest_feature, suffix = c("23", "22"))
art23_18 <- st_join(art23s, stops_as_sf(art18$stops) %>% select(stop_id, stop_name), join = st_nearest_feature, suffix = c("23", "18"))
art23_14 <- st_join(art23s, stops_as_sf(art14$stops) %>% select(stop_id, stop_name), join = st_nearest_feature, suffix = c("23", "14"))

art23_22_18_14 <- inner_join(art23_22, art23_18 %>% st_drop_geometry() %>% select(stop_id23, stop_id18)) %>%
  inner_join(., st_drop_geometry(art23_14) %>% select(stop_id23, stop_id14))

st_write(art23_22_18_14, file.path(path, "artref_stop_id.csv"))

#CUE
cue14 <- read_gtfs(file.path(gtfs_path, "CUE/cue gtfs/CUE gtfs (2015).zip"))
cue22 <- read_gtfs(file.path(gtfs_path, "CUE/cue gtfs/2022-03_CUE.zip"))
cue23 <- read_gtfs(file.path(gtfs_path, "CUE/cue gtfs/CUE GTFS 4-25-23.zip"))

cue23s <- samplestops %>% filter(agency == "CUE" & stop_id %in% cue23$stops$stop_id)

cue23_22 <- st_join(cue23s, stops_as_sf(cue22$stops) %>% select(stop_id, stop_name), join = st_nearest_feature, suffix = c("23", "22"))
cue23_14 <- st_join(cue23s, stops_as_sf(cue14$stops) %>% select(stop_id, stop_name), join = st_nearest_feature, suffix = c("23", "14"))

cue23_22_14 <- inner_join(cue23_22, cue23_14 %>% st_drop_geometry() %>% select(stop_id23, stop_id14))

st_write(cue23_22_14 , file.path(path, "cueref_stop_id.csv"))

#DASH
dash14 <- read_gtfs(file.path(gtfs_path, "DASH/dash gtfs/DASH gtfs (2015).zip"))
dash18 <- read_gtfs(file.path(gtfs_path, "DASH/dash gtfs/DASH gtfs (2018).zip"))
dash22 <- read_gtfs(file.path(gtfs_path, "DASH/dash gtfs/2022-04_DASH.zip"))
dash23 <- read_gtfs(file.path(gtfs_path, "DASH/dash gtfs/google_transit (23_24).zip"))

dash23s <- samplestops %>% filter(agency == "DASH" & stop_id %in% dash23$stops$stop_id)


dash23_22 <- st_join(dash23s, stops_as_sf(dash22$stops) %>% select(stop_id, stop_name), join = st_nearest_feature, suffix = c("23", "22"))
dash23_18 <- st_join(dash23s, stops_as_sf(dash18$stops) %>% select(stop_id, stop_name), join = st_nearest_feature, suffix = c("23", "18"))
dash23_14 <- st_join(dash23s, stops_as_sf(dash14$stops) %>% select(stop_id, stop_name), join = st_nearest_feature, suffix = c("23", "14"))

dash23_22_18_14 <- inner_join(dash23_22, dash23_18 %>% st_drop_geometry() %>% select(stop_id23, stop_id18)) %>%
  inner_join(., st_drop_geometry(dash23_14) %>% select(stop_id23, stop_id14))

st_write(dash23_22_18_14, file.path(path, "dashref_stop_id.csv"))

#FFX
ffx14 <- read_gtfs(file.path(gtfs_path, "Fairfax Connector/ffx gtfs/FFX gtfs (july 2014)-ss.zip"))
ffx17 <- read_gtfs(file.path(gtfs_path, "Fairfax Connector/ffx gtfs/FFX gtfs (dec 2017).zip"))
ffx22 <- read_gtfs(file.path(gtfs_path, "Fairfax Connector/ffx gtfs/2022-03_Fairfax_Connector.zip"))
ffx23 <- read_gtfs(file.path(gtfs_path, "Fairfax Connector/ffx gtfs/FFX gtfs (2023).zip"))

ffx23s <- samplestops %>% filter(agency == "Fairfax Connector" & stop_id %in% ffx23$stops$stop_id)


ffx23_22 <- st_join(ffx23s, stops_as_sf(ffx22$stops) %>% select(stop_id), join = st_nearest_feature, suffix = c("23", "22"))
ffx23_17 <- st_join(ffx23s, stops_as_sf(ffx17$stops) %>% select(stop_id), join = st_nearest_feature, suffix = c("23", "17"))
ffx23_14 <- st_join(ffx23s, stops_as_sf(ffx14$stops) %>% select(stop_id), join = st_nearest_feature, suffix = c("23", "14"))

ffx23_22_17_14 <- inner_join(ffx23_22, st_drop_geometry(ffx23_17) %>% select(stop_id23, stop_id17)) %>%
  inner_join(., st_drop_geometry(ffx23_14) %>% select(stop_id23, stop_id14))

st_write(ffx23_22_17_14, file.path(path, "ffxref_stop_id.csv"))

#OmniRide
prtc14 <- read_gtfs(file.path(gtfs_path, "OmniRide/omniride gtfs/OmniRide gtfs (2014).zip"))
prtc18 <- read_gtfs(file.path(gtfs_path, "OmniRide/omniride gtfs/OmniRide gtfs (2018)-ss.zip"))
prtc22 <- read_gtfs(file.path(gtfs_path, "OmniRide/omniride gtfs/2022-03_OmniRide.zip"))
prtc23 <- read_gtfs(file.path(gtfs_path, "OmniRide/omniride gtfs/OmniRide gtfs (2023)-ss.zip"))

prtc23s <- samplestops %>% filter(agency == "OmniRide" & stop_id %in% prtc23$stops$stop_id)


prtc23_22 <- st_join(prtc23s, stops_as_sf(prtc22$stops) %>% select(stop_id), join = st_nearest_feature, suffix = c("23", "22"))
prtc23_18 <- st_join(prtc23s, stops_as_sf(prtc18$stops) %>% select(stop_id), join = st_nearest_feature, suffix = c("23", "18"))
prtc23_14 <- st_join(prtc23s, stops_as_sf(prtc14$stops) %>% select(stop_id), join = st_nearest_feature, suffix = c("23", "14"))

prtc23_22_18_14 <- inner_join(prtc23_22, st_drop_geometry(prtc23_18) %>% select(stop_id23, stop_id18)) %>%
  inner_join(., st_drop_geometry(prtc23_14) %>% select(stop_id23, stop_id14))

st_write(prtc23_22_18_14, file.path(path, "prtcref_stop_id.csv"))

#Loudoun
lct22 <- read_gtfs(file.path(gtfs_path, "Loudoun/loudoun gtfs/2022-03_Loudoun.zip"))
lct23 <- read_gtfs(file.path(gtfs_path, "Loudoun/loudoun gtfs/LCT gtfs (2023).zip"))

lct23s <- samplestops %>% filter(agency == "Loudoun County Transit" & stop_id %in% lct23$stops$stop_id)


lct23_22 <- st_join(lct23s, stops_as_sf(lct22$stops) %>% select(stop_id), join = st_nearest_feature, suffix = c("23", "22"))

st_write(lct23_22, file.path(path, "lctref_stop_id.csv"))

#WMATA
wmata14 <- read_gtfs(file.path(gtfs_path, "WMATA/wmata gtfs (april 2014).zip"))
wmata18 <- read_gtfs(file.path(gtfs_path, "WMATA/wmata gtfs (may 2018).zip"))
wmata22 <- read_gtfs(file.path(gtfs_path, "WMATA/2022-04_WMATA.zip"))
wmata23 <- read_gtfs(file.path(gtfs_path, "WMATA/2023-06_Metrobus.zip"))

wmata23s <- samplestops %>% filter(agency == "WMATA" & stop_id %in% wmata23$stops$stop_id)


wmata23_22 <- st_join(wmata23s, stops_as_sf(wmata22$stops) %>% select(stop_id, stop_name), join = st_nearest_feature, suffix = c("23", "22"))
wmata23_18 <- st_join(wmata23s, stops_as_sf(wmata18$stops) %>% select(stop_id, stop_name), join = st_nearest_feature, suffix = c("23", "18"))
wmata23_14 <- st_join(wmata23s, stops_as_sf(wmata14$stops) %>% select(stop_id, stop_name), join = st_nearest_feature, suffix = c("23", "14"))


wmata23_22_18_14 <- inner_join(wmata23_22, st_drop_geometry(wmata23_18) %>% select(stop_id23, stop_id18)) %>%
  inner_join(., st_drop_geometry(wmata23_14) %>% select(stop_id23, stop_id14))

st_write(wmata23_22_18_14, file.path(path, "wmataref_stop_id.csv"))


## Join Survye Data with spatial data ------

survey <- readxl::read_excel(file.path(path, "0725 amenity responses.xlsx"))


surveystops <- full_join(survey, samplestops)
surveystops %>% filter(`Year in Street View` == "2022") %>%
  st_write(file.path(path, "surveysops2022.shp"))
surveystops %>% filter(`Year in Street View` == "2018") %>%
  st_write(file.path(path, "surveystops2018.shp"))
surveystops %>% filter(`Year in Street View` == "2014") %>%
  st_write(file.path(path, "surveystops2014.shp"))
