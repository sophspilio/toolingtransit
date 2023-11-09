library(tidyverse)
library(censusapi)
library(tidycensus)
library(sf)
library(mapview)
library(ggplot2)
library(kableExtra)
library(scales)
library(xlsx)
Work_path <- file.path("Z:", "NVTC General", "Projects and Programs",
                       "Transit Resource Center (TRC)", "Data Projects",
                       "23-03 NoVA Transit Trends", "Analysis")

#1. RTS ##################

#RTS trip file
tripfile <- read_csv(file.path(Work_path, "rtsdata/trip.csv")) %>% rename_with(tolower) %>%
  unite(d_state_county_fips, d_tract_fips,
        col = "d_fips", sep = "", remove = FALSE) %>%
  unite(o_state_county_fips, o_tract_fips,
        col = "o_fips", sep = "", remove = FALSE) %>%
  mutate(oridest = paste(o_fips, d_fips, sep = "-"))


# Identify census tracts within dmv
#use 2017 census data to match 2017 rts tract data
dmv_tracts <- get_acs(geography = "tract",
                      year = 2017,
                      variables = c(TotalPopulation = "B01003_001"),
                      state = c("VA", "MD", "DC", "WV", "PA", "NJ", "NC", "MA",
                                "FL", "NY", "OH", "NH"),
                      geometry = TRUE,
                      output = "wide") %>%
  st_transform(crs = 4326) %>% st_drop_geometry() %>%
  select(GEOID, NAME) %>% separate(NAME, into = c("tract", "county", "state"), ", ")

county <- get_acs(geography = "county",
        year = 2017,
        state = c("VA", "MD", "DC"),
        variables = c(TotalPopulation = "B01003_001"),
        geometry = FALSE,
        output = "wide") %>%
  rename(county = NAME) %>%
  separate(county, into = c("county", "state"), sep = ", ") %>%
  select(GEOID, county)

states <- get_acs(geography = "state",
        year = 2017,
        variables = c(TotalPopulation = "B01003_001"),
        geometry = TRUE,
        output = "wide") %>%
  st_transform(crs = 4326) %>% st_drop_geometry() %>%
  select(GEOID, NAME) %>% rename(state = NAME)

#regions
dmv <- c("Virginia", "Maryland", "District of Columbia")
compact <- c("Arlington County", "Alexandria city", "Falls Church city",
             "Fairfax city", "Fairfax County", "Loudoun County",
             "Prince William County", "Manassas Park city", "Manassas city",
             "District of Columbia", "Prince George's County", "Montgomery County")

nova <- c("Arlington County", "Alexandria city", "Falls Church city",
          "Fairfax city", "Fairfax County", "Loudoun County",
          "Prince William County", "Manassas Park city", "Manassas city",
          "Maryland", "District of Columbia")

#travel mode lookup table
mode_df <- read_csv(file.path(Work_path, "rtsdata/travelmode.csv")) %>% as_tibble()

#travel purpose lookup table
activity_df <- read.xlsx(file.path(Work_path, "rtsdata/ActivityPurpose.xlsx"),
                         sheetName = "activity") %>% as_tibble()
activity_lookup <- read.xlsx(file.path(Work_path, "rtsdata/ActivityPurpose.xlsx"),
                             sheetName = "lookup") %>% as_tibble()

#All Trips w/ activity and trip mode
trip <- tripfile %>%
  select(tripid, o_fips, d_fips,
         o_state_fips, o_state_county_fips,
         d_state_fips, d_state_county_fips,
         oridest,
         mpo_travel_mode,
         o_activity, d_activity,
         distance, wttrdfin, wwm_wttrdfin) %>%
  #filter travel mode NAs
  filter(mpo_travel_mode != -9) %>%
  filter(o_activity != -9,
         d_activity != -9) %>%
  #join with mode ids
  full_join(., mode_df, by = c("mpo_travel_mode" = "ModeID")) %>%
  #join with trip activities
  full_join(., activity_df, by = c("o_activity" = "Label")) %>%
  full_join(., activity_df, by = c("d_activity" = "Label"), suffix = c("_o" ,"_d")) %>%
  mutate(Activity_od = paste(Activity_o, Activity_d, sep = "-")) %>%
  full_join(., activity_lookup) %>%
  #join dest state
  right_join(states, ., by = c("GEOID" = "d_state_fips")) %>%
  rename(d_state_fips = GEOID) %>%
  #join origin state
  right_join(states, ., by = c("GEOID" = "o_state_fips"), suffix = c("_o", "_d")) %>%
  rename(o_state_fips = GEOID) %>%
  #join dest county
  right_join(county, ., by = c("GEOID" = "d_state_county_fips")) %>%
  rename(d_state_county_fips = GEOID) %>%
  #join origin county
  right_join(county, ., by = c("GEOID" = "o_state_county_fips"), suffix = c("_o", "_d")) %>%
  rename(o_state_county_fips = GEOID) %>%
  #calculate trip miles
  mutate(wtt_tripmiles = wttrdfin*distance, wmm_tripmiles = wwm_wttrdfin*distance) %>%
  #remove invalid origin and dest state codes
  filter(!is.na(state_o) & !is.na(state_d)) %>%
  #id counties based on region
  mutate(region_o = ifelse(county_o %in% compact, "GreaterWash",
                           ifelse(state_o == "Maryalnd", "RestofMd",
                                  ifelse(state_o == "Virginia", "RestofVa",
                                         "OutsideRegion"))),
         region_d = ifelse(county_d %in% compact, "GreaterWash",
                           ifelse(state_d == "Maryalnd", "RestofMd",
                                  ifelse(state_d == "Virginia", "RestofVa",
                                         "OutsideRegion"))))

st_write(trip, file.path(Work_path, "TripPurpose.xlsx"))

#All Trips just trip mode
trip <- tripfile %>%
  select(tripid, o_fips, d_fips,
         o_state_fips, o_state_county_fips,
         d_state_fips, d_state_county_fips,
         oridest, mpo_travel_mode,
         distance, wttrdfin, wwm_wttrdfin) %>%
  #filter travel mode NAs
  filter(mpo_travel_mode != -9) %>%
  #join with mode ids
  full_join(., mode_df, by = c("mpo_travel_mode" = "ModeID")) %>%
  #join dest state
  right_join(states, ., by = c("GEOID" = "d_state_fips")) %>%
  rename(d_state_fips = GEOID) %>%
  #join origin state
  right_join(states, ., by = c("GEOID" = "o_state_fips"), suffix = c("_o", "_d")) %>%
  rename(o_state_fips = GEOID) %>%
  #join dest county
  right_join(county, ., by = c("GEOID" = "d_state_county_fips")) %>%
  rename(d_state_county_fips = GEOID) %>%
  #join origin county
  right_join(county, ., by = c("GEOID" = "o_state_county_fips"), suffix = c("_o", "_d")) %>%
  rename(o_state_county_fips = GEOID) %>%
  #calculate trip miles
  mutate(wtt_tripmiles = wttrdfin*distance, wmm_tripmiles = wwm_wttrdfin*distance) %>%
  #remove invalid origin and dest state codes
  filter(!is.na(state_o) & !is.na(state_d)) %>%
  #id counties based on region
  mutate(region_o = ifelse(county_o %in% compact, "GreaterWash",
                           ifelse(state_o == "Maryalnd", "RestofMd",
                                  ifelse(state_o == "Virginia", "RestofVa",
                                         "OutsideRegion"))),
         region_d = ifelse(county_d %in% compact, "GreaterWash",
                           ifelse(state_d == "Maryalnd", "RestofMd",
                                  ifelse(state_d == "Virginia", "RestofVa",
                                         "OutsideRegion"))))


#save to separate file
st_write(trip, file.path(Work_path, "alltrips.xlsx"), delete_dsn = TRUE)

#save to master file
#(this will break pivot tables in the excel file)
st_write(trip, file.path(Work_path, "RTS-VirginiaTrips.xlsx"),
         layer = "alltrips", delete_dsn = TRUE)


#Northern Virginia Trips--
novatrip <- trip %>% filter(state_o == "Virginia" | state_d == "Virginia")

st_write(novatrip, file.path(Work_path, "RTS-VirginiaTrips.xlsx"),
         layer = "nova", delete_layer = TRUE)

#Chord Diagram setup (full DMV region)
trip %>%
  mutate(chord_o = ifelse(state_o == "Maryland", "Maryland",
                          ifelse(county_o %in% compact, county_o,
                                 ifelse(state_o == "Virginia", "RestofVa",
                                        state_o))),
         chord_d = ifelse(state_d == "Maryland", "Maryland",
                          ifelse(county_d %in% compact, county_d,
                                 ifelse(state_d == "Virginia", "RestofVa",
                                        state_d)))) %>%
  filter(state_o %in% dmv & state_d %in% dmv) %>%
  group_by(chord_o, chord_d, ModeShort) %>%
  summarize(sum_tripmiles = sum(wtt_tripmiles)) %>%
  pivot_wider(names_from = ModeShort, values_from = sum_tripmiles) %>%
  ungroup() %>%
  mutate(TripTotal = rowSums(across(where(is.numeric)), na.rm = TRUE),
         PublicTransitTotal = rowSums(across(c(Bus, Rail, `Commuter Rail`)), na.rm = TRUE)) %>%
  st_write(., file.path(Work_path, "nova_chord.xlsx"))

#chords by state only
trip %>% filter(state_o %in% dmv & state_d %in% dmv) %>%
  group_by(state_o, state_d, ModeShort) %>%
  summarize(sum_tripmiles = sum(wtt_tripmiles)) %>%
  pivot_wider(names_from = ModeShort, values_from = sum_tripmiles) %>%
  ungroup() %>%
  mutate(TripTotal = rowSums(across(where(is.numeric)), na.rm = TRUE),
         PublicTransitTotal = rowSums(across(c(Bus, Rail, `Commuter Rail`)), na.rm = TRUE))


#Virginia Trips -- Chord Diagram setup (start/end in Virginia )
#using novatrip
novatrip %>%
  mutate(chord_o = ifelse(state_o == "Maryland", "Maryland",
                          ifelse(county_o %in% compact, county_o,
                                 ifelse(state_o == "Virginia", "RestofVa",
                                        state_o))),
        chord_d = ifelse(state_d == "Maryland", "Maryland",
                                 ifelse(county_d %in% compact, county_d,
                                        ifelse(state_d == "Virginia", "RestofVa",
                                               state_d)))) %>%
  filter(state_o %in% dmv & state_d %in% dmv) %>%
  group_by(chord_o, chord_d, ModeShort) %>%
  summarize(sum_tripmiles = sum(wtt_tripmiles)) %>%
  pivot_wider(names_from = ModeShort, values_from = sum_tripmiles) %>%
  ungroup() %>%
  mutate(TripTotal = rowSums(across(where(is.numeric)), na.rm = TRUE),
         PublicTransitTotal = rowSums(across(c(Bus, Rail, `Commuter Rail`)), na.rm = TRUE)) %>%
  st_write(., file.path(Work_path, "nova_chord.xlsx"), delete_dsn = TRUE)


#write to master file
st_write(nova_chord, file.path(Work_path, "RTS-VirginiaTrips.xlsx"), layer = "chord", delete_layer = TRUE)



# Speed for Env Benefit Analysis #############
tripspeed <- tripfile %>%
  select(tripid, o_fips, d_fips,
         o_state_fips, o_state_county_fips,
         d_state_fips, d_state_county_fips,
         oridest, mpo_travel_mode, distance, reported_travel_time,
         distance, wttrdfin, wwm_wttrdfin) %>%
  #filter travel mode NAs
  filter(mpo_travel_mode != -9) %>%
  #join with mode ids
  full_join(., mode_df, by = c("mpo_travel_mode" = "ModeID")) %>%
  #join dest state
  right_join(states, ., by = c("GEOID" = "d_state_fips")) %>%
  rename(d_state_fips = GEOID) %>%
  #join origin state
  right_join(states, ., by = c("GEOID" = "o_state_fips"), suffix = c("_o", "_d")) %>%
  rename(o_state_fips = GEOID) %>%
  #join dest county
  right_join(county, ., by = c("GEOID" = "d_state_county_fips")) %>%
  rename(d_state_county_fips = GEOID) %>%
  #join origin county
  right_join(county, ., by = c("GEOID" = "o_state_county_fips"), suffix = c("_o", "_d")) %>%
  rename(o_state_county_fips = GEOID) %>%
  #calculate trip miles
  mutate(wtt_tripmiles = wttrdfin*distance, wmm_tripmiles = wwm_wttrdfin*distance) %>%
  #remove invalid origin and dest state codes
  filter(!is.na(state_o) & !is.na(state_d)) %>%
  #label based on greater washington region
  mutate(id_o = ifelse(county_o %in% compact, county_o,
                       ifelse(state_o == "Virginia", "RestofVa",
                              ifelse(state_o == "Maryland", "RestofMd", state_o))),
         id_d = ifelse(county_d %in% compact, county_d,
                       ifelse(state_d == "Virginia", "RestofVa",
                              ifelse(state_d == "Maryland", "RestofMd", state_d))),
         speed = distance/(reported_travel_time/60)) %>%
  #only travel within the DMV
  filter(state_o %in% dmv,
         state_d %in% dmv)

#aggregate speed
tripspeed <- tripspeed %>%
  right_join(dmv_tracts %>% select(GEOID, tract), . , by = c("GEOID" = "d_fips")) %>%
  rename(d_fips = GEOID) %>%
  right_join(dmv_tracts %>% select(GEOID, tract), ., by = c("GEOID" = "o_fips"), suffix = c("_o", "_d")) %>%
  rename(o_fips = GEOID) %>%
  group_by(oridest, tract_o, county_o, state_o,  tract_d, county_d, state_d, ModeShort) %>%
  summarize(mean_speed = mean(speed),
            sum_miles = sum(distance))

st_write(tripspeed,
         file.path("Z:/NVTC General/Projects and Programs/Transit Resource Center (TRC)/Data Projects/23-01 Environmental Benefits of Transit",
                   "Analysis/RTS-Trips-Speeds.xlsx"))


#unmatched fips
rbind(tripspeed %>% distinct(county_o, state_o, o_fips) %>% anti_join(., dmv_tracts, by = c("o_fips" = "GEOID")) %>%
        rename(GEOID = o_fips, county = county_o, state = state_o),
      tripspeed %>% distinct(county_d, state_d, d_fips) %>% anti_join(., dmv_tracts, by = c("d_fips" = "GEOID")) %>%
        rename(GEOID = d_fips, county = county_d, state = state_d)) %>% distinct()

#all fips
rbind(tripspeed %>% distinct(o_fips) %>% rename(fips = o_fips),
      tripspeed %>% distinct(d_fips) %>% rename(fips = d_fips)) %>% distinct()



##spatial----------
dmv_tracts_center <- get_acs(geography = "tract",
                             year = 2021,
                             variables = c(TotalPopulation = "B01003_001"),
                             state = c("VA", "MD", "DC"),
                             geometry = TRUE,
                             output = "wide") %>%
  st_transform(crs = 4326) %>%
  select(GEOID, NAME) %>% separate(NAME, into = c("tract", "county", "state"), ", ") %>%
  st_centroid()

st_write(dmv_tracts_center, file.path(Work_path, "rtsdata/sf/dmv_tracts_center.shp"))

dmv_coords <- dmv_tracts_center %>% mutate(coord = paste(st_coordinates(geometry)[,1], st_coordinates(geometry)[,2], sep = ", "))


inner_join(trip, dmv_coords %>% select(GEOID, coord) %>% st_drop_geometry(), by = c("GEOID_o" = "GEOID")) %>%
  inner_join(., dmv_coords %>% select(GEOID, coord) %>% st_drop_geometry(), by = c("GEOID_d" = "GEOID"), suffix = c("_o", "_d")) %>%
  pivot_longer(cols = 20:21, names_to = "origindest", values_to = "coord") %>%
  separate(col = coord, into = c("coord_y", "coord_x"), sep = ", ") %>%
  st_as_sf(coords = c("coord_y", "coord_x"), crs = 4326) %>%
  st_write(file.path(Work_path, "rtsdata/sf/od_tracts.shp"), delete_dsn = TRUE)


trip %>% group_by(oridest, GEOID_o, GEOID_d, ModeShort) %>% summarize(sum_wtt = sum(wttrdfin), sum_wwm = sum(wwm_wttrdfin)) %>%
  inner_join(. ,dmv_coords %>% select(GEOID, coord) %>% st_drop_geometry(), by = c("GEOID_o" = "GEOID")) %>%
  inner_join(., dmv_coords %>% select(GEOID, coord) %>% st_drop_geometry(), by = c("GEOID_d" = "GEOID"), suffix = c("_o", "_d")) %>%
  pivot_longer(cols = 7:8, names_to = "origindest", values_to = "coord") %>%
  separate(col = coord, into = c("coord_y", "coord_x"), sep = ", ") %>%
  st_as_sf(coords = c("coord_y", "coord_x"), crs = 4326) %>%
  st_write(file.path(Work_path, "rtsdata/sf/od_tracts2.shp"))


origin_tracts <- inner_join(dmv_tracts_center, trip, by = c("GEOID" = "GEOID_o"))

st_write(origin_tracts, file.path(Work_path, "rtsdata/sf/origin_tracts.shp"))

dest_tracts <- inner_join(dmv_tracts_center, trip, by = c("GEOID" = "GEOID_d"))

st_write(dest_tracts, file.path(Work_path, "rtsdata/sf/dest_tracts.shp"))


#2. Census Commuter ############

#https://censusreporter.org/tables/B08301/
getworkcommute <- function(year, acs = "acs1") {
  get_acs(geography = "county",
             year = year,
             survey = acs,
             variable = c(Total = "B08301_001",
                          CarTruckVan = "B08301_002",
                          DroveAlone = "B08301_003",
                          Carpool = "B08301_004",
                          PublicTransit = "B08301_010",
                          Bus = "B08301_011",
                          Subway ="B08301_012",
                          CommuterRail = "B08301_013",
                          LightRail = "B08301_014",
                          Ferryboat = "B08301_015",
                          Taxicab = "B08301_016",
                          Motorcycle = "B08301_017",
                          Bike = "B08301_018",
                          Walk = "B08301_019",
                          Other = "B08301_020",
                          WorkFromHome = "B08301_021"),
             county = c("Arlington",
                        "Fairfax County",
                        "Loudoun",
                        "Alexandria City",
                        "Prince William County"),
             state = "Virginia"
             ) %>% select(-moe) %>% mutate(acs_year = year)
}

workcommute_20112022 <- rbind(
      getworkcommute(2011),
      getworkcommute(2012),
      getworkcommute(2013),
      getworkcommute(2014),
      getworkcommute(2015),
      getworkcommute(2016),
      getworkcommute(2017),
      getworkcommute(2018),
      getworkcommute(2019),
      getworkcommute(2021),
      getworkcommute(2022)) %>%
  pivot_wider(names_from = "acs_year", values_from = "estimate")

st_write(workcommute_20112022,
         file.path(Work_path, "CensusCommuteMode2011_2022.xlsx"), layer = "raw",
         delete_layer = TRUE)

#run census acs 1
commutedf <- data.frame()
for (i in 2007:2022) {
     c <- try(rbind(getworkcommute(i)), silent = TRUE)
     commutedf <- rbind(commutedf, c)
}

commutedf <- commutedf %>% pivot_wider(names_from = "acs_year", values_from = "estimate")
write.xlsx(commutedf, file = file.path(Work_path, "CensusCommute.xlsx"), sheetName = "acs1", append = TRUE)

#acs 5
getworkcommute5 <- function(year, acs = "acs5") {
  get_acs(geography = "county",
          year = year,
          survey = acs,
          variable = c(Total = "B08301_001",
                       CarTruckVan = "B08301_002",
                       DroveAlone = "B08301_003",
                       Carpool = "B08301_004",
                       PublicTransit = "B08301_010",
                       Bus = "B08301_011",
                       Subway ="B08301_012",
                       CommuterRail = "B08301_013",
                       LightRail = "B08301_014",
                       Ferryboat = "B08301_015",
                       Taxicab = "B08301_016",
                       Motorcycle = "B08301_017",
                       Bike = "B08301_018",
                       Walk = "B08301_019",
                       Other = "B08301_020",
                       WorkFromHome = "B08301_021"),
          county = c("Arlington",
                     "Fairfax County",
                     "Falls Church city",
                     "Fairfax city",
                     "Manassas Park city",
                     "Manassas city",
                     "Loudoun",
                     "Alexandria City",
                     "Prince William County"),
          state = "Virginia"
  ) %>% select(-moe) %>% mutate(acs_year = year)
}


commutedf5 <- data.frame()
for (i in 2009:2022) {
  c <- try(rbind(getworkcommute5(i)), silent = TRUE)
  commutedf5 <- rbind(commutedf5, c)
}
commutedf5 <- commutedf5 %>% pivot_wider(names_from = "acs_year", values_from = "estimate")
write.xlsx(commutedf5, file = file.path(Work_path, "CensusCommute.xlsx"), sheetName = "acs5", append = TRUE)
