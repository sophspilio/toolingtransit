library(tidyverse)
library(sf)
library(tidytransit)
library(mapview)
library(units)
library(hms)
library(ggplot2)


# Part 0: Setup -----------------------------------------------------------


GTFS_path <- file.path ("Z:",
                        "NVTC General", "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data", "GTFS")
Project_path <- file.path ("Z:",
                           "NVTC General","Projects and Programs",
                           "Transit Resource Center (TRC)",
                           "Data Projects", "22-07 Bus Speed and Congestion Study")

Nova <- st_read("data/Nova.shp")


ARTzip <- "2023-02_Arlington.zip"
CUEzip <- "2023-02_CUE.zip"
DASHzip <- "2023-02_DASH.zip"
FFXzip <- "2023-02_Fairfax_Connector.zip"
LCTzip <- "2023-02_Loudoun.zip"
PRTCzip <- "2023-02_OmniRide_PRTC.zip"
VREzip <- "2023-02_VRE.zip"
Metrobuszip <- "2023-02_Metrobus.zip"
Metrorailzip <- "2023-02_Metrorail.zip"

# Stops -----------------------------------------------------------

stops <- function(gtfszip, agency) {
  require(tidyverse)
  require(tidytransit)
  require(sf)
  require(units)

  #establish gtfs data
  GTFS <- read_gtfs(file.path(GTFS_path, gtfszip))
  message(agency)
  #if coordinates are non numeric
  if(is.character(GTFS$stops$stop_lat)) {
    GTFS$stops$stop_lat <- as.numeric( GTFS$stops$stop_lat)
    GTFS$stops$stop_lon <- as.numeric(GTFS$stops$stop_lon)
    stops <- inner_join(GTFS$stop_times, GTFS$trips) %>%
      group_by(route_id, stop_id) %>% summarize(trips = n()) %>%
      inner_join(stops_as_sf(GTFS$stops)) %>% st_sf() %>% inner_join(., GTFS$routes %>% select(route_id)) %>%
      mutate(Agency = agency) %>% select(route_id, stop_id,Agency)
  } else {
    stops <- inner_join(GTFS$stop_times, GTFS$trips) %>%
      group_by(route_id, stop_id) %>% summarize(trips = n()) %>%
      inner_join(stops_as_sf(GTFS$stops)) %>% st_sf()%>% inner_join(., GTFS$routes %>% select(route_id)) %>%
      mutate(Agency = agency) %>% select(route_id, stop_id, Agency)
  }
  return(stops)
}

#all nova bus stops
NovaStops <- rbind(stops(PRTCzip, "PRTC"),
                   stops(ARTzip, "ART"),
                   stops(CUEzip, "CUE"),
                   stops(DASHzip, "DASH"),
                   stops(FFXzip, "FFX"),
                   stops(LCTzip, "LCT"),
                   stops(Metrobuszip, "MB") %>% st_intersection(., Nova) %>% select(-FID))

st_write(NovaStops, "data/NovaBusStops.shp", delete_dsn = TRUE)


# Cluster Stops -----------------------------------------------------------

#import clustered stops back from ArcGIS
#steps in arc:
#using density based clustering with 20ft buffer, min 2 features per cluster
#join field on clustered stops and oringal stops

stopclusters <- st_read(file.path(Project_path, "2023-02-24_NovaStopsClustering.xls")) %>%
  as_tibble() %>%
  mutate(stop_id = paste0(Agency, stop_id),
         novastop_id = ifelse(CLUSTER_ID > -1, paste0("NOVA", CLUSTER_ID), stop_id)) %>%
  select(Agency, route_id, stop_id, novastop_id)


# GTFS Segments -----------------------------------------------------------
#spatial segments file
Novasegments <- st_read(file.path(Project_path, "Spatial Data/Feb 23 Segments/NOVAsegments202302.shp")) %>%
  filter(Agency != "CUE") %>%
  mutate(Agency = ifelse(Agency == "Metrobus", "MB", Agency)) %>%
  mutate(stop_id1 = paste0(Agency, stop_id1),
         stop_id2 = paste0(Agency, stop_id2),
         segment_id = paste(stop_id1, stop_id2, sep = "-")) %>%
  as_tibble() %>% select(-traversals, -direction_)


#find MB routes within VA
#identify routes outside VA
MetrobusStops <- stops(Metrobuszip, "MB") %>% st_intersection(., Nova) %>% select(-FID)
VaMetrobusRoutes <- MetrobusStops %>% distinct(route_id)
VaMetrobusStops <- MetrobusStops %>% distinct(stop_id)

notVA <- stops(Metrobuszip, "MB") %>%
  filter(!route_id %in% VaMetrobusRoutes$route_id) %>% distinct(route_id)

#remove DC and MD metrobus routes and stops
Novasegments <- Novasegments %>%
  mutate(VA = ifelse(Agency == "MB" & route_id %in% VaMetrobusRoutes$route_id &
                       stop_id1 %in% VaMetrobusStops$stop_id & stop_id2 %in% VaMetrobusStops, "VA",
                     ifelse(Agency != "MB", "VA", "Nope"))) %>%
  filter(VA == "VA") %>% select(-VA)

#convert to miles
Novasegments$distance <- Novasegments$distance %>% as.numeric() %>% set_units("meters")
Novasegments$distance <- Novasegments$distance %>% set_units("miles")

#join segments with clusters
#to identify stops that are shared across
Novasegments <- full_join(Novasegments, stopclusters,
                          by = c("stop_id1" = "stop_id", "route_id" = "route_id", "Agency" ="Agency")) %>%
  rename(novastop_id1 = novastop_id) %>% filter(!is.na(segment_id)) %>%
  full_join(., stopclusters, by = c("stop_id2" = "stop_id", "route_id" = "route_id", "Agency" = "Agency")) %>%
  rename(novastop_id2 = novastop_id) %>% filter(!is.na(segment_id)) %>%
  mutate(novaseg_id = paste(novastop_id1, novastop_id2, sep = "-"), .keep = "unused")

st_write(Novasegments, file.path(Project_path, "Spatial Data/newIDNovasegments.shp"), delete_dsn = TRUE)

# Time Between Stops ------------------------------------------------------

#identify service ids that add up to one week of service
cal <- function(GTFSzip){
  GTFS <- read_gtfs(file.path(GTFS_path, GTFSzip))
  cal <- GTFS$calendar %>%
  mutate_at(c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'), as.numeric) %>%
  mutate(daysofop = monday + tuesday + wednesday + thursday + friday + saturday + sunday) %>%
  mutate(days = ifelse(daysofop == 5, "M-F", ifelse(daysofop == 4, "M-Th",
                                                    ifelse(friday == 1, "F",
                                                           ifelse(saturday == 1, "Sa",
                                                                  ifelse(sunday == 1, "Su", NA)))))) %>%
    select(service_id, daysofop, days) %>% filter(!is.na(days))
  return(cal)
}


#find time between stops using GTFS stop_times departure from previous stop - arrival to current stop
time <- function(gtfszip, agency){
  GTFS <- read_gtfs(file.path(GTFS_path, gtfszip))
  GTFStime <- GTFS$stop_times %>% select(trip_id, stop_id, arrival_time, departure_time) %>%
    inner_join(GTFS$trips %>% select(route_id, trip_id, service_id), .) %>%
    filter(service_id %in% cal(gtfszip)$service_id) %>%
    mutate(time = arrival_time - lag(departure_time),
                                         segment_id = paste(paste0(agency,lag(stop_id)),
                                                            paste0(agency, stop_id), sep = "-"),
           Agency = agency) %>%
    select(segment_id, route_id, Agency, trip_id, arrival_time, time) %>%
    na.omit() %>% filter(time > 0)
  units(GTFStime$time) <- "hours"
  return(GTFStime)
}


NovaTime <- rbind(time(PRTCzip, "PRTC"),
                  time(ARTzip, "ART"),
                  # time(CUEzip, "CUE"), remove CUE because stop_times are bad
                  time(DASHzip, "DASH"),
                  time(FFXzip, "FFX"),
                  time(LCTzip, "LCT"),
                  time(Metrobuszip, "MB") %>%
                    filter(!route_id %in% notVA$route_id))



# Segment Speed -----------------------------------------------------------

#determine speed using seg dist and time
#and clean up outliers using thresholds and 1.5IQR

NovaSpeed <- inner_join(Novasegments, NovaTime, by = c("segment_id", "route_id", "Agency")) %>%
  #find speed, drop units
  mutate(time_hr = as.numeric(time), dist_mi = as.numeric(distance)) %>%
  mutate(speed_mph = dist_mi/time_hr) %>%
  #upper and lower thresholds
  filter(speed_mph < 71 & speed_mph > 1, time_hr < 2) %>%
  select(-distance, -time) %>%
  #threshold with 1.5IQR
  group_by(route_id, segment_id) %>%
  mutate(upper = quantile(speed_mph)[4] + (1.5*IQR(speed_mph)),
         lower =  quantile(speed_mph)[2] - (1.5*IQR(speed_mph))) %>%
  filter(speed_mph >= lower & speed_mph <= upper)

NovaSpeed <- mutate(NovaSpeed, timeperiod = case_when(
                      arrival_time <= parse_hms("01:00:00") ~ "1. 12am-1am",
                      arrival_time <= parse_hms("02:00:00") ~ "2. 1-2am",
                      arrival_time <= parse_hms("03:00:00") ~ "3. 2-3am",
                      arrival_time <= parse_hms("04:00:00") ~ "4. 3-4am",
                      arrival_time <= parse_hms("05:00:00") ~ "5. 4-5am",
                      arrival_time <= parse_hms("06:00:00") ~ "6. 5-6am",
                      arrival_time <= parse_hms("07:00:00") ~ "7. 6-7am",
                      arrival_time <= parse_hms("08:00:00") ~ "8. 7-8am",
                      arrival_time <= parse_hms("09:00:00") ~ "9. 8-9am",
                      arrival_time <= parse_hms("10:00:00") ~ "10. 9-10am",
                      arrival_time <= parse_hms("11:00:00") ~ "11. 10-11am",
                      arrival_time <= parse_hms("12:00:00") ~ "12. 11am-12pm",
                      arrival_time <= parse_hms("13:00:00") ~ "13. 12-1pm",
                      arrival_time <= parse_hms("14:00:00") ~ "14. 1-2pm",
                      arrival_time <= parse_hms("15:00:00") ~ "15. 2-3pm",
                      arrival_time <= parse_hms("16:00:00") ~ "16. 3-4pm",
                      arrival_time <= parse_hms("17:00:00") ~ "17. 4-5pm",
                      arrival_time <= parse_hms("18:00:00") ~ "18. 5-6pm",
                      arrival_time <= parse_hms("19:00:00") ~ "19. 6-7pm",
                      arrival_time <= parse_hms("20:00:00") ~ "20. 7-8pm",
                      arrival_time <= parse_hms("21:00:00") ~ "21. 8-9pm",
                      arrival_time <= parse_hms("22:00:00") ~ "22. 9-10pm",
                      arrival_time <= parse_hms("23:00:00") ~ "23. 10-11pm",
                      arrival_time > parse_hms("23:00:00") ~ "24. 11pm-12am",
                      TRUE ~ NA_character_)) %>%
  filter(!is.na(timeperiod)) %>%
  separate(timeperiod, into = c("order", "timeperiod"), sep = ". ")

#save file --- use readRDS
saveRDS(NovaSpeed, file.path(Project_path, "NovaSpeed.RDS"))


# Part 1: Descriptive Stats -------------------------------------------------------
NovaSpeed <- readRDS(file.path(Project_path, "NovaSpeed.RDS"))

#general stats by route
Stats <- NovaSpeed %>%  group_by(Agency, route_id) %>% summarize(avg_speed = mean(speed_mph),
                                             sd_speed = sd(speed_mph),
                                             median_speed = median(speed_mph),
                                             min_speed = min(speed_mph),
                                             max_speed = max(speed_mph))
#range of speeds by route
NovaSpeed %>% group_by(Agency, route_id) %>% summarize(minspeed = min(speed_mph),
                                                       maxspeed = max(speed_mph)) %>%
  mutate(range = maxspeed - minspeed, .keep = "unused" ) %>% arrange(range)

#change in off peak vs peak speeds
peak <- NovaSpeed %>% filter(timeperiod == "5-6pm")
offpeak <- NovaSpeed %>% filter(timeperiod == "10-11am")

sharedsegments <- inner_join(peak %>% ungroup() %>% distinct(segment_id),
                           offpeak %>% ungroup() %>% distinct(segment_id))


PeakChanges <- NovaSpeed %>%  filter(segment_id %in% sharedsegments$segment_id) %>%
  filter(timeperiod == "5-6pm" | timeperiod == "10-11am") %>%
  mutate(timeperiod = ifelse(timeperiod == "5-6pm", "peak", "offpeak")) %>%
  select(Agency, route_id, trip_id, segment_id, novaseg_id, timeperiod, speed_mph) %>%
  group_by(Agency, route_id, segment_id, timeperiod) %>%
  summarize(med_speed = median(speed_mph)) %>%
  pivot_wider(names_from = timeperiod, values_from = med_speed) %>%
  filter(!is.na(peak) & !is.na(offpeak)) %>%
  mutate(deltaspeed = (peak-offpeak)/abs(peak))

#histo
ggplot(PeakChanges, aes(deltaspeed))+
  geom_histogram()

st_sf(Novasegments) %>% select(segment_id, geometry) %>%
  filter(segment_id %in% sharedsegments$segment_id) %>% distinct() %>%
  inner_join(., PeakChanges, by = "segment_id") %>%
  st_write(., file.path(Project_path, "Spatial Data/PeakChanges.shp"), delete_dsn = TRUE)

#viz with violin plots
NovaSpeed %>% filter(Agency == "ART") %>%
  ggplot(., aes( x = route_id, y = speed_mph))+
  geom_violin()

ggplot(NovaSpeed, aes(x=Agency, y=speed_mph)) +
  geom_violin()


ggplot(NovaSpeed, aes(x=route_id, y=speed_mph)) +
  geom_violin()+
  facet_wrap(vars(Agency))


#jitter plot
ggplot(NovaSpeed %>% filter(Agency == "ART"), aes(x = route_id, y = speed_mph)) +
  geom_boxplot(alpha = 0)+
  geom_jitter(alpha = 0.3, color = "steelblue3")+
  theme(panel.background = element_blank(), panel.grid.minor = element_blank())



# Speed by Time of Day ----------------------------------------------------

SpeedbyHour <- NovaSpeed %>%
  group_by(Agency, route_id, order, timeperiod) %>% summarize(avgspeed = mean(speed_mph)) %>%
  ungroup() %>% group_by(order, timeperiod) %>% summarize(min_avgspeed = min(avgspeed),
                                                   routecount = n()) %>%
  mutate(order = as.numeric(order)) %>%
  arrange(order)

SpeedbyHour$timeperiod <- factor(SpeedbyHour$timeperiod, levels = SpeedbyHour$order)

ggplot(SpeedbyHour)+
  geom_bar(aes(x = order, y = routecount,
               fill = min_avgspeed),
           stat = "identity") +
  scale_fill_gradientn(colours = colorspace::heat_hcl(7))+
  theme_minimal()


#aggregate speeds by time of day
SpeedbyTime <- NovaSpeed %>%
  mutate(timeperiod =
           ifelse(arrival_time >= parse_hms("00:00:00") & arrival_time < parse_hms("06:00:00"), "0-6",
                  ifelse(arrival_time >= parse_hms("06:00:00") & arrival_time < parse_hms("09:00:00"), "6-9",
                         ifelse(arrival_time >= parse_hms("09:00:00") & arrival_time < parse_hms("15:00:00"), "9-15",
                                ifelse(arrival_time >= parse_hms("15:00:00") & arrival_time < parse_hms("19:00:00"), "15-19",
                                       ifelse(arrival_time >= parse_hms("19:00:00") & arrival_time < parse_hms("22:00:00"), "19-22",
                                                     ifelse(arrival_time >= parse_hms("22:00:00"), "22+", NA))))))) %>%
  group_by(Agency, route_id, segment_id, novaseg_id, timeperiod) %>% summarize(avgspeed = mean(speed_mph))


#what time of day do the slowest speeds occur?
SpeedbyTime %>% filter(avgspeed < 5)


#link speed to spatial segments to visualize in ArcGIS
SpeedbyTimeSF <-inner_join(SpeedbyTime,
                           Novasegments %>% st_sf() %>% select(segment_id, geometry) %>% distinct(),
                           by = c("segment_id"))


st_write(SpeedbyTimeSF, file.path(Project_path, "Spatial Data/SpeedbyTime.shp"), delete_dsn = TRUE)
st_write(SpeedbyTime, file.path(Project_path, "Spatial Data/SpeedbyTime.csv"), delete_dsn = TRUE)

# Shared Segments ---------------------------------------------------------

SharedSeg <- NovaSpeed %>% ungroup() %>%
  #filter only shared stops
  separate(., novaseg_id, sep = "-",
           into = c("novaseg1", "novaseg2"), remove = FALSE) %>%
  filter(grepl("NOVA", novaseg1), grepl("NOVA", novaseg2)) %>%
  #find shared stops between agencies
  select(Agency, novaseg_id) %>% distinct() %>%
  group_by(novaseg_id) %>% summarize(n = n()) %>%
  #identify duplicates
  filter(n > 1)

#compare shared segments speeds at dif times of day
SpeedbyTime %>% filter(novaseg_id %in% SharedSeg$novaseg_id) %>% ungroup() %>%
  group_by(novaseg_id, Agency, route_id) %>%
  pivot_wider(names_from = "timeperiod", values_from = "avgspeed") %>%
  arrange(novaseg_id)

# Routes ------------------------------------------------------------------

routes <- function(GTFSzip, agency){
  GTFS <- read_gtfs(file.path(GTFS_path, GTFSzip))
  routes <- inner_join(GTFS$trips %>% select(route_id, trip_id, shape_id),
                       GTFS$routes %>% select(route_id, route_short_name, route_long_name)) %>%
    select(-trip_id) %>% distinct() %>%
    inner_join(.,shapes_as_sf(GTFS$shapes)) %>% st_sf() %>%
    group_by(route_id, route_short_name, route_long_name) %>%
    summarize(n = n()) %>%
    select(-n) %>%
    mutate(Agency = agency,
           dist = st_length(geometry))
  routes$dist <- set_units(routes$dist, mi)
  return(routes)
}

#all nova routes with short and long names
NovaRoutes <- rbind(routes(ARTzip, "ART"),
                    routes(DASHzip, "DASH"),
                    routes(FFXzip, "FFX"),
                    routes(PRTCzip, "PRTC"),
                    routes(LCTzip, "LCT"),
                    routes(CUEzip, "CUE"),
                    routes(Metrobuszip, "MB") %>%
                      filter(route_id %in% VaMetrobusRoutes$route_id))

st_write(NovaRoutes, file.path(Project_path, "Spatial Data/NovaRoutes.shp"))

#by route
RouteSpeedAgg <- SpeedbyTime %>% ungroup() %>% group_by(Agency, route_id, timeperiod) %>%
  summarize(speed = mean(avgspeed)) %>% inner_join(NovaRoutes, by = c("Agency", "route_id")) %>%
  st_sf()

RouteSpeed <- NovaSpeed %>% ungroup() %>% group_by(Agency, route_id, timeperiod) %>%
  summarize(speed = mean(speed_mph)) %>% inner_join(NovaRoutes, by = c("Agency", "route_id")) %>%
  st_sf()


st_write(RouteSpeedAgg, file.path(Project_path, "Spatial Data/RouteSpeedAgg.shp"), delete_dsn = TRUE)
st_write(RouteSpeed, file.path(Project_path, "Spatial Data/RouteSpeed.shp"), delete_dsn = TRUE)


# Part 2: Cost ------------------------------------------------------------
NovaSpeed <- readRDS(file.path(Project_path, "NovaSpeed.RDS")) %>%
  select(-stop_id1, -stop_id2, -upper, -lower, -geometry)

opcost <- st_read(file.path(Project_path, "Agency Operating Cost/AgencyOperatingCosts.xlsx")) %>%
  rename(costperVRH = Operating.Expenses.per.VRH) %>% as_tibble() %>%
  select(Agency, costperVRH)

##segment level
  #cost per VRH each agency
NovaSpeed %>% inner_join(., opcost) %>%
  #for each segment id, find min time and subtract time - mintime
  group_by(Agency, route_id, novaseg_id) %>%
  mutate(mintime = min(time_hr),
         difftime = time_hr - mintime) %>%
  #sum (time - mintime) for each segment
  group_by(Agency, route_id, novaseg_id, costperVRH) %>%
  summarize(sumdifftime = sum(difftime)) %>%
  #cost savings
  mutate(wklycost = sumdifftime*costperVRH,
         yrlycost = wklycost * 52)

##route
  group_by(Agency, route_id) %>%
  summarize(wklycost = sum(wklycost),
            yrlycost = sum(yrlycost)) %>%
  arrange(-yrlycost)



#top cost savings routes
topten <- Cost %>% arrange(-wklycost) %>% head(n = 10) %>%
  inner_join(., NovaRoutes, by = c("Agency", "route_id")) %>% st_sf()

##populations that could benefit from bus priority
#census data
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
countycensus <- function(County) {
  #load required libraries
  require(tidyverse)
  require(censusapi)
  require(tidycensus)
  require(sf)
  require(units)
  #pull census acs data for 2020, select variables for county
  CensusCnty <- get_acs(
    geography = "county",
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
    select(NAME, TotalPopulationE, HHCountE, Count0Car, TotalCommutersE, PublicTransportE, NonWhitePop, Under200PctPoverty, area)
  #set units for the area as square miles
  CensusCnty$area <- set_units(CensusCnty$area, mi^2)
  return(CensusCnty)
}

NOVA <- c("Arlington",
          "Fairfax County",
          "Fairfax city",
          "Loudoun",
          "Alexandria City",
          "Falls Church City",
          "Prince William",
          "Manassas city",
          "Manassas Park city")
#demographics by tract
CensusDataTracts <- countycensus_tract(NOVA) %>%
  st_transform(crs = 4326) %>%
  select(-GEOID)

#demographics by county
CensusCounty <- countycensus(NOVA) %>%
  st_transform(crs = 4326)

#find populations within 1/4 mile of route
routes2benefit <- st_interpolate_aw(
  CensusDataTracts,
  topten %>%
    st_buffer(dist = 400) %>% st_make_valid(),
  extensive = TRUE) %>%
  st_join( ., topten, join = st_covers)


# Part 3: Congestion ------------------------------------------------------

TCI <- NovaSpeed %>%
  group_by(Agency, novaseg_id) %>%
  #min segment time
  mutate(mintime = min(time_hr)) %>%
  group_by(Agency, novaseg_id, timeperiod, mintime, dist_mi) %>%
  #avg seg time for each hour
  summarize(avgsegtime = mean(time_hr)) %>%
  #TCI = avg segment time divided by min segment time
  mutate(TCI = avgsegtime/mintime) %>%
  rename(seg_dist = dist_mi)
TCI$seg_dist <- set_units(TCI$seg_dist, mi)

#weighted distance for route:
#(tci of segment * seg distance) + (tci of segment * distance) /distance of the route

#trip length --> shapes master trip id and dist of trip
#avg all trips for route

weighted <- inner_join(TCI, NovaRoutes, by = c("Agency", "route_id")) %>%
  rename(rt_dist = dist) %>%
  group_by(Agency, route_id, novaseg_id, rt_dist, trip_id) %>% #trip dist
  mutate(TCIdist = TCI*seg_dist) %>%
  summarize(triplevel = sum(TCIdist)) %>%
  mutate(wdist = wdist/rt_dist) #avg of trip dist for a route


