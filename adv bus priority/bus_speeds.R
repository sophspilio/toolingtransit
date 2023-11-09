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


#load all
load(file.path(Project_path, "data/busspeeds.RData"))

#zip files
ARTzip <- "2023-02_Arlington.zip"
CUEzip <- "2023-02_CUE.zip"
DASHzip <- "2023-02_DASH.zip"
FFXzip <- "2023-02_Fairfax_Connector.zip"
LCTzip <- "2023-02_Loudoun.zip"
PRTCzip <- "2023-02_OmniRide_PRTC.zip"
VREzip <- "2023-02_VRE.zip"
Metrobuszip <- "2023-02_Metrobus.zip"
Metrorailzip <- "2023-02_Metrorail.zip"



#load individual files
Novasegments <- readRDS(file.path(Project_path, "Data/Novasegments.RDS"))
NovaRoutes <- readRDS(file.path(Project_path, "Data/NovaRoutes.RDS"))
NovaSpeed <- readRDS(file.path(Project_path, "Data/NovaSpeed.RDS"))


#save all important files to one rdata file
save(Novasegments, NovaRoutes, NovaTime, NovaSpeed, RouteSpeed,
     VaMetrobusRoutes, notVA,
     file = file.path(Project_path, "Data/busspeeds.RData")
     )

## Stops -----------------------------------------------------------

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


## Cluster Stops -----------------------------------------------------------

#import clustered stops back from ArcGIS
#steps in arc:
#using density based clustering with 20ft buffer, min 2 features per cluster
#join field on clustered stops and oringal stops

stopclusters <- st_read(file.path(Project_path, "Analysis/2023-02-24_NovaStopsClustering.xls")) %>%
  as_tibble() %>%
  mutate(stop_id = paste0(Agency, stop_id),
         novastop_id = ifelse(CLUSTER_ID > -1, paste0("NOVA", CLUSTER_ID), stop_id)) %>%
  select(Agency, route_id, stop_id, novastop_id)


## GTFS Segments -----------------------------------------------------------
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

notVA <- stops(Metrobuszip, "MB") %>%
  filter(!route_id %in% VaMetrobusRoutes$route_id) %>% distinct(route_id)

#remove DC and MD metrobus routes and stops
Novasegments <- Novasegments %>%
  mutate(VA = ifelse(Agency == "MB" & route_id %in% VaMetrobusRoutes$route_id, "VA",
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
  mutate(novaseg_id = paste(novastop_id1, novastop_id2, sep = "-"), .keep = "unused") %>%
  filter(!grepl("NA", novaseg_id))

saveRDS(Novasegments, file.path(Project_path, "Data/Novasegments.RDS"))
st_write(Novasegments, file.path(Project_path, "Spatial Data/newIDNovasegments.shp"), delete_dsn = TRUE)

## Time Between Stops ------------------------------------------------------

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
  #units(GTFStime$time) <- "hours"
  return(GTFStime)
}



NovaTime <- rbind(time(PRTCzip, "PRTC"),
                  time(ARTzip, "ART"),
                  # time(CUEzip, "CUE"), remove CUE because stop_times are bad
                  time(DASHzip, "DASH"),
                  time(FFXzip, "FFX"),
                  time(LCTzip, "LCT"),
                  time(Metrobuszip, "MB") %>%
                    filter(!route_id %in% notVA$route_id)) %>%
  mutate(time = as.numeric(time),
                    time_hr = time/3600)



## Segment Speed -----------------------------------------------------------

#determine speed using seg dist and time
#and clean up outliers using thresholds and 1.5IQR
NovaSpeed <- inner_join(Novasegments, NovaTime, by = c("segment_id", "route_id", "Agency")) %>%
  #find speed, drop units
  mutate(dist_mi = as.numeric(distance)) %>%
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
  arrival_time <= parse_hms("01:00:00") ~ "01:00",
  arrival_time <= parse_hms("02:00:00") ~ "02:00",
  arrival_time <= parse_hms("03:00:00") ~ "03:00",
  arrival_time <= parse_hms("04:00:00") ~ "04:00",
  arrival_time <= parse_hms("05:00:00") ~ "05:00",
  arrival_time <= parse_hms("06:00:00") ~ "06:00",
  arrival_time <= parse_hms("07:00:00") ~ "07:00",
  arrival_time <= parse_hms("08:00:00") ~ "08:00",
  arrival_time <= parse_hms("09:00:00") ~ "09:00",
  arrival_time <= parse_hms("10:00:00") ~ "10:00",
  arrival_time <= parse_hms("11:00:00") ~ "11:00",
  arrival_time <= parse_hms("12:00:00") ~ "12:00",
  arrival_time <= parse_hms("13:00:00") ~ "13:00",
  arrival_time <= parse_hms("14:00:00") ~ "14:00",
  arrival_time <= parse_hms("15:00:00") ~ "15:00",
  arrival_time <= parse_hms("16:00:00") ~ "16:00",
  arrival_time <= parse_hms("17:00:00") ~ "17:00",
  arrival_time <= parse_hms("18:00:00") ~ "18:00",
  arrival_time <= parse_hms("19:00:00") ~ "19:00",
  arrival_time <= parse_hms("20:00:00") ~ "20:00",
  arrival_time <= parse_hms("21:00:00") ~ "21:00",
  arrival_time <= parse_hms("22:00:00") ~ "22:00",
  arrival_time <= parse_hms("23:00:00") ~ "23:00",
  arrival_time > parse_hms("23:00:00") ~ "24:00",
                      TRUE ~ NA_character_)) %>%
  filter(!is.na(timeperiod))

#save file --- use readRDS
saveRDS(NovaSpeed, file.path(Project_path, "NovaSpeed.RDS"))


# Route Speed  -----------------------------------------------------------------

routes <- function(GTFSzip, agency){
  GTFS <- read_gtfs(file.path(GTFS_path, GTFSzip)) %>% gtfs_as_sf()

  tripdist <- inner_join(GTFS$shapes, GTFS$trips) %>% mutate(dist = st_length(geometry))

  routes <- inner_join(tripdist %>% select(route_id, trip_id, shape_id, direction_id, dist),
                       GTFS$routes %>% select(route_id, route_short_name, route_long_name)) %>%
    group_by(route_id, route_short_name, route_long_name, direction_id) %>%
    summarize(meandist = mean(dist)) %>%
    group_by(route_id, route_short_name, route_long_name) %>%
    summarize(dist1way = mean(meandist),
              dist2way = sum(meandist),
              directions = n()) %>%
    mutate(Agency = agency)
  routes$dist1way <- set_units(routes$dist1way, mi)
  routes$dist2way <- set_units(routes$dist2way, mi)
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

saveRDS(NovaRoutes, file.path(Project_path, "NovaRoutes.RDS"))

st_write(NovaRoutes, file.path(Project_path, "Spatial Data/NovaRoutes.shp"), delete_dsn = TRUE)

##Route Speed
#distance and time for each trip

rtspeed <- function(GTFSzip, agency){
    GTFS <- gtfstools::read_gtfs(file.path(GTFS_path, GTFSzip))
    routespeed <- inner_join(GTFS$trips, get_trip_duration(GTFS, unit = "h")) %>%
      inner_join(., get_trip_length(GTFS, unit = "m") %>% filter(origin_file == "shapes")) %>%
      as_tibble()
    routespeed$length <- routespeed$length %>% set_units("m") %>% set_units("mi")
    routespeed$duration <- routespeed$duration %>% set_units("hr")
    routespeed <- routespeed %>% mutate(tripspeed = length/duration,
                                        Agency = agency) %>%
      group_by(Agency, route_id) %>%
      summarize(avgspeed = mean(tripspeed))
    return(routespeed)
  }

RouteSpeed <- rbind(rtspeed(ARTzip, "ART"),
                    rtspeed(DASHzip, "DASH"),
                    rtspeed(FFXzip, "FFX"),
                    rtspeed(PRTCzip, "PRTC"),
                    rtspeed(LCTzip, "LCT"),
                    rtspeed(CUEzip, "CUE"),
                    rtspeed(Metrobuszip, "MB") %>%
                      filter(route_id %in% VaMetrobusRoutes$route_id))
RouteSpeed <- inner_join(RouteSpeed, NovaRoutes)
st_write(RouteSpeed, file.path(Project_path, "Spatial Data/RouteSpeed.shp"), delete_dsn = TRUE)

#fastest avg route speed by agency
RouteSpeed %>% filter(avgspeed < set_units(71, mi/h)) %>%
  group_by(Agency) %>% mutate(max = max(avgspeed)) %>% filter(max == avgspeed) %>% st_sf() %>%
  st_write(., file.path(Project_path, "Spatial Data/FastAgencyRoutes.shp"))



#slowest avg route speed by agency
RouteSpeed %>% group_by(Agency) %>% mutate(min = min(avgspeed)) %>% filter(min == avgspeed) %>% st_sf() %>%
  st_write(., file.path(Project_path, "Spatial Data/SlowAgencyRoutes.shp"))

#slowest overall
RouteSpeed %>% arrange(avgspeed) %>% head(10) %>% st_sf() %>%
  st_write(., file.path(Project_path, "Spatial Data/SlowestRoutes.shp"), delete_dsn = TRUE)

#fastest segments
NovaSpeed %>% select(Agency, segment_id, speed_mph, geometry) %>%
  group_by(Agency) %>% mutate(max = max(speed_mph)) %>% filter(max == speed_mph) %>%
  distinct() %>% st_sf() %>%
  inner_join(.,
             NovaRoutes %>% st_drop_geometry() %>% select(route_id, route_short_name, route_long_name)) %>%
  st_write(., file.path(Project_path, "Spatial Data/FastestSegments.shp"))




# Part 1: Descriptive Stats -------------------------------------------------------

# Peak Changes ------------------------------------------------------------
#change in off peak vs peak speeds

#off peak 10-11am, peak 7-8am
PeakChangesAM <- NovaSpeed %>% filter(days != "Sa" | days != "Su") %>%
  filter(timeperiod == "11:00" | timeperiod == "08:00") %>%
  mutate(timeperiod = ifelse(timeperiod == "08:00", "peak", "offpeak")) %>%
  select(Agency, route_id, trip_id, segment_id, novaseg_id, timeperiod, speed_mph) %>%
  group_by(Agency, route_id, segment_id, timeperiod) %>%
  summarize(med_speed = median(speed_mph)) %>%
  pivot_wider(names_from = timeperiod, values_from = med_speed) %>%
  filter(!is.na(peak) & !is.na(offpeak)) %>%
  mutate(deltaspeed = (peak-offpeak)/abs(peak))


sharedsegmentsAM <- inner_join(NovaSpeed %>% filter(timeperiod == "08:00") %>%
                                 ungroup() %>% distinct(segment_id),
                               NovaSpeed %>% filter(timeperiod == "11:00") %>%
                                 ungroup() %>% distinct(segment_id))
#save sf file
st_sf(Novasegments) %>% select(segment_id, geometry) %>%
  filter(segment_id %in% sharedsegmentsAM$segment_id) %>% distinct() %>%
  inner_join(., PeakChangesAM, by = "segment_id") %>%
  inner_join(., NovaRoutes %>% st_drop_geometry() %>% select(route_id, route_short_name)) %>%
  st_write(., file.path(Project_path, "Spatial Data/PeakChangesAM.shp"), delete_dsn = TRUE)

#off peak 10-11am, peak 5-6pm
PeakChangesPM <- NovaSpeed %>% filter(days != "Sa" | days != "Su") %>%
  filter(timeperiod == "11:00" | timeperiod == "18:00") %>%
  mutate(timeperiod = ifelse(timeperiod == "18:00", "peak", "offpeak")) %>%
  select(Agency, route_id, trip_id, segment_id, novaseg_id, timeperiod, speed_mph) %>%
  group_by(Agency, route_id, segment_id, timeperiod) %>%
  summarize(med_speed = median(speed_mph)) %>%
  pivot_wider(names_from = timeperiod, values_from = med_speed) %>%
  filter(!is.na(peak) & !is.na(offpeak)) %>%
  mutate(deltaspeed = (peak-offpeak)/abs(peak))

sharedsegmentsPM <- inner_join(NovaSpeed %>% filter(timeperiod == "18:00") %>%
                                 ungroup() %>% distinct(segment_id),
                               NovaSpeed %>% filter(timeperiod == "11:00") %>%
                                 ungroup() %>% distinct(segment_id))

#save sf file
st_sf(Novasegments) %>% select(segment_id, geometry) %>%
  filter(segment_id %in% sharedsegmentsPM$segment_id) %>% distinct() %>%
  inner_join(., PeakChangesPM, by = "segment_id") %>%
  inner_join(., NovaRoutes %>% st_drop_geometry() %>% select(route_id, route_short_name)) %>%
  st_write(., file.path(Project_path, "Spatial Data/PeakChangesPM.shp"), delete_dsn = TRUE)


## Speed by Time of Day ----------------------------------------------------

#aggregate speeds by time of day
SpeedbyTime <- NovaSpeed %>% filter(days != "Sa" | days != "Su") %>%
  mutate(timeperiod =
           ifelse(arrival_time >= parse_hms("00:00:00") & arrival_time < parse_hms("06:00:00"), "0-6",
                  ifelse(arrival_time >= parse_hms("06:00:00") & arrival_time < parse_hms("09:00:00"), "6-9",
                         ifelse(arrival_time >= parse_hms("09:00:00") & arrival_time < parse_hms("15:00:00"), "9-15",
                                ifelse(arrival_time >= parse_hms("15:00:00") & arrival_time < parse_hms("19:00:00"), "15-19",
                                       ifelse(arrival_time >= parse_hms("19:00:00") & arrival_time < parse_hms("22:00:00"), "19-22",
                                                     ifelse(arrival_time >= parse_hms("22:00:00"), "22+", NA))))))) %>%
  group_by(Agency, route_id, segment_id, novaseg_id, timeperiod) %>% summarize(avgspeed = mean(speed_mph))


#link speed to spatial segments to visualize in ArcGIS
WeekdaySpeedbyTimeSF <-inner_join(SpeedbyTime,
                           Novasegments %>% st_sf() %>% select(segment_id, geometry) %>% distinct(),
                           by = c("segment_id"))


st_write(WeekdaySpeedbyTimeSF, file.path(Project_path, "Spatial Data/WeekdaySpeedbyTime.shp"), delete_dsn = TRUE)
st_write(SpeedbyTime, file.path(Project_path, "Spatial Data/SpeedbyTime.csv"), delete_dsn = TRUE)


SpeedbyTimeSF %>% filter(timeperiod == "6-9") %>% st_sf() %>% mapview()

NovaSpeedSF <- NovaSpeed %>%
  filter(!is.na(timeperiod)) %>%
  select(segment_id, route_id,
         novaseg_id, Agency, timeperiod, trip_id, speed_mph) %>%
  group_by(Agency, route_id, segment_id, timeperiod) %>%
  summarize(avgspeed = mean(speed_mph)) %>%
  mutate(timeday = paste0("2023/02/01 ", timeperiod, ":00")) %>%
  inner_join(., Novasegments %>% select(segment_id, geometry) %>% distinct()) %>% st_sf()


WeekdayNovaSpeedSF <- NovaSpeed %>% filter(days != "Sa" | days != "Su") %>%
  filter(!is.na(timeperiod)) %>%
  select(segment_id, route_id,
         novaseg_id, Agency, timeperiod, trip_id, speed_mph) %>%
  group_by(Agency, route_id, segment_id, timeperiod) %>%
  summarize(avgspeed = mean(speed_mph)) %>%
  mutate(timeday = paste0("2023/02/01 ", timeperiod, ":00")) %>%
  inner_join(., Novasegments %>% select(segment_id, geometry) %>% distinct()) %>% st_sf()
st_write(WeekdayNovaSpeedSF, file.path(Project_path, "Spatial Data/WeekdayNovaSpeed.shp"))


# Part 2: Cost ------------------------------------------------------------
NovaSpeed <- NovaSpeed %>%
  select(-stop_id1, -stop_id2, -upper, -lower, -geometry) %>%
  mutate(AgencyLng =
           ifelse(Agency == "MB", "Metrobus",
                  ifelse(Agency == "PRTC", "OmniRide",
                         ifelse(Agency == "LCT", "Loudoun County Transit",
                                ifelse(Agency == "FFX", "Fairfax Connector",
                                       Agency)))))

#fy20 NTD transit profiles
opcost <- st_read(file.path(Project_path, "Analysis/Agency Operating Cost/AgencyOperatingCosts.xlsx")) %>%
  rename(costperVRH = Cost.VRH.2020) %>% as_tibble() %>%
  select(Agency, costperVRH)

#segment level
SegmentCost <- NovaSpeed %>% inner_join(., opcost) %>%
  #for each segment id, find min time and subtract time - mintime
  group_by(Agency, route_id, novaseg_id) %>%
  mutate(mintime = min(time_hr),
         difftime = time_hr - mintime) %>%
  #sum (time - mintime) for each segment
  group_by(Agency, novaseg_id, costperVRH) %>%
  summarize(sumdifftime = sum(difftime)) %>%
  #cost savings
  mutate(wklycost = sumdifftime*costperVRH,
         yrlycost = wklycost * 52)


##route level
RouteCost <- NovaSpeed %>% inner_join(., opcost) %>%
  #for each segment id, find min time and subtract time - mintime
  group_by(AgencyLng, Agency, route_id, novaseg_id) %>%
  mutate(mintime = min(time_hr),
         difftime = time_hr - mintime) %>%
  #sum (time - mintime) for each segment
  group_by(AgencyLng, Agency, route_id,  costperVRH) %>%
  summarize(sumdifftime = sum(difftime)) %>%
  #cost savings
  mutate(wklycost = sumdifftime*costperVRH,
         yrlycost = wklycost * 52)

#agency level
AgencyCost <- NovaSpeed %>% inner_join(., opcost) %>%
  #for each segment id, find min time and subtract time - mintime
  group_by(Agency, route_id, novaseg_id) %>%
  mutate(mintime = min(time_hr),
         difftime = time_hr - mintime) %>%
  #sum (time - mintime) for each segment
  group_by(Agency, costperVRH) %>%
  summarize(sumdifftime = sum(difftime)) %>%
  #cost savings
  mutate(wklycost = sumdifftime*costperVRH,
         yrlycost = wklycost * 52)

#save spreadsheet
st_write(SegmentCost, file.path(Project_path, "CostImpacts.xlsx"),
         layer = "SegmentCost", delete_layer = TRUE)
st_write(RouteCost, file.path(Project_path, "CostImpacts.xlsx"),
         layer = "RouteCost", delete_layer = TRUE)
st_write(AgencyCost, file.path(Project_path, "CostImpacts.xlsx"),
         layer = "AgencyCost", delete_layer = TRUE)

#Cost per Mile

#rt
inner_join(NovaRoutes, RouteCost, by = c("Agency", "route_id")) %>%
  mutate(yrcostmi = as.numeric(yrlycost/dist1way),
         wkcostmi = as.numeric(wklycost/dist1way)) %>%
  select(AgencyLng, route_id, route_short_name, route_long_name, wklycost, yrlycost, yrcostmi, wkcostmi) %>%
  `colnames<-`(c("Agency", "rt", "rt_short", "rt_long", "wklycost", "yrlcost", "ycostmi", "wcostmi", "geom")) %>%
  st_write(., file.path(Project_path, "Spatial Data/RouteCost.shp"), delete_dsn = TRUE)


#seg
inner_join(SegmentCost,
           Novasegments %>% select(Agency, route_id, novaseg_id, distance, geometry) %>% distinct(),
           by = c("Agency", "novaseg_id")) %>%
  mutate(yrcostmi = as.numeric(yrlycost/distance)) %>%
  inner_join(., NovaRoutes %>% st_drop_geometry() %>%
               select(route_id, route_short_name, route_long_name)) %>%
  st_sf() %>%
  st_write(., file.path(Project_path, "Spatial Data/SegmentCost.shp"), delete_dsn = TRUE)

#Make spatial and add route info
SegmentCostSF <-inner_join(SegmentCost,
                           Novasegments %>% select(Agency, route_id, novaseg_id, geometry) %>% distinct(),
                           by = c("Agency", "novaseg_id")) %>%
  inner_join(., NovaRoutes %>% st_drop_geometry() %>% select(route_id, route_short_name, route_long_name)) %>% st_sf()

RouteCostSF <- inner_join(NovaRoutes, RouteCost, by = c("Agency", "route_id")) %>%
  mutate(yrcostmi = as.numeric(yrlycost/dist1way),
         wkcostmi = as.numeric(wklycost/dist1way)) %>% st_sf()


st_write(SegmentCostSF, file.path(Project_path, "Spatial Data/SegmentCost.shp"),
         delete_dsn = TRUE)
st_write(RouteCostSF, file.path(Project_path, "Spatial Data/RouteCost.shp"),
         delete_dsn = TRUE)

##Populations to Benefit----
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
NOVA <- c("Arlington","Fairfax County","Fairfax city",
          "Loudoun","Alexandria City","Falls Church City",
          "Prince William","Manassas city","Manassas Park city")
#demographics by tract
CensusDataTracts <- countycensus_tract(NOVA) %>%
  st_transform(crs = 4326) %>%
  select(-GEOID)

#demographics by county
CensusCounty <- countycensus(NOVA) %>%
  st_transform(crs = 4326) %>%  summarize(across(where(is.numeric), sum)) %>% mutate(NAME = "Nova")

#find populations within 1/4 mile of route

#for each route, find population within
RouteCostBuff <- st_buffer(RouteCostSF, dist = 400) %>% st_make_valid()

RoutePop <- st_interpolate_aw(
  CensusDataTracts,
  RouteCostBuff,
  extensive = TRUE) %>% st_join(., RouteCostBuff, join = st_equals) %>%
  mutate(CostPersonYr = yrlycost/TotalPopulationE,
         CostPersonWk = wklycost/TotalPopulationE)

RouteCostPop <- RoutePop %>% select(AgencyLng, route_id,
                    CostPersonYr, CostPersonWk) %>% st_drop_geometry() %>%
  inner_join(., RouteCostSF, by = c("AgencyLng", "route_id")) %>% st_sf() %>%
  st_cast("MULTILINESTRING")

  st_write(RouteCostPop, file.path(Project_path, "Spatial Data/RouteCostPop.shp"), delete_dsn = TRUE)


st_write(RoutePop, file.path(Project_path, "Analysis/RouteCost.xlsx"), delete_dsn = TRUE)
st_write(RoutePop,
         file.path(Project_path, "Spatial Data/RoutePop.shp"), delete_dsn = TRUE)


# Part 3: Congestion ------------------------------------------------------
## TCI ----
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


#spatial TCI
segments <- Novasegments %>% st_sf() %>%
  group_by(Agency, novaseg_id) %>%
  summarize(n = n())


TCIagg <- inner_join(TCI, segments %>% select(novaseg_id, geometry)) %>%
  st_sf() %>%
  group_by(novaseg_id) %>%
  summarize(meanTCI = mean(TCI))


tciSF <- inner_join(TCI,
                    segments %>% select(-n)) %>% st_sf()


st_write(tciSF, file.path(Project_path, "Spatial Data/TCI.shp"))
st_write(TCIagg, file.path(Project_path, "Spatial Data/TCIagg.shp"), delete_dsn = TRUE)

## Weighted TCI ------
# distance weigthed TCI for agency route and op hour
dir_id <- function(GTFSzip, Agency) {
  read_gtfs(file.path(GTFS_path, GTFSzip)) %>% .$trips %>%
    select(trip_id, direction_id) %>%
    mutate(Agency = Agency) %>%
    return()
}

direction <- rbind(dir_id(ARTzip, "ART"),
                   dir_id(DASHzip, "DASH"),
                   dir_id(FFXzip, "FFX"),
                   dir_id(LCTzip, "LCT"),
                   dir_id(PRTCzip, "PRTC"),
                   dir_id(Metrobuszip, "MB"))

#with direction
wTCI_dir <- NovaSpeed %>%
  inner_join(., direction) %>%
  group_by(Agency, novaseg_id) %>%
  mutate(mintime = min(time_hr)) %>%
  group_by(Agency, route_id, trip_id) %>%
  mutate(dist_trip = sum(dist_mi)) %>%
  group_by(Agency, route_id, trip_id, direction_id, dist_trip, novaseg_id, dist_mi, timeperiod) %>%
  summarize(TCI = time_hr/mintime) %>%
  mutate(TCIsegdist = TCI*dist_mi) %>%
  group_by(Agency, route_id, trip_id, direction_id, dist_trip) %>%
  mutate(TCIsumtrip = sum(TCIsegdist),
         TCIdw = TCIsumtrip/dist_trip) %>%
  group_by(Agency, route_id, timeperiod) %>%
  summarize(TCIdwrh = sum(TCIdw),
            crh = n()) %>%
  mutate(TCIdw = TCIdwrh/crh, .keep = "unused")



wTCI <- NovaSpeed %>%
  group_by(Agency, novaseg_id) %>%
  mutate(mintime = min(time_hr)) %>%
  group_by(Agency, route_id, trip_id) %>%
  mutate(dist_trip = sum(dist_mi)) %>%
  group_by(Agency, route_id, trip_id, dist_trip, novaseg_id, dist_mi,timeperiod) %>%
  summarize(TCI = time_hr/mintime) %>%
  mutate(TCIsegdist = TCI*dist_mi) %>%
  group_by(Agency, route_id, trip_id, dist_trip) %>%
  mutate(TCIsumtrip = sum(TCIsegdist),
         TCIdw = TCIsumtrip/dist_trip) %>%
  group_by(Agency, route_id, timeperiod) %>%
  summarize(TCIdwrh = sum(TCIdw),
            crh = n()) %>%
  mutate(TCIdw = TCIdwrh/crh, .keep = "unused")


#compare segment distance sum to trip distance calculated above
NovaSpeed %>%
  mutate(trip_id = paste0(trip_id, Agency)) %>%
  inner_join(trips %>% select(trip_id, dist_trip)) %>%
  group_by(Agency, route_id, trip_id, dist_trip) %>%
  summarize(sum_segments = sum(dist_mi))

NovaSpeed %>% group_by(Agency, route_id, timeperiod) %>%
  summarize(cth = n())

wTCI %>% arrange(Agency, route_id, timeperiod) %>%
  ggplot(aes(x = timeperiod, y = TCIdw, group = route_id, fill = Agency))+
  geom_col()+
  labs(x = "Time",
       y = "Weighted TCI")


wTCI %>% arrange(Agency, route_id, timeperiod) %>% filter(Agency == "DASH") %>%
  ggplot(aes(x = timeperiod, y = TCIdw, group = route_id, col = route_id))+
  geom_line(size = 1)+
  labs(x = "Time",
       y = "Weighted TCI")

#save
st_write(TCI, file.path(Project_path, "Analysis/TransitCongestion.xlsx"),
         layer = "TCI", delete_layer = TRUE)
st_write(wTCI, file.path(Project_path, "Analysis/TransitCongestion.xlsx"),
         layer = "WeightedTCI", delete_layer = TRUE)
st_write(wTCI_dir, file.path(Project_path, "Analysis/TransitCongestion.xlsx"),
         layer = "WeightedTCI_direction_id", delete_layer = TRUE)


# INRIX data --------------------------------------------------------------

ispeeds <- st_read(file.path(Project_path, "Data/Inrix Data/Speeds.csv")) %>% as_tibble()

tmc <- st_read(file.path(Project_path, "Data/Inrix Data/TMC_Identification.csv")) %>%
  filter(state == "VA")

t1 <- tmc %>% select(tmc, start_latitude, start_longitude, end_latitude, end_longitude) %>%
  rename(StartLat = start_latitude, StartLong = start_longitude, EndLat = end_latitude, EndLong = end_longitude) %>%
pivot_longer(-tmc, names_to = c("Type", ".value"),
                     names_pattern = "(^[A-Z][a-z]+)([A-Z][a-z]+$)")

tmc_line <- t1 %>% st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  group_by(tmc) %>% summarize(n = n()) %>%
  st_cast("MULTIPOINT") %>% st_cast("LINESTRING")

st_write(tmc_line, file.path(Project_path, "Spatial Data/inrixlines.shp"))



#join tmc with inrix speeds to create spatial dataset
speed_tmc <- inner_join(tmc_line, ispeeds, by = c("tmc" = "tmc_code")) %>% as_tibble() %>%
  select(tmc, reference_speed, speed) %>% mutate(
    reference_speed = as.numeric(reference_speed),
    speed = as.numeric(speed),
    congestion = reference_speed/speed) %>%
  select(tmc, congestion)

st_write(speed_tmc, file.path(Project_path, "data/Inrix Data/Speed_TMC.shp"))




# Rasterize NovaSpeeds ---------------------------------------------------
amspeed <- st_read(file.path(Project_path, "Spatial Data/AMPeakSpeeds.shp"))

am_rast <- st_rasterize(amspeed %>% dplyr::select(avgspeed, geometry))

x <- st_make_grid(amspeed, square = FALSE)



am_interp <- st_interpolate_aw(amspeed["avgspeed"], x, extensive = FALSE)


write_stars(am_rast, file.path(Project_path, "Spatial Data/AMSpeed_square.tif"))
