#Find Jobs near Metro

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

Jobs <- st_read("data/JobsbyBG.shp")

MetroBuff <- st_read("Z:/NVTC General/Projects and Programs/GIS Database/Spatial Repo/WMATA/Metrorail_Stations.shp") %>% st_transform(crs =4326) %>%
  filter(State == "Virginia") %>% st_buffer(., dist = 800) %>%
  st_union() %>% st_make_valid()


MetroJobs <- st_interpolate_aw(
  Jobs,
  MetroBuff,
  extensive = T
)
