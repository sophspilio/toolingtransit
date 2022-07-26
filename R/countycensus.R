#' A function to pull and rework census data for transit calculations
#'
#' @description This function allows you to easily pull census data to county level
#' and creates new variables.
#' Note: You must already have a Census API key set up. Load using readRenviron("~/.Renviron")
#' @param County An accepted county or city in Northern Virginia.
#' @export
#' @examples
#' countycensus("Arlington")
#' countycensus("Fairfax city")
#' countycensus("Fairfax County")
#' Nova counties and cities:
#' "Arlington", ,"Fairfax County", "Fairfax city", "Loudoun", "Alexandria City","Falls Church City"

countycensus <- function(County) {
  #load required libraries
  require(tidyverse)
  require(censusapi)
  require(sf)
  require(units)
  #pull census acs data for 2020, select variables for county
  CensusCnty <- get_acs(
    geography = "block group",
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
    select(TotalPopulationE, HHCountE, Count0Car, TotalCommutersE, PublicTransportE, NonWhitePop, Under200PctPoverty, GEOID, area)
  #set units for the area as square miles
  CensusCnty$area <- set_units(CensusCnty$area, mi^2)
  return(CensusCnty)
}
