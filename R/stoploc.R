#' A function to create spatial files for stop locations
#'
#' @description This function allows you to create stop location from a gtfs file.
#' It creates a spatial file that indicates what agency the stops are associated with.
#' @param gtfszip The location of the gtfs zip file
#' @param agency Name of agency as identified in output data frame
#' @export
#' @examples
#' stoploc("2019-10_Arlington.zip", "ART")
#'

stoploc <- function(gtfszip, agency) {
  ### All gtfs data is located in this file path
  GTFS_path <- file.path ("Z:",
                          "NVTC General",
                          "Projects and Programs",
                          "Transit Resource Center (TRC)",
                          "Data",
                          "GTFS")

  #establish gtfs data
  CountyPath <- file.path(GTFS_path, gtfszip)
  CountyGTFS <- read_gtfs(CountyPath)
  #create spatial file for stops and select only necessary variables
  CountyStopLoc <- stops_as_sf(CountyGTFS$stops) %>%
    dplyr::select(stop_id) %>%
    mutate("Agency" = agency)
  return(CountyStopLoc)
}
