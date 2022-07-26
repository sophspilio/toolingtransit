#setup
library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
library(openxlsx)


#################################################
metroridersVA <- function(month, year, file) {
  require(dplyr)
  require(lubridate)
  require(readxl)
  require(data.table)

  entermonth <- month
  enteryear <- year
  monthnum <- match(entermonth,month.abb)

  #days of the month math
  month <- ym(paste(enteryear,entermonth, sep = "-"))#ym("2021-09")
  Date1 <- month
  Date2 <- rollforward(month)
  wkday <- sum(!weekdays(seq(Date1, Date2, "days"))%in% c("Saturday","Sunday"))
  saturdays <- sum(weekdays(seq(Date1, Date2, "days"))%in% c("Saturday"))
  sundays <- sum(weekdays(seq(Date1, Date2, "days"))%in% c("Sunday"))

  #load data
  riders <- read_excel(file, sheet = entermonth, range = "A3:V443") %>%
    #rename columns
    setNames(c("MezzID", "MezzName", "EntryExit",
               "wkdayAM", "wkdayMID", "wkdayPM", "wkdayEVN", "wkdayTOTAL", "Empty1", "Empty2",
               "satAM", "satMID", "satPM", "satEVN", "satTOTAL", "Empty3", "Empty4",
               "sunAM", "sunMID", "sunPM", "sunEVN", "sunTOTAL"))

  ##########################################################
  #filter all boardings to just va stations and only entries
  #NOTE some stations have multiple entrances

  vastations <- c("ARLINGTON CEMETERY",
                  "BALLSTON",
                  "BRADDOCK ROAD",
                  "CLARENDON",
                  "COURT HOUSE",
                  "CRYSTAL CITY",
                  "DUNN LORING",
                  "EAST FALLS CHURCH",
                  "EISENHOWER AVENUE" ,
                  "FRANCONIA-SPRINGFLD",
                  "GREENSBORO",
                  "HUNTINGTON - NORTH",
                  "HUNTINGTON - SOUTH",
                  "KING STREET",
                  "KING STREET NORTH",
                  "McLEAN",
                  "REGAN AIRPORT S",
                  "REGAN AIRPORT N",
                  "PENTAGON",
                  "PENTAGON CITY",
                  "ROSSLYN",
                  "ROSSLYN  EAST",
                  "SPRING HILL",
                  "TYSONS CORNER",
                  "VAN DORN STREET" ,
                  "VIENNA",
                  "VIRGINIA SQUARE-GMU",
                  "WEST FALLS CHURCH",
                  "WIEHLE-RESTON EAST")

  vaentries <- riders %>% filter(MezzID != "***") %>%
    filter(MezzName %in% vastations) %>%
    filter(EntryExit == "ENTRY")


  #calculate total VA station entries by current month
  VaWkdayTrips <- sum((vaentries$wkdayTOTAL)*wkday)
  VaSatTrips <- sum((vaentries$satTOTAL)*saturdays)
  VaSunTrips <- sum((vaentries$sunTOTAL)*sundays)
  TotalVaEntries <- sum(VaWkdayTrips,VaSatTrips,VaSunTrips)
  #########################################################
  # System Total Entries
  totalentries <- riders %>% filter(EntryExit == "ENTRY", MezzID != "***")

  TotalWkdayTrips <- sum(totalentries$wkdayTOTAL)*wkday
  TotalSatTrips <- sum(totalentries$satTOTAL)*saturdays
  TotalSunTrips <- sum(totalentries$sunTOTAL)*sundays
  TotalSystemEntries <- sum(TotalWkdayTrips,TotalSatTrips,TotalSunTrips)

  #############################################################
  #Estimated Total Virginia Unlinked Trips
  TotalVAENtriesExits <- TotalVaEntries*2
  MinusVA_VA <- TotalSystemEntries*0.0789
  TotalVALinked <- TotalVAENtriesExits-MinusVA_VA

  TotalVAUnlinked <- TotalVALinked*1.33


  t_data <- data.table(
    "Date" = paste(entermonth, enteryear),
    "Total Va station Entries" = TotalVaEntries,
    "Total VA UPT" = TotalVAUnlinked)
  return(t_data)
}


##################################################
file <- "Z:/NVTC General/Projects and Programs/Transit Resource Center (TRC)/Analysis/Metro Ridership Workbooks/FY 2022/MASTER NVTC Metrorail Ridership FY22.xlsx"
#create data table for VA UPT
#for new month, add line in rbind
rail_riders <- rbind(
  metroridersVA('Jul', 2021, file),
  metroridersVA('Aug', 2021, file),
  metroridersVA('Sep', 2021, file),
  metroridersVA('Oct', 2021, file),
  metroridersVA('Nov', 2021, file),
  metroridersVA('Dec', 2021, file),
  metroridersVA('Jan', 2022, file),
  metroridersVA('Feb', 2022, file),
  metroridersVA('Mar', 2022, file),
  metroridersVA('Apr', 2022, file),
  metroridersVA('May', 2022, file),
  metroridersVA('Jun', 2022, file)
)

###write to excel

wb <- loadWorkbook("Z:/NVTC General/Projects and Programs/Transit Resource Center (TRC)/Data/Ridership/MASTER metro ridership.xlsx")
writeData(wb, sheet = "MetroRail", rail_riders)
saveWorkbook(wb, "Z:/NVTC General/Projects and Programs/Transit Resource Center (TRC)/Data/Ridership/MASTER metro ridership.xlsx", overwrite = T)

