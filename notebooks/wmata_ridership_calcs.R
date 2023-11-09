#setup
library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
library(openxlsx)

metroridersVA <- function(month, year, file) {
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
                  "WIEHLE-RESTON EAST",
                  #silver line phase 2 stations
                  "ASHBURN",
                  "DULLES AIRPORT",
                  "INNOVATION CENTER",
                  "LOUDOUN GATEWAY",
                  "RESTON TOWN CENTER",
                  "HERNDON MONROE",
                  #potomac yard
                  "POTOMAC YARD")

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
Data_path <- file.path ("Z:",
                        "NVTC General", "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data", "WMATA Ridership data (for OLGA)")

file <-file.path(Data_path, "MetroRail/FY2024-WMATA-Ridership-Calcs.xlsx")
#create data table for VA UPT
#for new month, add line in rbind
rail_riders <- rbind(
  metroridersVA('Jul', 2023, file),
  metroridersVA('Aug', 2023, file),
  metroridersVA('Sep', 2023, file)
)




###write to excel
wb <- loadWorkbook(file.path(Data_path, "WMATA-Ridership-Data.xlsx"))
writeData(wb, sheet = "MetroRailFY24", rail_riders)
saveWorkbook(wb, file.path(Data_path,"WMATA-Ridership-Data.xlsx"), overwrite = T)


#BLUE and YELLOW VA RIDERS-------
#June through September 2022 and 2023
Data_path2 <- file.path ("Z:",
                        "NVTC General", "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data", "WMATA Ridership data (for OLGA)", "MetroRail", "VA")

BlueYellowmetroridersVA <- function(month, year, file) {
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
                  #"BALLSTON",
                  "BRADDOCK ROAD",
                  #"CLARENDON",
                  #"COURT HOUSE",
                  "CRYSTAL CITY",
                  #"DUNN LORING",
                  #"EAST FALLS CHURCH",
                  "EISENHOWER AVENUE" ,
                  "FRANCONIA-SPRINGFLD",
                  #"GREENSBORO",
                  "HUNTINGTON - NORTH",
                  "HUNTINGTON - SOUTH",
                  "KING STREET",
                  "KING STREET NORTH",
                  #"McLEAN",
                  "REGAN AIRPORT S",
                  "REGAN AIRPORT N",
                  "PENTAGON",
                  "PENTAGON CITY",
                  "ROSSLYN",
                  "ROSSLYN  EAST",
                  #"SPRING HILL",
                  #"TYSONS CORNER",
                  "VAN DORN STREET" ,
                  #"VIENNA",
                  #"VIRGINIA SQUARE-GMU",
                  #"WEST FALLS CHURCH",
                  #"WIEHLE-RESTON EAST",
                  #silver line phase 2 stations
                  #"ASHBURN",
                  #"DULLES AIRPORT",
                  #"INNOVATION CENTER",
                  #"LOUDOUN GATEWAY",
                  #"RESTON TOWN CENTER",
                  #"HERNDON MONROE",
                  #potomac yard
                  "POTOMAC YARD")

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
    "Total Blue Yellow Va station Entries" = TotalVaEntries,
    "Total Blue Yellow VA UPT" = TotalVAUnlinked)
  return(t_data)
}


fy22file <- file.path(Data_path2,
                      "FY22/FY22 Rail Ridership Jurisdictional Report_VA_June.xlsx")
fy23file <- file.path(Data_path2,
                      "FY23/FY23 Rail Ridership Jurisdictional Report_VA_June.xlsx")
fy24file <- file.path(Data_path2,
                      "FY24/FY24 Rail Ridership Jurisdictional Report_VA_August.xlsx")

BlueYellow <- rbind(BlueYellowmetroridersVA('Jun', 2022, fy22file),
                    BlueYellowmetroridersVA('Jul', 2022, fy23file),
                    BlueYellowmetroridersVA('Aug', 2022, fy23file),
                    BlueYellowmetroridersVA('Jun', 2023, fy23file),
                    BlueYellowmetroridersVA('Jul', 2023, fy24file),
                    BlueYellowmetroridersVA('Aug', 2023, fy24file))

st_write(BlueYellow, "Z:/Sophie/Transit Resource Center/BlueYellowJunAug2022-2023.xlsx")
