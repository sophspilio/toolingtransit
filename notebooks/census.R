#CensusTracts need to include PRTC jursidictions
NOVA_Plus <- c("Arlington",
               "Fairfax County",
               "Fairfax city",
               "Loudoun",
               "Alexandria City",
               "Falls Church City",
               "Prince William County",
               "Stafford County",
               "Spotsylvania County",
               "Fredericksburg City",
               "Manassas City",
               "Manassas Park City")

NOVA<- c("Arlington",
               "Fairfax County",
               "Fairfax city",
               "Loudoun",
               "Alexandria City",
               "Falls Church City")
tracts <- get_acs(
  geography = "tract",
  year = 2020,
  variables = c(TotalPopulation = "B01003_001"),
  state = "VA",
  county = NOVA,
  geometry = TRUE,
  output = "wide"
) %>% select(GEOID, NAME)
st_write(tracts, "data/Novatracts.geojson")
