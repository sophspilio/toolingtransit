access2transit <- function(Mode, buff, Census) {

  BuffJoin <- NovaStopsRoutes_2022 %>% filter(Mode == Mode) %>%
    st_buffer(dist = buff) %>% st_union() %>% st_make_valid()

  #interpolate census data to these stops
  TransitCensus <- st_interpolate_aw(
    Census,
    BuffJoin,
    extensive = T
  ) %>% st_drop_geometry() %>% as.data.frame()
  return(TransitCensus)
}

#bus = 1/4 mile (400m)
#hr = 1/2 mile (800m)
#para = 3/4 mile (1200m)
#cr = 1 mile (1600m)

#arlington
ARlCR <- st_interpolate_aw(
  Arl,
  NovaStopsRoutes_2022 %>% filter(Mode == "CR") %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "CR")
ARlBus <- st_interpolate_aw(
  Arl,
  NovaStopsRoutes_2022 %>% filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")
ARlHR <- st_interpolate_aw(
  Arl,
  NovaStopsRoutes_2022 %>% filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "HR")
ARlpara <- st_interpolate_aw(
  Arl,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

ArlAccess <- rbind(ARlBus, ARlHR, ARlCR, ARlpara) %>% mutate(Jurisdiction = "Arlington County")
#falls church
fcCR <- st_interpolate_aw(
  fc,
  NovaStopsRoutes_2022 %>% filter(Mode == "CR") %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid(),
  extensive = T
)
fcHR <- st_interpolate_aw(
  fc,
  NovaStopsRoutes_2022 %>% filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
)
fcBus <- st_interpolate_aw(
  fc,
  NovaStopsRoutes_2022 %>% filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
)
fcpara <- st_interpolate_aw(
  fc,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
)

fcAccess <- rbind(fcHR, fcBus, fcpara) %>% mutate(Jurisdiction = "City of Falls Church")
#Alexandria
AlxCR <- st_interpolate_aw(
  Alx,
  NovaStopsRoutes_2022 %>% filter(Mode == "CR") %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "CR")
AlxBus <- st_interpolate_aw(
  Alx,
  NovaStopsRoutes_2022 %>% filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")
AlxHR <- st_interpolate_aw(
  Alx,
  NovaStopsRoutes_2022 %>% filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "HR")
Alxpara <- st_interpolate_aw(
  Alx,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

AlxAccess <- rbind(AlxBus, AlxHR, AlxCR, Alxpara) %>% mutate(Jurisdiction = "City of Alexandria")
#City of Fairfax
CityFFXCR <- st_interpolate_aw(
  CityFFX,
  NovaStopsRoutes_2022 %>% filter(Mode == "CR") %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "CR")
CityFFXBus <- st_interpolate_aw(
  CityFFX,
  NovaStopsRoutes_2022 %>% filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")
CityFFXHR <- st_interpolate_aw(
  CityFFX,
  NovaStopsRoutes_2022 %>% filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "HR")
CityFFXpara <- st_interpolate_aw(
  CityFFX,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

CityFFXAccess <- rbind(CityFFXBus, CityFFXpara) %>% mutate(Jurisdiction = "City of Fairfax")
#Fairfax
ffxCR <- st_interpolate_aw(
  ffx,
  NovaStopsRoutes_2022 %>% filter(Mode == "CR") %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "CR")
ffxBus <- st_interpolate_aw(
  ffx,
  NovaStopsRoutes_2022 %>% filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")
ffxHR <- st_interpolate_aw(
  ffx,
  NovaStopsRoutes_2022 %>% filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "HR")
ffxpara <- st_interpolate_aw(
  ffx,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

ffxAccess <- rbind(ffxBus, ffxHR, ffxpara, ffxCR) %>% mutate(Jurisdiction = "Fairfax County")
#Loudoun
louCR <- st_interpolate_aw(
  Lou,
  NovaStopsRoutes_2022 %>% filter(Mode == "CR") %>%
    st_buffer(dist = 1600) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "CR")
louBus <- st_interpolate_aw(
  Lou,
  NovaStopsRoutes_2022 %>% filter(Mode == "Bus") %>%
    st_buffer(dist = 400) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Bus")
louHR <- st_interpolate_aw(
  Lou,
  NovaStopsRoutes_2022 %>% filter(Mode == "HR") %>%
    st_buffer(dist = 800) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "HR")
loupara <- st_interpolate_aw(
  Lou,
  NovaStopsRoutes_2022 %>%
    st_buffer(dist = 1200) %>% st_union() %>% st_make_valid(),
  extensive = T
) %>% mutate(Mode = "Paratransit")

LouAccess <- rbind(louBus, loupara) %>% mutate(Jurisdiction = "Loudoun County")

NovaAcessJurisdictions <- rbind(LouAccess,
                                ArlAccess,
                                AlxAccess,
                                ffxAccess,
                                CityFFXAccess,
                                fcAccess)
st_write(NovaAcessJurisdictions, "AgencyProfileData/NovaAccessJurisdictions.xlsx")


NovaCensus <- rbind(Arl %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "Arlington"),
                    Alx %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "Alexandria"),
                    Lou %>% st_drop_geometry() %>% summarize_all(sum) %>% mutate(County = "Loudoun"),
                    ffx %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "Fairfax"),
                    fc %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "Falls Church"),
                    CityFFX %>% st_drop_geometry()%>% summarize_all(sum) %>% mutate(County = "CityFairfax"))

st_write(NovaCensus, "AgencyProfileData/NovaCensusSum.xlsx")

colnames(NovaStopsRoutes)

NovaStopsRoutes <- NovaStopsRoutes_2022 %>%
  mutate(lat = st_coordinates(.)[,2],
         lon = st_coordinates(.)[,1]) %>%
  st_drop_geometry()
st_write(NovaStopsRoutes, "AgencyProfileData/NovaStops.xlsx")
