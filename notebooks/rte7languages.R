#languages in Rte 7 corridor

#https://censusreporter.org/tables/C16001/

#get census language data
Languages <-  get_acs(
  geography = "tract",
  year = 2020,
  variables = c(TotalPopulation = "B01003_001",
                TotalSpanish = "C16001_003",
                SpanPoorEng = "C16001_005",
                TotalViet = "C16001_024",
                VietPoorEng = "C16001_026"),
  state = "VA",
  county = c("Fairfax County",
             "Falls church city",
             "Arlington"),
  geometry = TRUE,
  output = "wide"
) %>% st_transform(crs = 4326)

rte7 <- st_read("data/ArFallsChurchRte7.shp") %>% st_transform(crs = 4326)

buffer_half <- st_buffer(rte7, dist = 800) %>% st_union() %>% st_make_valid()

LanguageHalfMile <- st_interpolate_aw(
  Languages %>% select(-NAME, -GEOID),
  buffer_half,
  extensive = T
) %>% mutate(Buffer = "Half Mile")


buffer_quarter <- st_buffer(rte7, dist = 400) %>% st_union() %>% st_make_valid()

LanguageQuarterMile <- st_interpolate_aw(
  Languages %>% select(-NAME, -GEOID),
  buffer_quarter,
  extensive = T
) %>% mutate(Buffer = "Quarter Mile")

LanguagesRte7 <- rbind(LanguageQuarterMile, LanguageHalfMile) %>% mutate(SpanishPercent = TotalSpanishE/TotalPopulationE, VietnamesePercent = TotalVietE/TotalPopulationE)

st_interpolate_aw(
  Languages %>% select(-NAME, -GEOID),
  buffer_quarter,
  extensive = T
) %>% mapview()

st_write(LanguagesRte7, "data/LangaugesRte7.shp")
st_write(Languages, "data/LangaugesCensus.shp")
st_write(LanguagesRte7, "data/LanguagesRte7.xlsx")
