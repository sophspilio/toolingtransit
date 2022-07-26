library(tidyverse)
library(censusapi)
library(tidycensus)
library(sf)
library(units)
library(tidytransit)
library(toolingtransit)
library(mapview)

amountservice_demos <- st_read("data/amountservice_demos.shp") %>% mutate(perCount0Car = Cont0Cr/HHContE, perNonWhite = NnWhtPp/TtlPplE, perUnder200 = Un200PP/TtlPplE) %>% select(TSI19, TSI22, TSI_chan_1, perCount0Car, perNonWhite, perUnder200) %>% rename(TSI_change = TSI_chan_1)

amountservice_demos

st_write(amountservice_demos, "data/amountservice_demos.xlsx")
