library(sf)
library(tmap)
library(tidyverse)

crs <- 4326

A1 <- read_csv("/Users/wangqiqian/Desktop/ST-RTA/ComputedData/Accident/DataA1_with_youbike.csv")%>%
  st_as_sf(coords = c("經度", "緯度"), crs = crs)
A2 <- read_csv("/Users/wangqiqian/Desktop/ST-RTA/ComputedData/Accident/DataA2_with_youbike.csv")%>%
  st_as_sf(coords = c("經度", "緯度"), crs = crs)
# taiwan <- st_read("Data/村(里)界(TWD97經緯度)/VILLAGE_NLSC_1140825.shp")
taiwan <- st_read("Data/村(里)界(TWD97經緯度)/VILLAGE_NLSC_1140825.shp")%>%st_transform(crs)
taiwan_road <- st_read("Data/road/gis_osm_roads_free_1.shp")%>%st_transform(crs)

taiwan_crop <- taiwan%>%st_crop(xmin = 119, ymin = 20, xmax = 123, ymax = 26)

