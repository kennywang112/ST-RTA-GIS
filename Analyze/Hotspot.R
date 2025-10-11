library(sf)
library(tmap)
library(tidyverse)

# taiwan <- st_read("Data/村(里)界(TWD97經緯度)/VILLAGE_NLSC_1140825.shp")
taiwan <- st_read("Data/村(里)界(TWD97經緯度)/VILLAGE_NLSC_1140825.shp")
taiwan_road <- st_read("Data/縣市界線(TWD97經緯度)/COUNTY_MOI_1090820.shpp")
A1 <- read_csv("/Users/wangqiqian/Desktop/ST-RTA/ComputedData/Accident/DataA1_with_youbike.csv")
A2 <- read_csv("/Users/wangqiqian/Desktop/ST-RTA/ComputedData/Accident/DataA2_with_youbike.csv")

crs <- 4326

A1_sf <- A1%>%st_as_sf(coords = c("經度", "緯度"), crs = crs)
A2_sf <- A2%>%st_as_sf(coords = c("經度", "緯度"), crs = crs)
taiwan <- st_transform(taiwan, crs)
taiwan_road <- st_transform(taiwan_road, crs)

taiwan_crop <- taiwan%>%st_crop(xmin = 119, ymin = 20, xmax = 123, ymax = 26)
road_crop <- taiwan_road%>%st_crop(xmin = 121, ymin = 20, xmax = 123, ymax = 26)

idx <- st_intersects(taiwan_crop, A2_sf)
taiwan_with_A1 <- taiwan_crop%>%mutate(A1_n = lengths(idx))
taipei_with_A1 <- taiwan_with_A1%>%filter(COUNTYNAME=="臺北市")

# tmap_mode("view")
tmap_mode("plot")
taipei_with_A1%>%
  tm_shape() +
  tm_polygons(col = "A1_n", style = "quantile", border.col = "grey40", title = "total A1") +
  tm_layout(title = "total A1 accident") +
  tm_shape(road_crop)+
  tm_lines(col="maxspeed", style="jenks", lwd=2.0, title.col="Road")
