library(sf)
library(tmap)
library(tidyverse)

crs_init <- 4326
crs <- 3826

taiwan <- st_read("Data/村(里)界(TWD97經緯度)/VILLAGE_NLSC_1140825.shp")
roads <- st_read(dsn="~/Desktop/ST-RTA-GIS/Data/road/gis_osm_roads_free_1.shp", layer="gis_osm_roads_free_1")

taiwan_crop <- taiwan%>%st_crop(xmin = 119, ymin = 20, xmax = 123, ymax = 26)
combined_data_in_taiwan <- read_csv("/Users/wangqiqian/Desktop/ST-RTA/ComputedDataV2/Accident/combined_data_in_taiwan.csv")


# 這個檔案的目的是找出離每個事故最近的youbike設施
youbike <- read_csv('../ST-RTA/ComputedData/Youbike/full_youbike.csv')
mrt <- read_csv('../ST-RTA/ComputedData/MRT/full_mrt.csv')
parking <- read_csv('../ST-RTA/ComputedData/Parkinglot/full_parkinglot.csv')

taiwan_3826 <- st_transform(taiwan, 3826)
youbike_sf <- st_as_sf(youbike, coords = c("PositionLon", "PositionLat"), crs = 4326) %>%
  st_transform(3826)%>%
  filter(!st_is_empty(geometry))%>%
  st_filter(taiwan_3826, .predicate = st_intersects)
mrt_sf <- st_as_sf(mrt, coords = c("PositionLon", "PositionLat"), crs = 4326) %>%
  st_transform(3826)%>%
  filter(!st_is_empty(geometry))%>%
  st_filter(taiwan_3826, .predicate = st_intersects)
parking_sf <- st_as_sf(parking, coords = c("PositionLon", "PositionLat"), crs = 4326)%>%
  st_transform(3826)%>%
  filter(!st_is_empty(geometry))%>%
  st_filter(taiwan_3826, .predicate = st_intersects)


combined_sf <- st_as_sf(combined_data_in_taiwan, coords = c("經度", "緯度"), crs = 4326) %>%
  st_transform(3826)

# 北部: 臺北市、新北市、基隆市、桃園市、新竹市、新竹縣
# 中部: 苗栗縣、臺中市、嘉義市、嘉義縣
# 南部: 臺南市、高雄市、屏東縣
# 東部: 臺東縣
# taipei <- st_read(dsn="~/Desktop/ST-RTA-GIS/Data/縣市界線(TWD97經緯度)/COUNTY_MOI_1090820.shp", layer="COUNTY_MOI_1090820")#%>%
  # filter(COUNTYNAME %in% c("臺北市", "新北市", "基隆市", "桃園市", "新竹市", "新竹縣",
  #                          "苗栗縣", "臺中市", "嘉義市", "嘉義縣",
  #                          "臺南市", "高雄市", "屏東縣",
  #                          "臺東縣"))

# crs transformation
# roads_3826  <- roads%>%st_transform(crs)
# taipei_3826 <- taipei%>%st_transform(crs)
#
# taipei_roads <- st_intersection(roads_3826, taipei_3826)

# write_sf(
#   taipei_roads,
#   "./CalculatedData/roads_all_city.shp",
#   layer_options = "ENCODING=UTF-8"
# )
