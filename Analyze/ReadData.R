library(sf)
library(tmap)

# crs <- 4326
crs <- 3826
#
# A1 <- read_csv("/Users/wangqiqian/Desktop/ST-RTA/ComputedData/Accident/DataA1_with_youbike.csv")%>%
#   st_as_sf(coords = c("經度", "緯度"), crs = crs)
# A2 <- read_csv("/Users/wangqiqian/Desktop/ST-RTA/ComputedData/Accident/DataA2_with_youbike.csv")%>%
#   st_as_sf(coords = c("經度", "緯度"), crs = crs)
# taiwan <- st_read("Data/村(里)界(TWD97經緯度)/VILLAGE_NLSC_1140825.shp")
taiwan <- st_read("Data/村(里)界(TWD97經緯度)/VILLAGE_NLSC_1140825.shp")%>%st_transform(crs)
taiwan_road <- st_read("Data/road/gis_osm_roads_free_1.shp")%>%st_transform(crs)

taiwan_crop <- taiwan%>%st_crop(xmin = 119, ymin = 20, xmax = 123, ymax = 26)

combined_data_in_taiwan <- read_csv("/Users/wangqiqian/Desktop/ST-RTA/ComputedDataV2/Accident/combined_data_in_taiwan.csv")

roads <- st_read(dsn="~/Desktop/RTA-GIS/Data/road/gis_osm_roads_free_1.shp", layer="gis_osm_roads_free_1")
youbike <- read_csv("~/Desktop/ST-RTA/ComputedDataV2/Youbike/full_youbike.csv")


taipei <- st_read(dsn="~/Desktop/RTA-GIS/Data/縣市界線(TWD97經緯度)/COUNTY_MOI_1090820.shp", layer="COUNTY_MOI_1090820")%>%
  filter(COUNTYNAME == "臺北市")

roads <- st_read(dsn="~/Desktop/RTA-GIS/Data/road/gis_osm_roads_free_1.shp", layer="gis_osm_roads_free_1")

# crs transformation
roads_3826  <- roads%>%st_transform(crs)
taipei_3826 <- taipei%>%st_transform(crs)

taipei_roads <- st_intersection(roads_3826, taipei_3826)
