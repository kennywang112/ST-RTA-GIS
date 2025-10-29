county <- st_read(dsn="~/Desktop/RTA-GIS/Data/縣市界線(TWD97經緯度)/COUNTY_MOI_1090820.shp", layer="COUNTY_MOI_1090820")
taipei <- county %>% filter(COUNTYNAME == "臺北市")

roads <- st_read(dsn="~/Desktop/RTA-GIS/Data/road/gis_osm_roads_free_1.shp", layer="gis_osm_roads_free_1")

# crs transformation
roads_3826  <- st_transform(roads, 3826)
taipei_3826 <- st_transform(taipei, 3826)

taipei_roads <- st_intersection(roads_3826, taipei_3826)

roads_3826%>%dim()
taipei_roads%>%dim()
