library(spNetwork)
library(tidyverse)
source("./Analyze/ReadData.R")

# roadsBuffer <- st_buffer(roads, dist=250)

# transfer youbike to sf object
youbike_sf <- st_as_sf(youbike, coords = c("PositionLon", "PositionLat"), crs = 4326)
youbike_sf_m <- st_transform(youbike_sf, 3857)
youbike_buf <- st_buffer(youbike_sf_m, dist = 100)

library(tmaptools)

tmap_mode("view")
tm_basemap("CartoDB.Positron") +
  tm_shape(youbike_buf) +
  tm_polygons(col = "blue", alpha = 0.3, border.col = "blue") +
  tm_shape(roads%>%head(1000)) +
  tm_lines(col = "red", alpha = 0.5)


# roads <- roads%>%head(1000)
roads_m <- st_transform(roads, st_crs(youbike_buf))
# 把多個 buffer 合併成一個（加速且避免重疊邊界問題）
buf_union <- st_union(youbike_buf) %>% st_make_valid()
# 篩選有與 buffer 相交的道路（整段保留）
roads_hit <- st_filter(roads_m, buf_union)

tm_basemap("CartoDB.Positron") +
  tm_shape(buf_union) + tm_polygons(alpha = 0.2) +
  tm_shape(roads_hit) + tm_lines(col = "red", lwd = 2)



# 先確保合法幾何
roads_m_v <- st_make_valid(roads_m)
# 只保留道路在 buffer 裡的那一小段（線會被裁切）
roads_clip <- st_intersection(roads_m_v, buf_union)

tm_basemap("CartoDB.Positron") +
  tm_shape(buf_union) + tm_polygons(alpha = 0.2) +
  tm_shape(roads_clip) + tm_lines(col = "red", lwd = 2)


roads_clip$maxspeed%>%mean()
