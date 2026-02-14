library(sf)
library(tmap)
library(tidyverse)
source('./utils/map_func.R')

tmap_mode("plot")
boundary <- st_read('../ST-RTA/ComputedDataV2/Taiwan/taiwan.shp')

palette <- c(
  "Hotspot 99%" = "#d73027",
  "Hotspot 95%" = "#f46d43",
  "Hotspot 90%" = "#fdae61",
  "Not Significant" = "#eeeeee",
  "Coldspot 90%" = "#abd9e9",
  "Coldspot 95%" = "#74add1",
  "Coldspot 99%" = "#4575b4"
)

### GI*
grid_gi <- read_csv("../ST-RTA/ComputedDataV2/Grid/grid_giV3.csv") %>%
  st_as_sf(wkt = "geometry", crs = 3826)
tmap_mode("plot")
tw_gi <- tm_shape(grid_gi) +
  tm_fill(col = "hotspot",
          palette = palette,
          title = "Gi* Analysis",
          border.alpha = 0,
          lwd = 0)+
  tm_shape(boundary) +
  tm_borders(col = "black", lwd = 0.1) +
  add_map_decorations()

tmap_save(tw_gi, filename = "./Layouts/GI_map.png", width = 6, height = 10, dpi = 300)


### Morans I
morans_i <- read_csv("/Users/wangqiqian/Desktop/ST-RTA/ComputedDataV2/Grid/local_moran_results.csv")%>%
  st_as_sf(wkt = "geometry", crs = 3826)
morans_i
