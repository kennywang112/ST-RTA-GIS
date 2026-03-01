library(sf)
library(tmap)
library(tidyverse)
source('./utils/map_func.R')

boundary <- st_read('../ST-RTA/ComputedDataV2/Taiwan/taiwan.shp')
trend_grid <- read_csv('../ST-RTA/ComputedDataV7/Grid/trend_grid.csv') %>% st_as_sf(wkt = "geometry", crs = 3826)

# 這是針對尖峰、離峰區域的分類
trend_grid$target_label%>%table()
trend_grid$mk_zscore

tmap_mode("plot")
gimk <- tm_shape(trend_grid) +
  tm_polygons(
    col = "target_label",
    palette = c(
      "Dissipated" = "#74add1",
      "Stable_Safe" = "#4575b4",
      "Emergent" = "#fdae61",
      "Persistent" = "#d73027"
    ),
    border.col = "#bac8cc",
    lwd = 0.2,
    title = "Accident Trend") +
  tm_shape(boundary) +
  tm_borders(col = "grey20", lwd = 0.05, alpha = 0.3) +
  add_map_decorations()

mk_zscore <- tm_shape(trend_grid) +
  tm_polygons(
    col = "mk_zscore",
    palette = "RdYlBu",
    border.col = "#bac8cc",
    lwd = 0.1,
    title = "MK Z-score") +
  tm_shape(boundary) +
  tm_borders(col = "grey20", lwd = 0.05, alpha = 0.3) +
  add_map_decorations()

dtw_cluster <- tm_shape(trend_grid) +
  tm_polygons(
    col = "DTW_Cluster",
    palette = "RdYlBu",
    border.col = "#bac8cc",
    lwd = 0.1,
    title = "DTW cluster"
    ) +
  tm_shape(boundary) +
  tm_borders(col = "grey20", lwd = 0.05, alpha = 0.3) +
  add_map_decorations()

tmap_arrange(mk_zscore,gimk, dtw_cluster, ncol = 3)


