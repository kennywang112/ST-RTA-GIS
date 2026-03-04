library(sf)
library(tmap)
library(tidyverse)
source('./utils/map_func.R')

boundary <- st_read('../ST-RTA/ComputedDataV2/Taiwan/taiwan.shp')
trend_grid <- read_csv('../ST-RTA/ComputedDataV7/Grid/trend_grid.csv') %>% st_as_sf(wkt = "geometry", crs = 3826)

# 這是針對尖峰、離峰區域的分類
trend_grid$target_label%>%table()
trend_grid$mk_zscore

tmap_mode("view")
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

# mk_zscore <- tm_shape(trend_grid) +
#   tm_polygons(
#     col = "mk_zscore",
#     palette = "RdYlBu",
#     border.col = "#bac8cc",
#     lwd = 0.1,
#     title = "MK Z-score") +
#   tm_shape(boundary) +
#   tm_borders(col = "grey20", lwd = 0.05, alpha = 0.3) +
#   add_map_decorations()

trend_grid$DTW_Cluster <- factor(trend_grid$DTW_Cluster,
                                 levels = c(0, 1, 2),
                                 labels = c("Cluster 0 (Rural/Suburbs)", "Cluster 1 (Urban Cores)", "Cluster 2 (Industrial/Hubs)"))
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

# tmap_arrange(gimk, dtw_cluster, ncol = 2)
tmap_save(dtw_cluster, filename = "./Layouts/dtw_cluster.png", width = 8, height = 14)


##################################################################
north_counties <- c("臺北市", "新北市", "基隆市", "桃園市", "新竹市", "新竹縣")
central_counties <- c("苗栗縣", "臺中市")
south_counties <- c("嘉義市", "嘉義縣", "臺南市", "高雄市", "屏東縣")
east_counties <- c("花蓮縣", "臺東縣")

tmap_mode("plot")

create_region_map <- function(data, region_name) {
  tm_shape(data) +
    tm_polygons(
      col = "target_label",
      palette = c(
        "Dissipated" = "#74add1",
        "Stable_Safe" = "#4575b4",
        "Emergent" = "#fdae61",
        "Persistent" = "#d73027"
      ),
      border.col = "#bac8cc",
      lwd = 0.1,
      title = region_name) +
    tm_shape(boundary) +
    tm_borders(col = "grey20", lwd = 0.1, alpha = 0.3) +
    tm_layout(
      legend.show = FALSE,
      frame = FALSE,
      main.title = region_name,
      main.title.size = 0.8,
      main.title.position = "center"
    )
}

map_north <- create_region_map(trend_grid %>% filter(COUNTYNAME %in% north_counties), "Northern Taiwan")
map_central <- create_region_map(trend_grid %>% filter(COUNTYNAME %in% central_counties), "Central Taiwan")
map_south <- create_region_map(trend_grid %>% filter(COUNTYNAME %in% south_counties), "Southern Taiwan")
map_east <- create_region_map(trend_grid %>% filter(COUNTYNAME %in% east_counties), "Eastern Taiwan")

map_east_with_legend <- map_east + tm_layout(legend.show = TRUE, legend.position = c("right", "bottom"))

final_layout <- tmap_arrange(map_north, map_central, map_south, map_east_with_legend, ncol = 4)

print(final_layout)
tmap_save(final_layout, filename = "./Layouts/Accident_Trend_Comparison.png", width = 16, height = 8)
