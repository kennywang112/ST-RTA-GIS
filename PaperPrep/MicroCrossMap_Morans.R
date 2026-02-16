morans <- read_csv('../ST-RTA/ComputedDataV2/Grid/local_moran_results.csv') %>%
  st_as_sf(wkt = "geometry", crs = 3826)

morans_filter <- morans%>%
  filter(num_accidents > 0)

quadrant_labels <- c(
  "Not Significant" = "#eeeeee",
  "High-High" = "#d73027",
  "Low-Low" = "#4575b4",
  "High-Low" = "#f46d43",
  "Low-High" = "#74add1"
)
tm_shape(morans_filter) +
  tm_fill(col = "quadrant",
          palette = quadrant_labels,
          title = "Local Morans I",
          border.alpha = 0,
          lwd = 0)+
  tm_shape(boundary) +
  tm_borders(col = "black", lwd = 0.1) +
  add_map_decorations()
