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
          title = "Gi*",
          border.alpha = 0,
          lwd = 0)+
  tm_shape(boundary) +
  tm_borders(col = "black", lwd = 0.1) +
  add_map_decorations()

tmap_save(tw_gi, filename = "./Layouts/GI_map.png", width = 6, height = 10, dpi = 300)


### Morans I
morans_i <- read_csv("/Users/wangqiqian/Desktop/ST-RTA/ComputedDataV2/Grid/local_moran_results.csv")%>%
  st_as_sf(wkt = "geometry", crs = 3826)

quadrant_labels <- c(
  "Not Significant" = "#eeeeee",
  "High-High" = "#d73027",
  "Low-Low" = "#4575b4",
  "High-Low" = "#f46d43",
  "Low-High" = "#74add1"
)
tmap_mode("plot")
morans_map <- tm_shape(morans_i) +
  tm_fill(col = "quadrant",
          palette = quadrant_labels,
          title = "Local Morans I",
          border.alpha = 0,
          lwd = 0)+
  tm_shape(boundary) +
  tm_borders(col = "black", lwd = 0.1) +
  add_map_decorations()
tmap_save(morans_map, filename = "./Layouts/Morans_map.png", width = 6, height = 10, dpi = 300)

plot_data <- morans_i %>%
  st_drop_geometry() %>%
  mutate(quadrant = factor(quadrant, levels = c("High-High", "High-Low", "Low-High", "Low-Low", "Not Significant")))

plot_data_sig <- plot_data %>% filter(significant == TRUE)

plot_data_sig%>%
  ggplot(aes(x = moran_std_val, y = moran_lag_val)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(color = quadrant), size = 0.1, alpha = 0.3) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 0.8) +
  scale_color_manual(values = quadrant_labels) +
  labs(title = "Moran's I Scatterplot",
       x = "Standardized Accidents (Z-Score)",
       y = "Spatial Lag (Avg of Neighbors)",
       color = "LISA Quadrant") +
  theme_minimal() +
  theme(legend.position = "bottom")

# outlier area
tmap_mode("view")
morans_i%>%
  # filter((quadrant %in% c("High-Low", "Low-High")) & (num_accidents > 0))%>%
  filter(num_accidents > 0)%>%
  tm_shape() +
  tm_fill(col = "quadrant",
          palette = quadrant_labels,
          title = "Local Morans I",
          border.alpha = 0,
          lwd = 0)+
  tm_shape(boundary) +
  tm_borders(col = "black", lwd = 0.1) +
  add_map_decorations()
