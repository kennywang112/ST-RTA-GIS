library(tmap)
library(sf)
library(tidyverse)
source('./utils/map_func.R')

peak_grid <- read_csv('/Users/wangqiqian/Desktop/ST-RTA/ComputedDataV7/Grid/grid_data_區級篩選_num_peak.csv')%>%
  st_as_sf(wkt = "geometry", crs = 3826)
offpeak_grid <- read_csv('/Users/wangqiqian/Desktop/ST-RTA/ComputedDataV7/Grid/grid_data_區級篩選_num_off_peak.csv')%>%
  st_as_sf(wkt = "geometry", crs = 3826)
combined_data <- read_csv("~/Desktop/ST-RTA/ComputedDataV2/Accident/combined_data_in_taiwan.csv")

tmap_mode("plot")

final_colors <- c(
  "Hotspot 99%" = "#b30000",
  "Hotspot 95%" = "#e34a33",
  "Hotspot 90%" = "#ff7f50",
  "Not Significant" = "#eeeeee",
  "Coldspot 90%" = "#74add1",
  "Coldspot 95%" = "#4575b4",
  "Coldspot 99%" = "#313695"
)

peak <- tm_shape(peak_grid) +
  tm_polygons(col = "hotspot", palette = final_colors, title = "Peak Accidents", alpha = 0.7, border.alpha = 0) +
  add_map_decorations()
offpeak <- tm_shape(offpeak_grid) +
  tm_polygons(col = "hotspot", palette = final_colors, title = "Off-Peak Accidents", alpha = 0.7, border.alpha = 0) +
  add_map_decorations()

hotspotmap <- tmap_arrange(peak, offpeak, ncol = 2)
tmap_save(hotspotmap, filename = paste0("./Layouts/peak_offpeak_hotspot.png", sep=''), width = 12, height = 6, dpi = 300)


peak_grid%>%dim()
offpeak_grid%>%dim()
df <- data.frame(
  Peak_Hotspot = peak_grid$hotspot,
  OffPeak_Hotspot = offpeak_grid$hotspot
)

hot_labels <- c("Hotspot 99%", "Hotspot 95%", "Hotspot 90%")
cold_labels <- c("Coldspot 99%", "Coldspot 95%", "Coldspot 90%")

df <- df %>%
  mutate(
    Peak_Hotspot = case_when(
      Peak_Hotspot %in% hot_labels ~ "Hotspot",
      TRUE ~ "Not Hotspot"
    ),
    OffPeak_Hotspot = case_when(
      OffPeak_Hotspot %in% hot_labels ~ "Hotspot",
      TRUE ~ "Not Hotspot"
    )
  )

df%>%table()

tab <- table(df$OffPeak_Hotspot, df$Peak_Hotspot)
prop.table(tab) %>% round(5) * 100

combined_data
