library(tmap)
library(leaflet)
library(sp)
library(sf)
library(raster)
library(adehabitatHR)
library(tidyverse)

boundary <- st_read('../ST-RTA/ComputedDataV2/Taiwan/taiwan.shp')
# county <- st_read(dsn="~/Desktop/ST-RTA/Data/OFiles_9e222fea-bafb-4436-9b17-10921abc6ef2/TOWN_MOI_1140318.shp", layer="TOWN_MOI_1140318")
combined_data <- read_csv("~/Desktop/ST-RTA/ComputedDataV2/Accident/combined_data_in_taiwan.csv")
combined_data <- st_as_sf(combined_data, coords = c("經度", "緯度"), crs = 4326)%>%
  st_transform(st_crs(boundary))

get_join <- function(sdf) {
  grouped_data <- sdf%>%
    st_drop_geometry() %>%
    # group_by(COUNTYNAME, TOWNNAME)%>%
    group_by(TOWNNAME)%>%
    summarise(count = n())%>%
    arrange(desc(count))

  # join county with grouped_data
  county_count <- boundary %>%
    # left_join(grouped_data, by = c("COUNTYNAME", "TOWNNAME"))%>%
    left_join(grouped_data, by = c("TOWNNAME"))%>%
    replace_na(list(count = 0L))
    # mutate(count = dplyr::coalesce(count, 0L))

  return(county_count)
}
new_county <- get_join(combined_data)

tmap_mode("plot")
accident_count <- tm_shape(new_county) +
  tm_polygons(
    col = "count",
    # style = "fixed",
    breaks = c(0, 1000, 5000, 10000, 20000, 50000),
    palette = "viridis",
    border.col = "#bac8cc",
    lwd = 0.1,
    title = "Accidents count") +
  add_map_decorations()

tmap_save(accident_count, filename = "./Layouts/Accident_count.png", width = 6, height = 10, dpi = 300)

# 縣市分群
pedestrian <- combined_data %>%filter(`當事者區分-類別-子類別名稱-車種` == '行人')
bike <- combined_data %>%filter(`當事者區分-類別-子類別名稱-車種` == '腳踏自行車')
bike_youbike <- combined_data %>%filter((`當事者區分-類別-子類別名稱-車種` == '腳踏自行車') & (`youbike_100m_count` != 0))

count_pedestrian <- get_join(pedestrian)
couny_bike <- get_join(bike)
count_bike_youbike <- get_join(bike_youbike)


maps_config <- list(
  # list(data = count_pedestrian, title = "Pedestrian Accidents", filename = "Pedestrian_count.png"),
  # list(data = couny_bike, title = "Bike Accidents", filename = "Bike_count.png"),
  list(data = count_bike_youbike, title = "Bike Accidents with youbike", filename = "Bike_ratio_count.png")
)

for (config in maps_config) {
  current_map <- tm_shape(config$data) +
    tm_polygons(
      col = "count",
      palette = "viridis",
      border.col = "#bac8cc",
      lwd = 0.1,
      title = config$title
    ) +
    add_map_decorations()
  full_path <- paste0("./Layouts/", config$filename)
  tmap_save(current_map, filename = full_path, width = 6, height = 10, dpi = 300)
}
