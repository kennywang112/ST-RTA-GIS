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
