library(tmap)
library(leaflet)
library(sp)
library(sf)
library(raster)
library(adehabitatHR)
library(tidyverse)

county <- st_read(dsn="~/Desktop/RTA-GIS/Data/縣市界線(TWD97經緯度)/COUNTY_MOI_1090820.shp", layer="COUNTY_MOI_1090820")
# county <- st_read(dsn="~/Desktop/ST-RTA/Data/OFiles_9e222fea-bafb-4436-9b17-10921abc6ef2/TOWN_MOI_1140318.shp", layer="TOWN_MOI_1140318")
ftown <- c('蘭嶼鄉', '綠島鄉', '琉球鄉')
fcounty <- c('金門縣', '連江縣', '澎湖縣')
# county <- county%>%filter(!(TOWNNAME %in% ftown) & !(COUNTYNAME %in% fcounty))
# plot(st_geometry(county))

combined_data <- read_csv("~/Desktop/ST-RTA/ComputedDataV2/Accident/combined_data_in_taiwan.csv")
combined_data <- st_as_sf(combined_data, coords = c("經度", "緯度"), crs = 4326)%>%
  st_transform(st_crs(county))

roundabout <- combined_data%>%
  filter(道路型態子類別名稱 == '圓環')%>%
  st_transform(st_crs(county))

get_join <- function(sdf) {
  grouped_data <- sdf%>%
    st_drop_geometry() %>%
    # group_by(COUNTYNAME, TOWNNAME)%>%
    group_by(COUNTYNAME)%>%
    summarise(count = n())%>%
    arrange(desc(count))

  # join county with grouped_data
  county_count <- county %>%
    # left_join(grouped_data, by = c("COUNTYNAME", "TOWNNAME"))%>%
    left_join(grouped_data, by = c("COUNTYNAME"))%>%
    replace_na(list(count = 0L))
    # mutate(count = dplyr::coalesce(count, 0L))

  return(county_count)
}

new_county <- get_join(roundabout)

tmap_mode("view")
tm_basemap("CartoDB.Positron") +
  tm_shape(new_county) +
  tm_polygons(
    col = "count",
    style = "fixed",
    # breaks = c(0, 1000, 5000, 10000, 20000),
    # breaks = c(0, 10, 20, 30, 40, 50),
    breaks = c(0, 50, 100, 150, 200, 250, 300),
    palette = "viridis",
    border.col = "white",
    lwd = 0.3,
    title = "Green-belt count",
  ) +
  # tm_text("TOWNID", size = 0.3, col = "black") +
  tm_compass(position = c("right","top")) +
  tm_scale_bar(position = c("right","top")) +
  tm_view()

