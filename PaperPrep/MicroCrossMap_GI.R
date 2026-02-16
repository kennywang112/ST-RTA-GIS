library(sf)
library(tidyverse)

boundary <- st_read('../ST-RTA/ComputedDataV2/Taiwan/taiwan.shp')
youbike <- read_csv('../ST-RTA/ComputedData/Youbike/full_youbike.csv')%>%
  st_as_sf(coords = c("PositionLon", "PositionLat"), crs = 4326)%>%
  st_transform(3826)

roads <- st_read("./Data/road_new.shp/gis_osm_roads_free_1.shp")


# target_counties <- c("臺北市", "新北市", "臺中市", "高雄市", "花蓮縣", "臺東縣")
target_counties <- c("花蓮縣", "臺東縣")
boundary_target <- boundary %>%
  filter(COUNTYNAME %in% target_counties) %>%
  select(COUNTYNAME)
roads_3826 <- st_transform(roads_classified, 3826)
boundary_3826 <- st_transform(boundary_target, 3826)
roads_filtered <- st_intersection(roads_3826, boundary_3826)

roads_classified <- roads_filtered %>%
  mutate(
    road_type = case_when(
      fclass %in% c('motorway','motorway_link','trunk','trunk_link','primary','primary_link','busway') ~ "Major Roads",
      fclass %in% c('secondary','secondary_link','tertiary','tertiary_link','living_street') ~ "Secondary Roads",
      fclass %in% c('residential','service','unclassified','unknown') ~ "Local Roads",
      fclass %in% c('footway','path','pedestrian','steps','track','track_grade1','track_grade2','track_grade3','track_grade4','track_grade5') ~ "Pedestrian & Paths",
      TRUE ~ "Other")) %>%
  filter(road_type != "Other") %>%
  mutate(
    line_width = case_when(
      road_type == "Major Roads" ~ 3.0,
      road_type == "Secondary Roads" ~ 1.5,
      road_type == "Local Roads" ~ 0.5,
      road_type == "Pedestrian & Paths" ~ 0.2))

gi <- read_csv('/Users/wangqiqian/Desktop/ST-RTA/ComputedDataV2/Grid/grid_giV3.csv') %>%
  st_as_sf(wkt = "geometry", crs = 3826)
gi <- gi%>%
  mutate(hotspot_simplify = case_when(
    hotspot %in% c("Hotspot 99%", "Hotspot 95%", "Hotspot 90%") ~ "Hotspot",
    hotspot %in% c("Coldspot 99%", "Coldspot 95%", "Coldspot 90%") ~ "Coldspot",
    TRUE ~ "Not Hotspot"
  ))

gi <- gi %>%
  mutate(
    station_count = lengths(st_intersects(geometry, youbike)),
    Infrastructure_Label = case_when(
      station_count > 0 ~ "Infrastructure",
      TRUE ~ "No Infrastructure"
    )
  )

gi <- gi %>%
  mutate(
    final_label = case_when(
      hotspot_simplify == "Hotspot" & Infrastructure_Label == "Infrastructure" ~ "Hotspot with Infrastructure",
      hotspot_simplify == "Hotspot" & Infrastructure_Label == "No Infrastructure" ~ "Hotspot without Infrastructure",
      hotspot_simplify == "Not Hotspot" & Infrastructure_Label == "Infrastructure" ~ "Not Hotspot with Infrastructure",
      hotspot_simplify == "Not Hotspot" & Infrastructure_Label == "No Infrastructure" ~ "Not Hotspot without Infrastructure",
      TRUE ~ "Other"
    )
  )

gi_centroids <- st_centroid(gi)
joined_info <- st_join(gi_centroids, boundary)
gi$COUNTYNAME <- joined_info$COUNTYNAME
gi$TOWNNAME <- joined_info$TOWNNAME

gi <- gi%>%filter(COUNTYNAME %in% c("臺北市", "新北市", "臺中市", "高雄市", "花蓮縣", "臺東縣"))

gi_filtered <- gi %>%
  group_by(COUNTYNAME, TOWNNAME) %>%
  filter(n() < 10000) %>%
  ungroup()%>%
  filter(num_accidents > 10)

gi_filtered$final_label%>%table()
library(tmap)
source('./utils/map_func.R')
tmap_mode("view")

final_colors <- c(
  "Hotspot with Infrastructure" = "#b30000",
  "Hotspot without Infrastructure" = "#e34a33",
  "Not Hotspot with Infrastructure" = "#74add1",
  "Not Hotspot without Infrastructure" = "#4575b4"
)
hotspot_colors <- c(
  "Hotspot" = "#b30000",
  "Coldspot" = "#4575b4",
  "Not Hotspot" = "#eeeeee")

tmap_mode("view")

m <- tm_shape(boundary) +
  tm_borders(col = "grey20", lwd = 1, alpha = 0.3) +
  tm_shape(roads_classified) +
  tm_lines(
    col = "road_type",
    lwd = "line_width",
    scale = 2,
    title.col = "Road Type",
    palette = c(
      "Major Roads" = "#E31A1C",
      "Secondary Roads" = "#FD8D3C",
      "Local Roads" = "#969696",
      "Pedestrian & Paths" = "#D9D9D9"
    ),
    legend.lwd.show = FALSE,
    popup.vars = c("Road Name" = "name", "Type" = "fclass"))+
  # tm_shape(youbike) +
  # tm_dots(col = "black",
  #         size = 0.2,
  #         alpha = 0.4) +
  tm_shape(gi_filtered) +
  tm_fill(col = "final_label",
          palette = final_colors,
          title = "Classification",
          alpha = 0.7,
          border.alpha = 0,
          popup.vars = c("Accidents" = "num_accidents", "Type" = "final_label")) +
  add_map_decorations()

m
