source("Analyze/ReadData.R")

idx <- st_intersects(taiwan_crop, A2)
taiwan_with_A <- taiwan_crop%>%mutate(A1_n = lengths(idx))
taipei_with_A <- taiwan_with_A%>%filter(COUNTYNAME=="臺北市")

taiwan_road_with_county <- st_join(
  taiwan_road,
  taiwan %>% select(COUNTYNAME),
  join = st_intersects,
  left = TRUE,
)

# taiwan_road_with_county <- st_intersection(
#   taiwan_road,
#   taiwan %>% select(COUNTYNAME)
# )

# save taiwan_road_with_county as shp
st_write(taiwan_road_with_county, "CalculatedData/taiwan_road_with_county.shp", delete_dsn = TRUE)

# tmap_mode("view")
tmap_mode("plot")
taipei_with_A%>%
  tm_shape() +
  tm_polygons(col = "A1_n", style = "quantile", border.col = "grey40", title = "total A1") +
  tm_layout(title = "total A1 accident")# +
  # tm_shape(taiwan_road_with_county)+
  # tm_lines(col="maxspeed", style="jenks", lwd=2.0, title.col="Road")
