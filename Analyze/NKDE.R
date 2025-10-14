library(sf)
library(spNetwork)
library(tmap)
source("Analyze/ReadData.R")

crs_proj <- 3826

road <- taiwan %>%
  filter(COUNTYNAME == "臺北市" & TOWNNAME == "信義區")

road_with_county <- st_join(
  taiwan_road,
  road%>%select(COUNTYNAME, TOWNNAME),
  join = st_intersects,
  left = FALSE,
)

road_with_county <- road_with_county %>%
  st_transform(crs_proj) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_cast("LINESTRING") %>%
  select(geometry)

accident <- A2 %>%
  st_transform(crs_proj) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_cast("POINT") %>%
  select(geometry)

accident_in_shinyi <- st_join(
  accident,
  road %>%
    st_transform(crs_proj)%>%
    select(COUNTYNAME, TOWNNAME),
  join = st_intersects,
  left = FALSE,
)

lixels  <- lixelize_lines(road_with_county, 200, mindist = 100)
samples <- lines_center(lixels)

densities <- nkde(road_with_county,
                  events = accident_in_shinyi,
                  w = rep(1, nrow(accident_in_shinyi)),
                  samples = samples,
                  kernel_name = "gaussian",
                  bw = 300, div= "bw",
                  method = "discontinuous", digits = 1, tol = 1,
                  grid_shape = c(5,5), max_depth = 8,
                  agg = 5,
                  sparse = TRUE,
                  verbose = FALSE)

# rescaling to help the mapping
samples$density <- densities * 100

tm_shape(taipei_road) +
  tm_lines(col = "gray80") +
  tm_shape(samples) +
  tm_dots(col = "density", palette = "-YlOrRd", size = 0.5, alpha = 0.7,
          title = "Density") +
  tm_layout(title = "Network Kernel Density Estimation",
            title.size = 1.2,
            legend.outside = TRUE)





