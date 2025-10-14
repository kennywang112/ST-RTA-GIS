library(sf)
library(spNetwork)
library(tmap)
source("Analyze/ReadData.R")

crs_proj <- 3826

taiwan_road_with_county <- st_join(
  taiwan_road,
  taiwan %>% select(COUNTYNAME),
  join = st_intersects,
  left = TRUE,
)

taipei_road <- taiwan_road_with_county%>%filter(COUNTYNAME=="臺北市")

taipei_road_p <- taipei_road %>%
  st_transform(crs_proj) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_cast("LINESTRING") %>%
  select(geometry)

A_p <- A1 %>%
  st_transform(crs_proj) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_cast("POINT") %>%
  select(geometry)


lixels  <- lixelize_lines(taipei_road_p, 200, mindist = 100)
samples <- lines_center(lixels)

densities <- nkde(taipei_road_p,
                  events = A_p,
                  w = rep(1, nrow(A_p)),
                  samples = samples,
                  kernel_name = "gaussian",
                  bw = 300, div= "bw",
                  method = "discontinuous", digits = 1, tol = 1,
                  grid_shape = c(5,5), max_depth = 8,
                  agg = 5,
                  sparse = TRUE,
                  verbose = FALSE)





