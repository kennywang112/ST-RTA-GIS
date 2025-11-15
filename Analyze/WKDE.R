library(spatstat.geom)
library(spatstat)

data <- st_read(dsn="./CalculatedData/pairs_annot_all_cities.shp", layer="pairs_annot_all_cities")%>%st_transform(crs)

data_sf <- data
coords <- st_coordinates(data_sf)
win <- as.owin(st_bbox(data_sf))

taiwan_sf <- st_as_sf(taiwan_crop) %>%
  st_transform(3826) %>%
  st_union()
taiwan_owin <- as.owin(taiwan_sf)

pp <- ppp(x = coords[,1],
          y = coords[,2],
          window = taiwan_owin,
          marks = data_sf$spd_dlt)

bw0 <- bw.diggle(pp)

lambda <- density.ppp(pp, weights = marks(pp), sigma = bw0 * 0.1, edge = TRUE, dimyx = 1000)

kde <- raster(lambda)


plot(kde)

crs(kde) <- st_crs(data_sf)$wkt
kde[kde <= 0] <- NA

writeRaster(kde, "/Users/wangqiqian/Desktop/RTA-GIS/CalculatedData/wkde_speeddifference.tif", format = "GTiff", overwrite = TRUE)

tmap_mode("view")

tm_shape(kde) +
  tm_raster(style = "quantile", n = 7, palette = "Blues", alpha = 0.85) +
  tm_layout(legend.outside = TRUE, frame = FALSE)


# accident

combined_sf <- st_as_sf(
  combined_data_in_taiwan,
  coords = c("經度", "緯度"),
  crs = 4326
)

combined_sf <- st_transform(combined_sf, 3826)
coords <- st_coordinates(combined_sf)

pp <- ppp(x = coords[,1],
          y = coords[,2],
          window = taiwan_owin)

bw0 <- bw.diggle(pp)

lambda <- density.ppp(pp, weights = marks(pp), sigma = bw0 * 0.1, edge = TRUE, dimyx = 1000)

kde_accident <- raster(lambda)
plot(kde_accident)
writeRaster(kde_accident, "/Users/wangqiqian/Desktop/RTA-GIS/CalculatedData/kde_acc", format = "GTiff", overwrite = TRUE)

# scatter

res(kde); res(kde_accident)
extent(kde); extent(kde_accident)

r_stack <- stack(kde, kde_accident)
df <- as.data.frame(r_stack, xy = FALSE)
colnames(df) <- c("speedKDE", "accKDE")

df <- na.omit(df)

cor_value <- cor(df$speedKDE, df$accKDE, use = "complete.obs", method = "pearson")
cor_value

ggplot(df, aes(x = speedKDE, y = accKDE)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(
    x = "Speed difference KDE",
    y = "Accident KDE"
  )

