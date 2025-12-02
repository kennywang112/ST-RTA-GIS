library(tmap)
library(leaflet)
library(sp)
library(sf)
library(raster)
library(adehabitatHR)
library(tidyverse)

# data <- specific_combined_data_sf%>%
  # c("臺北市", "新北市", "臺中市", "高雄市", "臺東縣", "花蓮縣")
  # filter(COUNTYNAME %in% c("臺北市"))
# data <- data%>%filter(事故類型及型態子類別名稱=='追撞')
# data <- data%>%filter(`號誌-號誌種類名稱`=='行車管制號誌(附設行人專用號誌)')

data <- combined_data_in_taiwan%>%
  filter(COUNTYNAME %in% c("臺北市"))
data <- data%>%filter(事故類型及型態子類別名稱=='追撞')

# data <- data%>%filter(`車道劃分設施-分向設施大類別名稱`=='中央分向島')
# data <- data%>%filter(youbike_100m_count > 0)
# data <- data%>%filter(COUNTYNAME=='臺北市' & `當事者區分.類別.子類別名稱.車種`=='腳踏自行車')

# 存成 GeoTIFF，QGIS 可直接讀
# writeRaster(r, "./CalculatedData/kde_output_bike.tif", format = "GTiff", overwrite = TRUE)

data_sp <- st_as_sf(data, coords = c("經度", "緯度"), crs = crs_init)%>%as_Spatial()
kde.output <- kernelUD(data_sp, h="href", grid = 1000)
h0 <- kde.output@h$h
kde.output <- kernelUD(data_sp, h=h0*0.4, grid = 3000)
kde <- raster(kde.output)
# sets projection to British National Grid
# projection(kde) <- CRS("+init=EPSG:27700")

tm_shape(kde) + tm_raster()

masked_kde <- mask(kde, data_sp)
tm_shape(masked_kde) + tm_raster()

tmap_mode("view")
tm_basemap(providers$Esri.WorldTopoMap)+
  tm_shape(masked_kde) +
  tm_raster(style = "cont", legend.show = FALSE, palette = "RdGy") +
  tm_shape(data_sp) + tm_borders(alpha=0.3, col = "white") +
  tm_layout(frame = FALSE)


kde_trim <- kde
kde_trim[kde_trim <= 0] <- NA

tm_shape(kde_trim) +
  tm_raster(style = "quantile", n = 7, palette = "Blues", alpha = 0.85) +
  tm_layout(legend.outside = TRUE, frame = FALSE)

# save kde_trim
writeRaster(kde_trim, "/Users/wangqiqian/Desktop/RTA-GIS/CalculatedData/kde_taipei.tif", format = "GTiff", overwrite = TRUE)
