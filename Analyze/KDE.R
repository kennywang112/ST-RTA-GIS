library(tmap)
library(leaflet)
library(sp)
library(sf)
library(raster)
library(adehabitatHR)
library(tidyverse)

data <- read.csv('/Users/wangqiqian/Desktop/ST-RTA/ComputedData/Accident/combineddata_with_CC.csv')
data <- data%>%filter(COUNTYNAME=='臺北市' & `當事者區分.類別.子類別名稱.車種`=='腳踏自行車')
A <- st_as_sf(data, coords = c("經度", "緯度"), crs = 4326)%>%st_transform(3826)
A <- st_zm(A, drop = TRUE, what = "ZM")
A_sp <- as(A, "Spatial")

A_sp$ID <- factor("all")

kde <- kernelUD(A_sp["ID"], h = "href", grid = 2000)

plot(kde$all)

r <- raster(kde$all)

# 存成 GeoTIFF，QGIS 可直接讀
writeRaster(r, "./CalculatedData/kde_output_bike.tif", format = "GTiff", overwrite = TRUE)
