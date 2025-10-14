library(tmap)
library(leaflet)
library(sp)
library(sf)
library(raster)
library(adehabitatHR)
library(tidyverse)

A_p  <- st_transform(A2, 3826)%>%as("Spatial")
kde.output <- kernelUD(A_p, h="href", grid = 2000)

plot(kde.output)

kde <- raster(kde.output)

r <- raster(kde.output)

# 存成 GeoTIFF，QGIS 可直接讀
writeRaster(r, "../CalculatedData/kde_output.tif", format = "GTiff", overwrite = TRUE)
