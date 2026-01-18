################################################################################
################################################################################

# 1. Preparação dos dados

options(scipen = 999) # remover notação científica dos dados

################################################################################
################################################################################

dados <- read.csv("AmSul_FinalMax.csv", header = TRUE, sep = ",")

head(dados)
summary(dados)
str(dados)

################################################################################
################################################################################

# Baixar pacotes

library(sp)
library(sf)
library(raster)
library(dplyr)

#library(lubridate)
#library(writexl)
#library(ggplot2)

################################################################################
################################################################################

# Baixar Shapefile usando sf
oceans <- st_read("shapefile/goas_v01.shp")

# Baixar Shapefile usando sf
eez <- st_read("shapefile/eez_boundaries_v12.shp")

par(mfrow=c(1, 1))

# Plotar o shapefile com personalizações
plot(oceans$geometry, col = "lightblue")
plot(eez, col = "black", add=TRUE)

# Adicionar eixos
axis(2, at = seq(-90, 90, by = 20))
axis(1, at = seq(-180, 180, by = 20))

# Definir as coordenadas de recorte
coord_limit <- c(-80, 0, -80, 10)

# Converter o objeto oceans de sf para sp
oceans_sp <- as(oceans, "Spatial")

# Transformar os polígonos para o sistema de coordenadas desejado
oceans_cropped_1 <- spTransform(oceans_sp, CRS("+proj=longlat +datum=WGS84"))

# Criar uma extensão usando a função extent do pacote raster
ext_lim <- raster::extent(coord_limit[1], coord_limit[2], coord_limit[3], coord_limit[4])

# Usar a função crop do pacote raster para recortar
oceans_cropped <- raster::crop(oceans_cropped_1, ext_lim)

plot(oceans_cropped, col = "lightblue")

# Converter o objeto oceans de sf para sp
eez_sp <- as(eez, "Spatial")

# Transformar os polígonos para o sistema de coordenadas desejado
eez_cropped_1 <- spTransform(eez_sp, CRS("+proj=longlat +datum=WGS84"))

# Usar a função crop do pacote raster para recortar
eez_cropped <- raster::crop(eez_cropped_1, ext_lim)

# Plotar Shapefile recortado
plot(eez_cropped, add=TRUE)

# Adicionar eixos y
valores_y <- c(10, 0, -10, -20, -30, -40, -50, -60, -70, -80)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-80, -60, -40, -20, 0)
axis(1, at = valores_x)

# Adicionar eixos
axis(2, at = seq(-80, 10, by = 10))
axis(1, at = seq(-80, 0, by = 20))

################################################################################
################################################################################




