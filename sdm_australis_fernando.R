################################################################################
################################################################################

# Baixar pacotes

install.packages("psych")
install.packages("devtools")
devtools::install_github("bio-oracle/biooracler")

library(biooracler)
library(sp)
library(dplyr)
library(psych) # Colinearidade
library(sf)
library(raster)
#library(lubridate)
library(writexl)
library(readxl)
library(ggplot2)

################################################################################
################################################################################

# 1. Preparação dos dados

options(scipen = 999) # remover notação científica dos dados

pal <- c("#76B9A5", "#E8E1A7", "#E4AD73", "#DC6D37", "#E02423") # paleta de cores
pal1 <- c("#3E49BB", "#3498DB", "yellow", "orange", "red", "darkred") # paleta de cores
blue_pal <- colorRampPalette(c("#08306B", "#2171B5", "#6BAED6", "#DEEBF7")) # paleta de cores

################################################################################
################################################################################

dados_ams <- read.csv("AmSul_FinalMax.csv")
dados_peru <- read.csv("Peru_FinalMax.csv")

head(dados_ams)
summary(dados_ams)
str(dados_ams)

head(dados_peru)
summary(dados_peru)
str(dados_peru)

dados_australis <- bind_rows(dados_ams, dados_peru)

head(dados_australis)
summary(dados_australis)
str(dados_australis)

write.csv(dados_australis,
          file = "dados_australis.csv",
          row.names = FALSE)

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
coord_limit <- c(-100, -20, -89.975, 20)

# Converter o objeto oceans de sf para sp
oceans_sp <- as(oceans, "Spatial")

# Transformar os polígonos para o sistema de coordenadas desejado
oceans_cropped_1 <- spTransform(oceans_sp, CRS("+proj=longlat +datum=WGS84"))

# Criar uma extensão usando a função extent do pacote raster
ext_lim <- raster::extent(coord_limit[1], coord_limit[2], coord_limit[3], coord_limit[4])

# Usar a função crop do pacote raster para recortar
oceans_cropped <- raster::crop(oceans_cropped_1, ext_lim)

# Converter o objeto oceans de sf para sp
eez_sp <- as(eez, "Spatial")

# Transformar os polígonos para o sistema de coordenadas desejado
eez_cropped_1 <- spTransform(eez_sp, CRS("+proj=longlat +datum=WGS84"))

# Usar a função crop do pacote raster para recortar
eez_cropped <- raster::crop(eez_cropped_1, ext_lim)

################################################################################
################################################################################

# Plotar Shapefile recortado
plot(oceans_cropped, col = "lightblue")
plot(eez_cropped, add=TRUE)

# Adicionar eixos y
valores_y <- c(20, 10, 0, -10, -20, -30, -40, -50, -60, -70, -80, -90)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-100, -90, -80, -70, -60, -50, -40, -30, -20)
axis(1, at = valores_x)

cores <- ifelse(dados_australis$Species == "Aaustralis",
                "red",
                "blue")

points(dados_australis$Long, dados_australis$Lat,
       pch = 19,
       col = cores,
       cex = 0.7)

################################################################################
################################################################################

list_layers("tas_baseline_2000_2020_depthsurf")

#camadas <- list_layers()

#write_xlsx(camadas, "informacoes_camadas.xlsx")

################################################################################
################################################################################

tas_baseline_surf <- "tas_baseline_2000_2020_depthsurf" ### Kelvin - Transformar em °C
chl_baseline_surf <- "chl_baseline_2000_2018_depthsurf" ### mg m-3
mld_baseline_surf <- "mlotst_baseline_2000_2019_depthsurf" ### m
tsm_baseline_surf <- "thetao_baseline_2000_2019_depthsurf" ### °C
sal_baseline_surf <- "so_baseline_2000_2019_depthsurf" ### PSU
swd_baseline_surf <- "swd_baseline_2000_2019_depthsurf" ### Graus
sws_baseline_surf <- "sws_baseline_2000_2019_depthsurf" ### m s**-1
produt_baseline_surf <- "phyc_baseline_2000_2020_depthsurf" ### Total Phytoplankton - MMol' 'M-3
bathy_baseline <- "terrain_characteristics" ### metros  
iron_baseline_surf <- "dfe_baseline_2000_2018_depthsurf" ###
nitrate_baseline_surf <- "no3_baseline_2000_2018_depthsurf" ###
oxygen_baseline_surf <- "o2_baseline_2000_2018_depthsurf" ###
ph_baseline_surf <- "ph_baseline_2000_2018_depthsurf" ###
phosphate_baseline_surf <- "po4_baseline_2000_2018_depthsurf" ###
silicate_baseline_surf <- "si_baseline_2000_2018_depthsurf" ###
  
time_bathy = c('1970-01-01T00:00:00Z', '1970-01-01T00:00:00Z')
time = c('2000-01-01T00:00:00Z', '2000-01-01T00:00:00Z')
latitude = c(-89.975, 20)
longitude = c(-100, 20)
  
constraints_bathy = list(time_bathy, latitude, longitude)
constraints = list(time, latitude, longitude)
names(constraints) = c("time", "latitude", "longitude")
names(constraints_bathy) = c("time", "latitude", "longitude")
  
variables_tas_baseline_surf = c("tas_mean")
variables_chl_baseline_surf = c("chl_mean")
variables_mld_baseline_surf = c("mlotst_mean")
variables_tsm_baseline_surf = c("thetao_mean")
variables_sal_baseline_surf = c("so_mean")
variables_swd_baseline_surf = c("swd_mean")
variables_sws_baseline_surf = c("sws_mean")
variables_produt_baseline_surf = c("phyc_mean")
variables_bathy_baseline = c("bathymetry_mean")
variables_iron_baseline_surf = c("dfe_mean")
variables_nitrate_baseline_surf = c("no3_mean")
variables_oxygen_baseline_surf = c("o2_mean")
variables_ph_baseline_surf = c("ph_mean")
variables_phosphate_baseline_surf = c("po4_mean")
variables_silicate_baseline_surf = c("si_mean")
  
tas_baseline_surf_2000_2010 <- download_layers(tas_baseline_surf, variables_tas_baseline_surf, constraints)
chl_baseline_surf_2000_2010 <- download_layers(chl_baseline_surf, variables_chl_baseline_surf, constraints)
mld_baseline_surf_2000_2010 <- download_layers(mld_baseline_surf, variables_mld_baseline_surf, constraints)
tsm_baseline_surf_2000_2010 <- download_layers(tsm_baseline_surf, variables_tsm_baseline_surf, constraints)
sal_baseline_surf_2000_2010 <- download_layers(sal_baseline_surf, variables_sal_baseline_surf, constraints)
swd_baseline_surf_2000_2010 <- download_layers(swd_baseline_surf, variables_swd_baseline_surf, constraints)
sws_baseline_surf_2000_2010 <- download_layers(sws_baseline_surf, variables_sws_baseline_surf, constraints)
produt_baseline_surf_2000_2010 <- download_layers(produt_baseline_surf, variables_produt_baseline_surf, constraints)
bathy_baseline_2000_2010 <- download_layers(bathy_baseline, variables_bathy_baseline, constraints_bathy)
iron_baseline_2000_2010 <- download_layers(iron_baseline_surf, variables_iron_baseline_surf, constraints)
nitrate_baseline_2000_2010 <- download_layers(nitrate_baseline_surf, variables_nitrate_baseline_surf, constraints)
oxygen_baseline_2000_2010 <- download_layers(oxygen_baseline_surf, variables_oxygen_baseline_surf, constraints)
ph_baseline_2000_2010 <- download_layers(ph_baseline_surf, variables_ph_baseline_surf, constraints)
phosphate_baseline_2000_2010 <- download_layers(phosphate_baseline_surf, variables_phosphate_baseline_surf, constraints)
silicate_baseline_2000_2010 <- download_layers(silicate_baseline_surf, variables_silicate_baseline_surf, constraints)

# Criar RasterLayer a partir dos SpatRaster
chl_surf_raster <- raster(chl_baseline_surf_2000_2010)
mld_surf_raster <- raster(mld_baseline_surf_2000_2010)
tsm_surf_raster <- raster(tsm_baseline_surf_2000_2010)
sal_surf_raster <- raster(sal_baseline_surf_2000_2010)
swd_surf_raster <- raster(swd_baseline_surf_2000_2010)
sws_surf_raster <- raster(sws_baseline_surf_2000_2010)
produt_surf_raster <- raster(produt_baseline_surf_2000_2010)
bathy_raster <- raster(bathy_baseline_2000_2010)
iron_surf_raster <- raster(iron_baseline_2000_2010)
nitrate_surf_raster <- raster(nitrate_baseline_2000_2010)
oxygen_surf_raster <- raster(oxygen_baseline_2000_2010)
ph_surf_raster <- raster(ph_baseline_2000_2010)
phosphate_surf_raster <- raster(phosphate_baseline_2000_2010)
silicate_surf_raster <- raster(silicate_baseline_2000_2010)
tas_surf_raster <- raster(tas_baseline_surf_2000_2010)

# Empilhar os RasterLayer em um RasterStack
bio <- stack(chl_surf_raster, mld_surf_raster, tsm_surf_raster, sal_surf_raster, swd_surf_raster, sws_surf_raster, 
             bathy_raster, iron_surf_raster, nitrate_surf_raster, phosphate_surf_raster, silicate_surf_raster)

print(bio)

plot(bio)

################################################################################
################################################################################

bio <- crop(bio, oceans_cropped) # recorte da área de estudo
bio <- mask(bio, oceans_cropped) # máscara fora da área de estudo

names(bio)

#bio <- projectRaster(bio, crs = crs(oceans)) # reprojetar

################################################################################
################################################################################

### Tratamento das variáveis -----

australis_coords <- dados_australis[, c("Long", "Lat")]

australis <- raster::extract(bio, australis_coords)

summary(australis)
str(australis)

australis_df <- as.data.frame(australis)

str(australis_df)
colnames(australis_df)

australis_full <- cbind(dados_australis, australis_df)

str(australis_full)
colnames(australis_full)

write.csv(australis_full,
          file = "australis_full_com_NAs.csv",
          row.names = FALSE)

australis_sem_NAs <- australis_full[complete.cases(australis_full), ]

str(australis_sem_NAs)
summary(australis_sem_NAs)

write.csv(australis_sem_NAs,
          file = "australis_full_sem_NAs.csv",
          row.names = FALSE)

write_xlsx(
  australis_sem_NAs,
  "australis_sem_NAs.xlsx"
)

################################################################################
################################################################################

# Plotar Shapefile recortado
plot(oceans_cropped, col = "lightblue")
plot(eez_cropped, add=TRUE)

# Adicionar eixos y
valores_y <- c(20, 10, 0, -10, -20, -30, -40, -50, -60, -70, -80, -90)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-100, -90, -80, -70, -60, -50, -40, -30, -20)
axis(1, at = valores_x)

cores <- ifelse(australis_sem_NAs$Species == "Aaustralis",
                "red",
                "blue")

points(australis_sem_NAs$Long, australis_sem_NAs$Lat,
       pch = 19,
       col = cores,
       cex = 0.7)

################################################################################
################################################################################

library(spThin)    # Realiza o "thinning" espacial, reduzindo a autocorrelação espacial em dados de ocorrência

### Espacialização geográfica -----
australis_thin <- thin(
  loc.data = australis_sem_NAs,                 # Dataframe de ocorrências filtrado
  lat.col = "Lat",                              # Coluna com latitude
  long.col = "Long",                            # Coluna com longitude
  spec.col = "Species",                         # Coluna com o nome da espécie
  thin.par = 100,                               # Distância mínima (km) entre pontos
  reps = 100,                                   # Quantas vezes repetir o processo
  locs.thinned.list.return = TRUE,              # Retorna lista com resultados de cada repetição
  write.files = FALSE,                          # Não salva arquivos automaticamente
  write.log.file = FALSE                        # Não cria arquivo de log
)

# Número de pontos em cada repetição
n_locs <- sapply(australis_thin, nrow)

# Repetição com maior número de ocorrências
australis_thin <- australis_thin[[which.max(n_locs)]]

nrow(australis_thin)                                             # Mostra número de registros
australis_thin                                                   # Visualiza tabela final
str(australis_thin)

write_xlsx(
  australis_thin,
  "australis_thin.xlsx"
)

################################################################################
################################################################################

australis_thin_full <- read_xlsx("australis_sem_NAs.xlsx")

head(australis_thin_full)
str(australis_thin_full)

australis_colin <- australis_sem_NAs %>%
  dplyr::select(-tas_mean, -phyc_mean, -o2_mean, -ph_mean)

pairs.panels(
  australis_colin,
  cex = 5,        # tamanho geral da fonte (números, correlações)
  cex.labels = 1.6 # tamanho dos nomes das variáveis
)

australis_thin_full = australis_colin 

################################################################################
################################################################################

# Plotar Shapefile recortado
plot(oceans_cropped, col = "lightblue")
plot(eez_cropped, add=TRUE)

# Adicionar eixos y
valores_y <- c(20, 10, 0, -10, -20, -30, -40, -50, -60, -70, -80, -90)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-100, -90, -80, -70, -60, -50, -40, -30, -20)
axis(1, at = valores_x)

cores <- ifelse(australis_thin_full$Species == "Aaustralis",
                "red",
                "blue")

points(australis_thin_full$Long, australis_thin_full$Lat,
       pch = 19,
       col = cores,
       cex = 0.7)

################################################################################
################################################################################

australis_all <- read_xlsx("australis_thin_presenca_sem_brasil.xlsx")

summary(australis_all)
str(australis_all)

################################################################################
################################################################################

# Plotar Shapefile recortado
plot(oceans_cropped, col = "lightblue")
plot(eez_cropped, add=TRUE)

# Adicionar eixos y
valores_y <- c(20, 10, 0, -10, -20, -30, -40, -50, -60, -70, -80, -90)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-100, -90, -80, -70, -60, -50, -40, -30, -20)
axis(1, at = valores_x)

cores <- ifelse(australis_all$Species == "Aaustralis",
                "red",
                "blue")

points(australis_all$Long, australis_all$Lat,
       pch = 19,
       col = cores,
       cex = 0.7)

################################################################################
################################################################################

# Converter o mapa recortado para sf

australis_pres_sf <- st_as_sf(
  australis_all,
  coords = c("Long", "Lat"),
  crs = 4326
)

oceans_sf <- st_as_sf(oceans_cropped)

################################################################################
################################################################################

# Sortear pontos candidatos (mais do que 28!)

set.seed(123)

candidatos <- st_sample(
  oceans_sf,
  size = 2000,        # quanto maior, melhor
  type = "random"
)

candidatos_sf <- st_as_sf(candidatos)

################################################################################
################################################################################

# Calcular distância até os pontos de presença

dist_matrix <- st_distance(candidatos_sf, australis_pres_sf)

# distância mínima de cada candidato até qualquer presença
dist_min <- apply(dist_matrix, 1, min)

# Filtrar pontos com distância ≥ 4°
ausencias_sf <- candidatos_sf[dist_min >= 4, ]

# Selecionar exatamente 30 pontos
if (nrow(ausencias_sf) < 30) {
  stop("Poucos pontos disponíveis. Aumente o número de candidatos.")
}

ausencias_sf <- ausencias_sf %>%
  slice_sample(n = 30)

################################################################################
################################################################################

# Visualização final (checagem)
plot(oceans_cropped, col = "lightblue")
plot(eez_cropped, add = TRUE)

plot(st_geometry(australis_pres_sf), add = TRUE, col = "blue", pch = 16)
plot(st_geometry(ausencias_sf), add = TRUE, col = "red", pch = 16)

legend(
  "bottomleft",
  legend = c("Presença", "Ausência"),
  col = c("blue", "red"),
  pch = c(16, 16),
  bty = "n"
)

################################################################################
################################################################################

# Extrair latitude e longitude
ausencias_df <- ausencias_sf %>%
  st_coordinates() %>%
  as.data.frame()

colnames(ausencias_df) <- c("Long", "Lat")

################################################################################
################################################################################

australis_ausencias <- raster::extract(bio, ausencias_sf)

summary(australis_ausencias)
str(australis_ausencias)

australis_ausencias_df <- as.data.frame(australis_ausencias)

str(australis_ausencias_df)
colnames(australis_ausencias_df)

################################################################################
################################################################################

write_xlsx(
  australis_ausencias_df,
  path = "australis_ausencias_28_pontos.xlsx"
)

write_xlsx(
  ausencias_df,
  path = "australis_ausencias_coords.xlsx"
)

################################################################################
################################################################################

australis_ausencias <- read_xlsx("australis_ausencias_28_pontos.xlsx")

summary(australis_ausencias)
str(australis_ausencias)

################################################################################
################################################################################

# Plotar Shapefile recortado
plot(oceans_cropped, col = "lightblue")
plot(eez_cropped, add=TRUE)

# Adicionar eixos y
valores_y <- c(20, 10, 0, -10, -20, -30, -40, -50, -60, -70, -80, -90)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-100, -90, -80, -70, -60, -50, -40, -30, -20)
axis(1, at = valores_x)

# Pontos de presença
plot(
  st_geometry(australis_pres_sf),
  add = TRUE,
  col = "blue",
  pch = 16
)

# Pontos de ausência
points(
  australis_ausencias$Long,
  australis_ausencias$Lat,
  col = "red",
  pch = 16
)

# Legenda
legend(
  "bottomleft",
  legend = c("Presença", "Ausência"),
  col = c("blue", "red"),
  pch = c(16, 16),
  bty = "n"
)

################################################################################
################################################################################

#library(devtools)
#devtools::install_github("biomodhub/biomod2", dependencies = TRUE)

library(biomod2)

################################################################################
################################################################################

# Dados de presença / ausência -----

australis_pres_aus <- read_xlsx("1_dados_presenca_ausencia.xlsx")

# Garantir data.frame (não tibble)
australis_pres_aus <- as.data.frame(australis_pres_aus)

# Checagens básicas
str(australis_pres_aus)
summary(australis_pres_aus)

# Nome da espécie
myRespName <- "sp_cod"

# Coordenadas (data.frame simples)
myRespXY <- australis_pres_aus[, c("Long", "Lat")]

################################################################################

# Dados ambientais -----

# Converter Brick → Stack (ok)
bio_cropped <- raster::stack(bio)

# Garantir CRS
crs(bio_cropped) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

################################################################################

# Formatação BIOMOD (com ausências reais) -----

myBiomodData <- BIOMOD_FormatingData(
  resp.var  = myResp,
  expl.var  = bio_cropped,
  resp.xy   = myRespXY,
  resp.name = myRespName
)

myBiomodData
myBiomodData@coord
head(myBiomodData@data.env.var)
plot(myBiomodData)

# Carregue o pacote openxlsx
library(openxlsx)

# Extraia os dados de myBiomodData
data_env_var <- myBiomodData@data.env.var

# Salve os dados em um arquivo .xlsx
write.xlsx(data_env_var, file = "australis_env_var.xlsx")

################################################################################
################################################################################

# k-fold selection
cv.k <- bm_CrossValidation(bm.format = myBiomodData,
                           strategy = 'kfold',
                           nb.rep = 2,
                           k = 3)

allModels <- c('GAM', 'GBM', 'GLM', 'RF', 'XGBOOST')

a_australis_opt <- bm_ModelingOptions(
  data.type = 'binary',
  models = allModels,
  strategy = 'default',
  bm.format = myBiomodData
)

a_australis_model <- BIOMOD_Modeling(
  bm.format    = myBiomodData,
  modeling.id = 'AllModels',
  models      = c('GAM','GBM','GLM','RF','XGBOOST'),
  CV.strategy = 'random',
  CV.nb.rep   = 10,
  CV.perc     = 0.8,
  OPT.strategy = 'default',
  metric.eval = c('TSS','AUCroc'),
  var.import  = 5,
  seed.val    = 42
)

a_australis_model <- BIOMOD_Modeling(
  bm.format    = myBiomodData,
  modeling.id = "AllModels",
  models      = c("GBM", "GAM", "RF", "XGBOOST"),
  OPT.user    = a_australis_opt,
  CV.strategy = "random",
  CV.nb.rep   = 10,
  CV.perc     = 0.7,
  var.import  = 3,
  nb.cpu      = 2,
  seed.val    = 123,
  metric.eval = c("TSS", "ROC")
)

################################################################################
################################################################################

# Model single models
a_australis_model <- BIOMOD_Modeling(
  bm.format    = myBiomodData,
  modeling.id = 'AllModels',
  models      = c("GBM", "GAM", "RF", "XGBOOST"),
  OPT.user    = a_australis_opt,   # 👈 NOME CORRETO
  CV.strategy = 'random',
  CV.nb.rep   = 10,
  CV.perc     = 0.7,
  var.import  = 3,
  nb.cpu      = 2,
  seed.val    = 123,
  metric.eval = c('TSS', 'ROC')
)

a_tropicalis_model_scores <- get_evaluations(a_tropicalis_model)
dim(a_tropicalis_model_scores)
dimnames(a_tropicalis_model_scores)

# Get evaluation scores & variables importance
bm_PlotEvalMean(
  a_tropicalis_model,
  metric.eval = c('TSS','ROC'),
  dataset = "validation",
  group.by = "algo",
  do.plot = TRUE
)

a_tropicalis_model_var_imp <- get_variables_importance(a_tropicalis_model)

# Calcula a média da importância das variáveis pelas colunas "expl.var"
mean_var_imp <- aggregate(var.imp ~ expl.var, data = a_tropicalis_model_var_imp, FUN = mean)

# Exibe o resultado
print(mean_var_imp)

# Ordena o dataframe mean_var_imp do mais importante para o menos importante
mean_var_imp <- mean_var_imp[order(mean_var_imp$var.imp, decreasing = TRUE), ]

# Gera o gráfico de barras invertido
barplot(mean_var_imp$var.imp, names.arg = mean_var_imp$expl.var, 
        xlab = "Importância", ylab = "Variáveis Explicativas",
        col = "blue")







# Criar fórmula explicitamente
myFormula <- bm_MakeFormula(
  resp.name = myRespName,
  expl.var  = get_formal_data(myBiomodData, "expl.var")
)
