#Calculo da area em km2 dos mapas de presença-ausência

# Defina o diretório onde estão os seus rasters
diretorio <- "E:/jaguar v4.0"

# Obtenha uma lista de todos os arquivos na pasta com a extensão desejada (por exemplo, .tif)
rasters <- list.files(diretorio, pattern = ".tif", full.names = TRUE)

# Crie uma lista para armazenar os rasters importados
lista_rasters <- list()

# Importe cada raster e armazene na lista
for (raster in rasters) {
  nome_raster <- basename(raster)  # Obtenha apenas o nome do raster sem o caminho
  lista_rasters[[nome_raster]] <- terra::rast(raster)  # Importe o raster e armazene na lista usando o nome do arquivo como chave
}

library(terra)

#use a função expanse do pacote terra - Compute the area covered by polygons or for all raster cells that are not NA.
# raster
# unit = km - para cálculo em km2
# byValue = T, a função irá disponibilizar separadamente a soma das células com valor 0 (ausência) e valor 1 (presença)
# transform = T, só utilize se o raster tiver em UTM ou outro CRS plano

expanse(lista_rasters$model_1.26_PA.tif, unit = "km", byValue = T)

# A unidade está em km, portanto, a função irá calcular todas as células que possuem algum valor

#Estamos interessados no valor 1, portanto:

> 1631858/1000 # em mil km²
[1] 1631.858

> 1631858/1000000
[1] 1.631858 # em milhões de km²
