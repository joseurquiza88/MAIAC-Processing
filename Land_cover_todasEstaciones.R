library(raster)
library(sp)
library(rgdal)
library(sf)
library(geosphere)

# Función para contar las categorías y organizar en un dataframe
contar_por_categoria <- function(df, columna, num_categorias,estacion,len_df) {
  
    # Crear un vector de conteo vacío con longitud igual al número de categorías
    conteos <- integer(num_categorias + 1)  # +1 para incluir la categoría 0
    
    # Obtener el conteo de frecuencias para los valores en la columna
    conteo <- table(df[[columna]])
    
    # Rellenar el vector de conteos con los valores correspondientes
    for (i in 0:num_categorias) {  # Cambiamos el rango de 0 a num_categorias
      if (as.character(i) %in% names(conteo)) {
        conteos[i + 1] <- conteo[as.character(i)]  # i + 1 porque el índice empieza en 1
      } else {
        conteos[i + 1] <- 0
      }
    }
    
    # Convertir el vector de conteos en un dataframe
    df_resultado <- as.data.frame(t(conteos))
    
    # Asignar nombres a las columnas de acuerdo a las categorías
    colnames(df_resultado) <- paste0("Categoria_", 0:num_categorias)
    df_resultado$estacion <- estacion
    df_resultado$n <- len_df
    return(df_resultado)
  }
num_categorias <- 17

directorio_tiff <- "D:/Josefina/Proyectos/LandCover/data/GeoTiff/"
# Leer el archivo TIFF
 raster_tiff <- raster(paste(directorio_tiff,"USA/combinado_USA_recortado.tif",sep=""))
#raster_tiff <- raster(paste(directorio_tiff,"SouthAmerica/Combinado_sudamerica_recortado.tif",sep=""))
#raster_tiff <- raster(paste(directorio_tiff,"CentroAmerica/CentroAmerica_recortado.tif",sep=""))

estaciones_coords <- read.csv(paste(directorio_tiff,"estaciones_coords.csv",sep=""))
estaciones_coords<- estaciones_coords[estaciones_coords$Region == "Mexico",]

# estaciones_coords<- estaciones_coords[estaciones_coords$Region == "South America",]
# estaciones_coords<- estaciones_coords[estaciones_coords$Region == "Caribbean and Mexico" |
#                                         estaciones_coords$Region == "Central America",]
# punto <- c(-84.29855475176893,30.44581679913366)
df_rbind <- data.frame()
for(i in 1:nrow(estaciones_coords)){
  print(i)
  coord_separadas <- strsplit(estaciones_coords$coords[i], ", ")
  lon <- as.numeric(coord_separadas[[1]][2])
  lat <- as.numeric(coord_separadas[[1]][1])
  punto <- c(lon,lat)
  estacion <- estaciones_coords$Estacion[i]
  # Crear el buffer de 25 km en torno al punto
  #buffer_25km <- destPoint(punto, 0:360, 25000)  # 25,000 metros = 25 km
  buffer_12km <- destPoint(punto, 0:360, 12500)  # 25,000 metros = 25 km
  
  # Convertir a un polígono SF
  # El de
  #buffer_sf <- st_as_sf(as.data.frame(buffer_25km), coords = c("lon", "lat"), crs = 4326)
  buffer_sf <- st_as_sf(as.data.frame(buffer_12km), coords = c("lon", "lat"), crs = 4326)
  
  buffer_sf <- st_convex_hull(buffer_sf)  # Crear el polígono de buffer
  # Convertir el polígono SF a Spatial para usar con raster
  buffer_sp <- as(buffer_sf, "Spatial")
  
  # Recortar el raster a la extensión del buffer
  raster_recortado <- crop(raster_tiff, buffer_sp)
  
  # Opcional: máscara para dejar valores fuera del buffer como NA
  #raster_buffer <- mask(raster_recortado, buffer_sp)
  # Convertir el raster recortado a un dataframe
  df_raster <- as.data.frame(raster_recortado, xy = TRUE, na.rm = TRUE)
  # View(df_raster)
  # write.csv(df_raster,"df_raster.csv")
  names(df_raster) <- c("x","y","categoria")
  
  len_df_raster <- nrow(df_raster)
  
  # Llamar a la función
  resultado_dataframe <- contar_por_categoria(df_raster, "categoria", num_categorias,estacion, len_df=len_df_raster)
  suma_por_filas <- rowSums(resultado_dataframe[, 1:18])
  validacion <- suma_por_filas == resultado_dataframe$n
  print(c(estacion,"validacion", validacion))
  #View(resultado_dataframe)
  
  df_rbind <- rbind (df_rbind,resultado_dataframe)

}
view(df_rbind)
setwd("D:/Josefina/Proyectos/LandCover/data/GeoTiff/USA/")
write.csv(df_rbind,"LandCover%_Mexico.csv")

