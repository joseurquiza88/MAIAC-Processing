######################################################################

#Merge por dia de los buffers de MODIS diarios con los diferentes horarios
city = "SL"
number = "5"
buffer = "25"
for (i in 1:1){
 
  data_30 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/USA/dia/",buffer,"km/",number,"_",city,"-",buffer,"km-modis-30-AER.csv",sep=""))
  data_60 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/USA/dia/",buffer,"km/",number,"_",city,"-",buffer,"km-modis-60-AER.csv",sep=""))
  data_90 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/USA/dia/",buffer,"km/",number,"_",city,"-",buffer,"km-modis-90-AER.csv",sep=""))
  data_120 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/USA/dia/",buffer,"km/",number,"_",city,"-",buffer,"km-modis-120-AER.csv",sep=""))
  
  # Elimino las columnas que no son de interes y cambio nombres de columnas
  data_30 <- data.frame(date = data_30$date,
                        AOD_550_modis_mean_30 = data_30$AOD_550_maiac_mean,
                        AOD_550_AER_mean_30 = data_30$AOD_550_AER_mean)
  
  data_60 <- data.frame(date = data_60$date,
                        AOD_550_modis_mean_60 = data_60$AOD_550_maiac_mean,
                        AOD_550_AER_mean_60 = data_60$AOD_550_AER_mean)
  
  data_90 <- data.frame(date = data_90$date,
                        AOD_550_modis_mean_90 = data_90$AOD_550_maiac_mean,
                        AOD_550_AER_mean_90 = data_90$AOD_550_AER_mean)
  
  data_120 <- data.frame(date = data_120$date,
                         AOD_550_modis_mean_120 = data_120$AOD_550_maiac_mean,
                         AOD_550_AER_mean_120 = data_120$AOD_550_AER_mean)
  # Convertir la columna de fecha a tipo Date si aún no lo es
  data_30$date <- as.Date(data_30$date,format="%Y-%m-%d")#"%d/%m/%Y"
  data_60$date <- as.Date(data_60$date,format="%Y-%m-%d")
  data_90$date <- as.Date(data_90$date,format="%Y-%m-%d")
  data_120$date <- as.Date(data_120$date,format="%Y-%m-%d")
  
  
  
  # Realizar el merge entre los cuatro data frames basándote en la columna de fecha
  resultado <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), list(data_30, data_60, data_90, data_120))
  
  # Mostrar el resultado
  #print(resultado)
  #dir <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_BUFFER/Latam_C60/horario/"
  dir <- paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_BUFFER_MODIS/USA/horario/",buffer,"km/",sep="")
  
  city_name <- paste(number,"-", city,"-",buffer, "km-MODIS-AER_MEAN-TotBuff.csv",sep = "")
  name <- paste(dir,city_name, sep="")
  write.csv(resultado,name)
}
rm(list = ls())
########################################################################
########################################################################

#Merge de los datasets con las mismas horas pero con distintos buffers de MAIAC diarios con los diferentes horarios
city = "SL"
number = "5"
buffer = "120"
for (i in 1:1){
  data_3 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/USA/dia/3km/",number,"_",city,"-","3km-modis-",buffer,"-AER.csv",sep=""))
  data_5 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/USA/dia/5km/",number,"_",city,"-","5km-modis-",buffer,"-AER.csv",sep=""))
  data_15 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/USA/dia/15km/",number,"_",city,"-","15km-modis-",buffer,"-AER.csv",sep=""))
  data_25 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/USA/dia/25km/",number,"_",city,"-","25km-modis-",buffer,"-AER.csv",sep=""))
  
  # Elimino las columnas que no son de interes y cambio nombres de columnas
  
  data_3 <- data.frame(date = data_3$date,
                       AOD_550_modis_mean_3 = data_3$AOD_550_maiac_mean,
                       AOD_550_AER_mean_3 = data_3$AOD_550_AER_mean)
  
  data_5 <- data.frame(date = data_5$date,
                       AOD_550_modis_mean_5 = data_5$AOD_550_maiac_mean,
                       AOD_550_AER_mean_5 = data_5$AOD_550_AER_mean)
  
  data_15 <- data.frame(date = data_15$date,
                        AOD_550_modis_mean_15 = data_15$AOD_550_maiac_mean,
                        AOD_550_AER_mean_15 = data_15$AOD_550_AER_mean)
  data_25 <- data.frame(date = data_25$date,
                        AOD_550_modis_mean_25 = data_25$AOD_550_maiac_mean,
                        AOD_550_AER_mean_25 = data_25$AOD_550_AER_mean)
  # Convertir la columna de fecha a tipo Date si aún no lo es
  data_3$date <- as.Date(data_3$date)
  data_5$date <- as.Date(data_5$date)
  data_15$date <- as.Date(data_15$date)
  data_25$date <- as.Date(data_25$date)
  
  
  
  # Realizar el merge entre los cuatro data frames basándote en la columna de fecha
  resultado <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), list(data_3, data_5, data_15, data_25))
  
  # Mostrar el resultado
  #print(resultado)
  # dir <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_BUFFER/Latam_C60/espacial/"
  # city_name <- paste(number,"-", city,"-",buffer, "-MAIAC-AER_MEAN-TotBuff_c60.csv",sep = "")
  dir <- paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_BUFFER_MODIS/USA/espacial/",buffer,"/",sep="")
  city_name <- paste(number,"-", city,"-",buffer, "-MODIS-AER_MEAN-TotBuff.csv",sep = "")
  
  name <- paste(dir,city_name, sep="")
  write.csv(resultado,name)
}
rm(list = ls())
