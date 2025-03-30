
######################################################################

#Merge por dia de los buffers de MAIAC diarios con los diferentes horarios
rm(list = ls())
city = "NY"
number = "7"
buffer = "25"
for (i in 1:1){
  # data_30 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C60/dia/",buffer,"km/",number,"_",city,"-",buffer,"km-MAIAC-30-AER_MEAN_c60.csv",sep=""))
  # data_60 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C60/dia/",buffer,"km/",number,"_",city,"-",buffer,"km-MAIAC-60-AER_MEAN_c60.csv",sep=""))
  # data_90 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C60/dia/",buffer,"km/",number,"_",city,"-",buffer,"km-MAIAC-90-AER_MEAN_c60.csv",sep=""))
  # data_120 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C60/dia/",buffer,"km/",number,"_",city,"-",buffer,"km-MAIAC-120-AER_MEAN_c60.csv",sep=""))
  
  data_30 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/USA_C61/dia/",buffer,"km/",number,"_",city,"-",buffer,"km-MAIAC-30-AER_MEAN_c61.csv",sep=""))
  data_60 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/USA_C61/dia/",buffer,"km/",number,"_",city,"-",buffer,"km-MAIAC-60-AER_MEAN_c61.csv",sep=""))
  data_90 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/USA_C61/dia/",buffer,"km/",number,"_",city,"-",buffer,"km-MAIAC-90-AER_MEAN_c61.csv",sep=""))
  data_120 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/USA_C61/dia/",buffer,"km/",number,"_",city,"-",buffer,"km-MAIAC-120-AER_MEAN_c61.csv",sep=""))
  
  # Elimino las columnas que no son de interes y cambio nombres de columnas
  data_30 <- data.frame(date = data_30$date,
                        AOD_550_maiac_mean_30 = data_30$AOD_550_maiac_mean,
                        AOD_550_AER_mean_30 = data_30$AOD_550_AER_mean)
  
  data_60 <- data.frame(date = data_60$date,
                        AOD_550_maiac_mean_60 = data_60$AOD_550_maiac_mean,
                        AOD_550_AER_mean_60 = data_60$AOD_550_AER_mean)
  
  data_90 <- data.frame(date = data_90$date,
                        AOD_550_maiac_mean_90 = data_90$AOD_550_maiac_mean,
                        AOD_550_AER_mean_90 = data_90$AOD_550_AER_mean)
  
  data_120 <- data.frame(date = data_120$date,
                        AOD_550_maiac_mean_120 = data_120$AOD_550_maiac_mean,
                        AOD_550_AER_mean_120 = data_120$AOD_550_AER_mean)
  # Convertir la columna de fecha a tipo Date si a?n no lo es
  data_30$date <- as.Date(data_30$date)
  data_60$date <- as.Date(data_60$date)
  data_90$date <- as.Date(data_90$date)
  data_120$date <- as.Date(data_120$date)
  
  
  
  # Realizar el merge entre los cuatro data frames bas?ndote en la columna de fecha
  resultado <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), list(data_30, data_60, data_90, data_120))
  
  # Mostrar el resultado
  #print(resultado)
  #dir <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_BUFFER/Latam_C60/horario/"
  dir <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_BUFFER/USA_C61/horario/"
  
  city_name <- paste(number,"-", city,"-",buffer, "km-MAIAC-AER_MEAN-TotBuff_c61.csv",sep = "")
  name <- paste(dir,city_name, sep="")
  write.csv(resultado,name)
}

########################################################################
########################################################################

#Merge de los datasets con las mismas horas pero con distintos buffers de MAIAC diarios con los diferentes horarios
rm(list = ls())
city = "NY"
number = "7"
buffer = "120"
for (i in 1:1){
  # data_1 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C60/dia/1km/",number,"_",city,"-","1km-MAIAC-",buffer,"-AER_MEAN_c60.csv",sep=""))
  # data_3 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C60/dia/3km/",number,"_",city,"-","3km-MAIAC-",buffer,"-AER_MEAN_c60.csv",sep=""))
  # data_5 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C60/dia/5km/",number,"_",city,"-","5km-MAIAC-",buffer,"-AER_MEAN_c60.csv",sep=""))
  # data_15 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C60/dia/15km/",number,"_",city,"-","15km-MAIAC-",buffer,"-AER_MEAN_c60.csv",sep=""))
  # data_25 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C60/dia/25km/",number,"_",city,"-","25km-MAIAC-",buffer,"-AER_MEAN_c60.csv",sep=""))
  data_1 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/USA_C61/dia/1km/",number,"_",city,"-","1km-MAIAC-",buffer,"-AER_MEAN_c61.csv",sep=""))
  data_3 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/USA_C61/dia/3km/",number,"_",city,"-","3km-MAIAC-",buffer,"-AER_MEAN_c61.csv",sep=""))
  data_5 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/USA_C61/dia/5km/",number,"_",city,"-","5km-MAIAC-",buffer,"-AER_MEAN_c61.csv",sep=""))
  data_15 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/USA_C61/dia/15km/",number,"_",city,"-","15km-MAIAC-",buffer,"-AER_MEAN_c61.csv",sep=""))
  data_25 <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/USA_C61/dia/25km/",number,"_",city,"-","25km-MAIAC-",buffer,"-AER_MEAN_c61.csv",sep=""))
  
  # Elimino las columnas que no son de interes y cambio nombres de columnas
  data_1 <- data.frame(date = data_1$date,
                        AOD_550_maiac_mean_1 = data_1$AOD_550_maiac_mean,
                        AOD_550_AER_mean_1 = data_1$AOD_550_AER_mean)
  
  data_3 <- data.frame(date = data_3$date,
                        AOD_550_maiac_mean_3 = data_3$AOD_550_maiac_mean,
                        AOD_550_AER_mean_3 = data_3$AOD_550_AER_mean)
  
  data_5 <- data.frame(date = data_5$date,
                        AOD_550_maiac_mean_5 = data_5$AOD_550_maiac_mean,
                        AOD_550_AER_mean_5 = data_5$AOD_550_AER_mean)
  
  data_15 <- data.frame(date = data_15$date,
                         AOD_550_maiac_mean_15 = data_15$AOD_550_maiac_mean,
                         AOD_550_AER_mean_15 = data_15$AOD_550_AER_mean)
  data_25 <- data.frame(date = data_25$date,
                        AOD_550_maiac_mean_25 = data_25$AOD_550_maiac_mean,
                        AOD_550_AER_mean_25 = data_25$AOD_550_AER_mean)
  # Convertir la columna de fecha a tipo Date si a?n no lo es
  data_1$date <- as.Date(data_1$date)
  data_3$date <- as.Date(data_3$date)
  data_5$date <- as.Date(data_5$date)
  data_15$date <- as.Date(data_15$date)
  data_25$date <- as.Date(data_25$date)
  
  
  
  # Realizar el merge entre los cuatro data frames bas?ndote en la columna de fecha
  resultado <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), list(data_1, data_3, data_5, data_15, data_25))
  
  # Mostrar el resultado
  #print(resultado)
  # dir <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_BUFFER/Latam_C60/espacial/"
  # city_name <- paste(number,"-", city,"-",buffer, "-MAIAC-AER_MEAN-TotBuff_c60.csv",sep = "")
  dir <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_BUFFER/USA_C61/espacial/"
  city_name <- paste(number,"-", city,"-",buffer, "-MAIAC-AER_MEAN-TotBuff_c61.csv",sep = "")
  
  name <- paste(dir,city_name, sep="")
  write.csv(resultado,name)
}
44