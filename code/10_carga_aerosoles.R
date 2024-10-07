

#21-09-2023

#Objetivo dividir los dataframe de las ciudades latinoamericanas segun la carga 
# de aersoloes como: 
# - carga de aerosoles baja (AERONET =<0.2 donde la contribución de la reflectancia superficial 
# a la reflectancia en la parte superior de la atmósfera (TOA) es considerable
# - aerosoles moderada 0.2 < AERONET ???0.4 donde la reflectancia en TOA se ve afectada 
#tanto por la reflectancia superficial como por las propiedades ópticas y físicas de los aerosoles
# - aerosoles alta AERONET >0.4 donde se espera que la contribución de la reflectancia 
#superficial a la reflectancia en TOA sea pequeña, pero la incertidumbre asociada a la 
#selección de las propiedades de los aerosoles (tipo y fracción de tamaño) es alta.

#0.
dire <- "D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61-AER-MOD/" 
# Local path where the .HDF files are located
id <- dir(dire, pattern = ".csv")
#Important: be located in the path where the files are located
setwd(dire)
df_rbind <- data.frame()
for (i in 1: length(id)){
  print(i)
    
  # 01. Leemos archivo
  # vamos a tomar el archivo donde esta la C6.1-6.0 y MODIS
  data <- read.csv(id[i])
  
  # 02. Categorizamos el AOD Aeronet
  #La clasificacion es segun AOD AERONET
  data_02 <- data[data$AOD_550_AER_mean <= 0.2, ]
  data_04 <- data[data$AOD_550_AER_mean > 0.2 & data$AOD_550_AER_mean <= 0.4, ]
  data_06 <- data[data$AOD_550_AER_mean > 0.4, ]
  
  # Si solo una observacion ponemos NA en todo el df
  if (nrow(data_02) <= 2){
    data_02_estadisticas_modis <- data.frame (mean.modelado = NA,mean.obs = NA, len.obs = nrow(data_02),
                                              len.mod= nrow(data_02), rmse = NA,nrmse.mean = NA,bias=NA,
                                              nbias.mean=NA, r=NA, R.2.aj=NA, sd_mod =NA, sd_obs=NA, crmsd=NA,
                                              intercep=NA, slope=NA, name="modis02")  
    data_02_estadisticas_maiac6 <- data.frame (mean.modelado = NA,mean.obs = NA, len.obs = nrow(data_02),
                                              len.mod= nrow(data_02), rmse = NA,nrmse.mean = NA,bias=NA,
                                              nbias.mean=NA, r=NA, R.2.aj=NA, sd_mod =NA, sd_obs=NA, crmsd=NA,
                                              intercep=NA, slope=NA, name="maiac6-02") 
    data_02_estadisticas_maiac61 <- data.frame (mean.modelado = NA,mean.obs = NA, len.obs = nrow(data_02),
                                                len.mod= nrow(data_02), rmse = NA,nrmse.mean = NA,bias=NA,
                                                nbias.mean=NA, r=NA, R.2.aj=NA, sd_mod =NA, sd_obs=NA, crmsd=NA,
                                                intercep=NA, slope=NA, name="maiac61-02") 
    
  }
    
  else if (nrow(data_04) <= 2){
    data_04_estadisticas_modis <- data.frame (mean.modelado = NA,mean.obs = NA, len.obs = nrow(data_04),
                                              len.mod= nrow(data_04), rmse = NA,nrmse.mean = NA,bias=NA,
                                              nbias.mean=NA, r=NA, R.2.aj=NA, sd_mod =NA, sd_obs=NA, crmsd=NA,
                                              intercep=NA, slope=NA, name="modis04")  
    data_04_estadisticas_maiac6 <- data.frame (mean.modelado = NA,mean.obs = NA, len.obs = nrow(data_04),
                                               len.mod= nrow(data_04), rmse = NA,nrmse.mean = NA,bias=NA,
                                               nbias.mean=NA, r=NA, R.2.aj=NA, sd_mod =NA, sd_obs=NA, crmsd=NA,
                                               intercep=NA, slope=NA, name="maiac6-04") 
    data_04_estadisticas_maiac61 <- data.frame (mean.modelado = NA,mean.obs = NA, len.obs = nrow(data_04),
                                                len.mod= nrow(data_04), rmse = NA,nrmse.mean = NA,bias=NA,
                                                nbias.mean=NA, r=NA, R.2.aj=NA, sd_mod =NA, sd_obs=NA, crmsd=NA,
                                                intercep=NA, slope=NA, name="maiac61-04") 
    
  }
  
  else if (nrow(data_06) <= 2){
    data_06_estadisticas_modis <- data.frame (mean.modelado = NA,mean.obs = NA, len.obs = nrow(data_06),
                                              len.mod= nrow(data_06), rmse = NA,nrmse.mean = NA,bias=NA,
                                              nbias.mean=NA, r=NA, R.2.aj=NA, sd_mod =NA, sd_obs=NA, crmsd=NA,
                                              intercep=NA, slope=NA, name="modis06")  
    data_06_estadisticas_maiac6 <- data.frame (mean.modelado = NA,mean.obs = NA, len.obs = nrow(data_06),
                                               len.mod= nrow(data_06), rmse = NA,nrmse.mean = NA,bias=NA,
                                               nbias.mean=NA, r=NA, R.2.aj=NA, sd_mod =NA, sd_obs=NA, crmsd=NA,
                                               intercep=NA, slope=NA, name="maiac6-06") 
    data_06_estadisticas_maiac61 <- data.frame (mean.modelado = NA,mean.obs = NA, len.obs = nrow(data_06),
                                                len.mod= nrow(data_06), rmse = NA,nrmse.mean = NA,bias=NA,
                                                nbias.mean=NA, r=NA, R.2.aj=NA, sd_mod =NA, sd_obs=NA, crmsd=NA,
                                                intercep=NA, slope=NA, name="maiac61-06") 
    
  }
  else{
    
    # 03. Generamos metricas
    #Modis
    data_02_estadisticas_modis <- data.frame(funcion_estaditicas(m=data_02$AOD_modis,data_02$AOD_550_AER_mean, tipo="tabla"))
    data_04_estadisticas_modis <- data.frame(funcion_estaditicas(m=data_04$AOD_modis,data_04$AOD_550_AER_mean, tipo="tabla"))
    data_06_estadisticas_modis <- data.frame(funcion_estaditicas(m=data_06$AOD_modis,data_06$AOD_550_AER_mean, tipo="tabla"))
    # nombre de las estadisticas
    data_02_estadisticas_modis$name <- "modis02"
    data_04_estadisticas_modis$name <- "modis04"
    data_06_estadisticas_modis$name <- "modis06"
    
    #MAIAC C6.0
    data_02_estadisticas_maiac6 <- data.frame(funcion_estaditicas(m=data_02$AOD_maiac_60,data_02$AOD_550_AER_mean, tipo="tabla"))
    data_04_estadisticas_maiac6 <- data.frame(funcion_estaditicas(m=data_04$AOD_maiac_60,data_04$AOD_550_AER_mean, tipo="tabla"))
    data_06_estadisticas_maiac6 <- data.frame(funcion_estaditicas(m=data_06$AOD_maiac_60,data_06$AOD_550_AER_mean, tipo="tabla"))
    # nombre de las estadisticas
    data_02_estadisticas_maiac6$name <- "maiac6-02"
    data_04_estadisticas_maiac6$name <- "maiac6-04"
    data_06_estadisticas_maiac6$name <- "maiac6-06"
    
    #MAIAC C6.1
    data_02_estadisticas_maiac61 <- data.frame(funcion_estaditicas(m=data_02$AOD_maiac_61,data_02$AOD_550_AER_mean, tipo="tabla"))
    data_04_estadisticas_maiac61 <- data.frame(funcion_estaditicas(m=data_04$AOD_maiac_61,data_04$AOD_550_AER_mean, tipo="tabla"))
    data_06_estadisticas_maiac61 <- data.frame(funcion_estaditicas(m=data_06$AOD_maiac_61,data_06$AOD_550_AER_mean, tipo="tabla"))
    # nombre de las estadisticas
    data_02_estadisticas_maiac61$name <- "maiac61-02"
    data_04_estadisticas_maiac61$name <- "maiac61-04"
    data_06_estadisticas_maiac61$name <- "maiac61-06"
  }
  # 04. Generamos tabla con todas las estadisticas
  
  df_ciudad <- rbind(data_02_estadisticas_modis, data_04_estadisticas_modis, data_06_estadisticas_modis,
                  data_02_estadisticas_maiac6, data_04_estadisticas_maiac6, data_06_estadisticas_maiac6,
                  data_02_estadisticas_maiac61, data_04_estadisticas_maiac61, data_06_estadisticas_maiac61)
  
  
  df_ciudad$ciudad <- substr(id[i],1,26)
  df_rbind <- rbind(df_rbind,df_ciudad)
}


# Guardamos csv para visualizarlo mejor
write.csv(df_rbind,"D:/Josefina/paper_git/paper_maiac/plot/estadisticas/estadisticas_M6M61-MOD-AER-carga.csv")




