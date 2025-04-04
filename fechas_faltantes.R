
library(dplyr)
# Leo el archivo
data <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/modis/Latinoamerica/SP/SP-3KM-MODIS.csv")
#data <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/modis/Latinoamerica/N/SP/MOD/prueba_3km_SP_MOD_tot.csv")
# aeromet
data <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/datasets_interp_s_L02/Latam/1_SP_2015-2022_interp-s_V02_L2.csv")
data <- data[complete.cases(data$AOD), ]
# Me quedo solo con la columna dia y genero un dataframe
fecha_modis <- data.frame(fecha=data$dia, num=1)
fecha_modis <- data.frame(fecha=data$date, num=1)
#Elimino los nans, y solo me quedo con los que si tienen fecha
fecha_modis <- fecha_modis[complete.cases(fecha_modis$fecha), ]

#Convierto el dato en tipo date
fecha_modis$fecha <- strptime(fecha_modis$fecha, format="%d/%m/%Y") 
fecha_modis$fecha <- strptime(fecha_modis$fecha, format="%Y-%m-%d %H:%M:%M") 
fecha_modis$fecha <- substr(fecha_modis$fecha,1,10)
fecha_modis%>%
  dplyr::group_by(fecha) %>%  
  dplyr::group_split() -> dat_agrupado
df_rbind <- data.frame()
for (x in 1:length(dat_agrupado)){
  df <- data.frame(fecha = dat_agrupado[[x]][["fecha"]][1],
                   num = dat_agrupado[[x]][["num"]][1] )
  df_rbind <- rbind(df_rbind,df)
}
fecha_modis<-df_rbind 
fecha_modis$fecha <- strptime(fecha_modis$fecha, format="%Y-%m-%d") 
#write.csv(fecha_modis,"D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/datasets_interp_s_L02/USA/2_GT_2015-2022_interp-s_V02_L2_recortado.csv")

# Crear vector de fechas desde 01-12-2015 al 31-12-2022
fecha_total <- data.frame(fechas=seq(as.Date("2015-01-01"), as.Date("2022-12-31"), by="day"))
#Convierto el dato en tipo date
fecha_total$fecha <- strptime(fecha_total$fechas, format="%Y-%m-%d") 
#Uno los dos dataframe por la columna fecha, 
#En los dos df se tienen que llamar igual
df_completo <- merge(fecha_total,fecha_modis,  by = "fecha", all.x = TRUE)  # O usando dplyr::left_join(df_fechas, df_original, by = "fecha")
#Modifico los nombres de las columnas para mejor entendimiento
names(df_completo) <- c("fecha_total","fecha_modis","fecha_existente")
#Me quedo con los que tienen valor nulo en la columa num que corresponde a los
#datos faltante
fechas_faltantes <- df_completo[is.na(df_completo$fecha_existente), ]
# Como los tengo que buscar en otro dataset, voy a filtrar el a�o interes
# Primero genero columna con el a�o
#Ten en cuenta que el componente year en la estructura "POSIXlt" 
#representa el a�o desde 1900. Por lo tanto, sumamos 1900 al valor de 
#fecha_posixlt$year para obtener el a�o actual.
fechas_faltantes$year <- fechas_faltantes$fecha_total$year+1900
interest_year = 2015

fechas_faltantes_interes <- fechas_faltantes[fechas_faltantes$year == interest_year,]


###############
(nrow(fechas_faltantes)/nrow(df_completo))*100

