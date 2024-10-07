# Ccomparativa entre MAIAC C6.0 vs C6.1

# Hay dos formas de hacerlo. 
# La primera uniendo el timestamp, es decir los datos de fecha-hora y la otra forma
# es haciendo la media diaria
# Leemos datos de ambas versiones
data_c06 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/BA-25KM-MAIAC.csv")
data_c061 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/BA-25KM-MAIAC_C61.csv")

data_c06 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/SP-25KM-MAIAC.csv")
data_c061 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/SP-25KM-MAIAC_C61.csv")

#
data_c06 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/ST-25KM-MAIAC.csv")
data_c061 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/ST-25KM-MAIAC_C61.csv")


data_c06 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/LP-25KM-MAIAC.csv")
data_c061 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/LP-25KM-MAIAC_C61.csv")

data_c06 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/MD-25KM-MAIAC.csv")
data_c061 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/MD-25KM-MAIAC_C61.csv")

data_c06 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/MX-25KM-MAIAC.csv")
data_c061 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/MX-25KM-MAIAC_C61.csv")


data_c06$timestamp2 <- paste(data_c06$date,data_c06$hora,sep="")
data_c061$timestamp2 <- paste(data_c061$date,data_c061$hora,sep="")

data_complete_c06  <- data_c06[complete.cases(data_c06), ]
data_complete_c061  <- data_c061[complete.cases(data_c061), ]
# Hacemos un merge
data_tot <- merge(x = data_complete_c06, y = data_complete_c061, by = "timestamp2") # Equivalente
# Hacemos un merge

# cambiamos nombres
names(data_tot) <- c( "timestamp2","dat_C06","hora_C06","Year_C06", "DOY_C06",            
                      "Hour_C06" ,"Minute_C06" ,"Lat_C06","Lon_C06","AOD_Type_C06",       
                      "AOD_047_C06","AOD_055_C06","AOD_Uncertainty_C06" ,"QA_AOD_C06","timestamp_C06",  "date_C061" ,          
                      "hora_C061","Year_C061","DOY_C061", "Hour_C061","Minute_C061",         
                      "Lat_C061","Lon_C061", "AOD_Type_C061","AOD_047_C061", "AOD_055_C061",        
                      "AOD_Uncertainty_C061", "QA_AOD_C061" ,"timestamp_C01")
# formato date
data_tot$date <- as.POSIXct(strptime(data_tot$timestamp2, format = "%Y%j%H%M", "GMT"))
# ---- 01 Primera forma nos quedamos con las 3 columnas de interes
df <- data.frame(date = data_tot$date ,
                 AOD_055_C06 = data_tot$AOD_055_C06,
                 AOD_055_C061=data_tot$AOD_055_C061)
# eliminamos nans
df2  <- df[complete.cases(df), ]
# guardamos
#write.csv (df2,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_hora/BA-25KM-MAIAC_comparativa.csv")
#write.csv (df2,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_hora/SP-25KM-MAIAC_comparativa.csv")
#write.csv (df2,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_hora/ST-25KM-MAIAC_comparativa.csv")
#write.csv (df2,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_hora/LP-25KM-MAIAC_comparativa.csv")
#write.csv (df2,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_hora/4_MD-25KM-MAIAC_comparativa.csv")
write.csv (df2,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_hora/6_MX-25KM-MAIAC_comparativa.csv")


######### 
#02 forma: hacemos media diaria, en vez de hacerla en el python. Creo que esta es la mejor forma
# agrupamps por fecha
data_tot %>%
  group_by(dat_C06) %>%  
  group_split() -> data_group

# hacemos media
df_rbind <- data.frame()
for (p in 1:length(data_group)){
  df <- data.frame(dat_C06 = data_group[[p]][["dat_C06"]][1],
                   AOD_055_C06 = mean(data_group[[p]][["AOD_055_C06"]],na.rm=T),
                   AOD_055_C061 = mean(data_group[[p]][["AOD_055_C061"]],na.rm=T))
  df_rbind <- rbind(df_rbind,df)
}

df_rbind$date <- as.POSIXct(strptime(df_rbind$dat_C06, format = "%Y%j"))
# eliminamos nans
df3  <- df_rbind[complete.cases(df_rbind), ]
# guardamos
#write.csv (df3,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_dia/BA-25KM-MAIAC_comparativa_dia.csv")
#write.csv (df3,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_dia/SP-25KM-MAIAC_comparativa_dia.csv")
#write.csv (df3,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_dia/ST-25KM-MAIAC_comparativa_dia.csv")
#write.csv (df3,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_dia/LP-25KM-MAIAC_comparativa_dia.csv")
#write.csv (df3,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_dia/4_MD-25KM-MAIAC_comparativa_dia.csv")
write.csv (df3,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_dia/6_MX-25KM-MAIAC_comparativa_dia.csv")


### 
#Aqua vs terra

data_c06 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/BA-25KM-MAIAC.csv")
data_c061 <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/BA-25KM-MAIAC_C61.csv")
data_c06$timestamp2 <- paste(data_c06$date,data_c06$hora,sep="")
data_c061$timestamp2 <- paste(data_c061$date,data_c061$hora,sep="")

data_complete_c06  <- data_c06[complete.cases(data_c06), ]
data_complete_c061  <- data_c061[complete.cases(data_c061), ]
# Hacemos un merge
data_tot <- merge(x = data_complete_c06, y = data_complete_c061, by = "timestamp2") # Equivalente
# cambiamos nombres
names(data_tot) <- c( "timestamp2","dat_C06","hora_C06","Year_C06", "DOY_C06",            
                      "Hour_C06" ,"Minute_C06" ,"Lat_C06","Lon_C06","AOD_Type_C06",       
                      "AOD_047_C06","AOD_055_C06","AOD_Uncertainty_C06" ,"QA_AOD_C06","timestamp_C06",  "date_C061" ,          
                      "hora_C061","Year_C061","DOY_C061", "Hour_C061","Minute_C061",         
                      "Lat_C061","Lon_C061", "AOD_Type_C061","AOD_047_C061", "AOD_055_C061",        
                      "AOD_Uncertainty_C061", "QA_AOD_C061" ,"timestamp_C01")
data_tot$tot <- data_tot$AOD_Type_C06==data_tot$AOD_Type_C061
# formato date
data_tot$date <- as.POSIXct(strptime(data_tot$timestamp2, format = "%Y%j%H%M", "GMT"))
# ---- 01 Primera forma nos quedamos con las 3 columnas de interes
df <- data.frame(date = data_tot$date ,
                 AOD_055_C06 = data_tot$AOD_055_C06,
                 AOD_055_C061=data_tot$AOD_055_C061,
                 AOD_Type_C06=data_tot$AOD_Type_C06,
                 AOD_Type_C061=data_tot$AOD_Type_C061)

data_complete <- df[complete.cases(df), ]
data_complete$tot <- data_complete$AOD_Type_C06==data_complete$AOD_Type_C061
#cortamos terra
data_terra <- data_complete[data_complete$AOD_Type_C06 == "T",]

#cortamos aqua
data_aqua <- data_complete[data_complete$AOD_Type_C06 == "A",]
# guardamos
# write.csv (data_terra,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_sat/terra/BA-25KM-MAIAC-T_comparativa.csv")
# write.csv (data_aqua,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_sat/aqua/BA-25KM-MAIAC-A_comparativa.csv")
# write.csv (data_terra,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_sat/terra/SP-25KM-MAIAC-T_comparativa.csv")
# write.csv (data_aqua,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_sat/aqua/SP-25KM-MAIAC-A_comparativa.csv")

# write.csv (data_terra,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_sat/terra/ST-25KM-MAIAC-T_comparativa.csv")
# write.csv (data_aqua,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_sat/aqua/ST-25KM-MAIAC-A_comparativa.csv")

#write.csv (data_terra,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_sat/terra/LP-25KM-MAIAC-T_comparativa.csv")
#write.csv (data_aqua,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_sat/aqua/LP-25KM-MAIAC-A_comparativa.csv")
#write.csv (data_terra,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_sat/terra/4_MD-25KM-MAIAC-T_comparativa.csv")
#write.csv (data_aqua,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_sat/aqua/4_MD-25KM-MAIAC-A_comparativa.csv")
write.csv (data_terra,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_sat/terra/6_MX-25KM-MAIAC-T_comparativa.csv")
write.csv (data_aqua,"D:/Josefina/paper_git/paper_maiac/datasets/maiac_comparativas_versiones_sat/aqua/6_MX-25KM-MAIAC-A_comparativa.csv")
