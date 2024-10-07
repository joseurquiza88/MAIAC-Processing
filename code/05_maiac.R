

## Pedido Seba
# Unir todos los data rfame de MAIAC procesados, es decir la correlacion con
# AERONET.
# Primero se sacan los promedios diarios de MAIAC Y AERONET y despue se unen
# Para el periodo 2015-2022
fecha <- as.data.frame(seq.Date(as.Date(ISOdate(2015,01,01)), as.Date(ISOdate(2022,12,31)), by = "day", tz = "GMT")) 
names(fecha )<- "date"
##    ------    DATOS BA
ba <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/BA-25KM-MAIAC-60-AER.csv")
ba %>%
  group_by(Date_MODIS) %>%  
  group_split() -> data_ba

df_ba_salida <- data.frame()
for (i in 1:length(data_ba)){
  df_ba <- data.frame (date = data_ba[[i]][["Date_MODIS"]][1],
                       BA_MAIAC = round(mean(data_ba[[i]][["AOD_550_maiac"]], na.rm=T),3),
                       BA_AER =  round(mean(data_ba[[i]][["AOD_550_AER_mean"]], na.rm=T),3))
  df_ba_salida <- rbind(df_ba_salida,df_ba )
}

df_ba_salida$date <- as.Date( df_ba_salida$date, format = "%Y-%m-%d")
ba_tot<- merge(df_ba_salida, fecha, by = "date", all = TRUE)

##    ------    DATOS SP
sp <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/SP-25KM-MAIAC-60-AER.csv")
sp %>%
  group_by(Date_MODIS) %>%  
  group_split() -> data_sp

df_sp_salida <- data.frame()
for (i in 1:length(data_sp)){
  df_sp<- data.frame (date = data_sp[[i]][["Date_MODIS"]][1],
                       SP_MAIAC = round(mean(data_sp[[i]][["AOD_550_maiac"]], na.rm=T),3),
                       SP_AER =  round(mean(data_sp[[i]][["AOD_550_AER_mean"]], na.rm=T),3))
  df_sp_salida <- rbind(df_sp_salida,df_sp)
}
df_sp_salida $date <- as.Date(df_sp_salida $date, format = "%Y-%m-%d")
sp_tot<- merge(df_sp_salida, fecha, by = "date", all = TRUE)




##    ------    DATOS ST
st <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/ST-25KM-MAIAC-60-AER.csv")
st %>%
  group_by(Date_MODIS) %>%  
  group_split() -> data_st

df_st_salida <- data.frame()
for (i in 1:length(data_st)){
  df_st<- data.frame (date = data_st[[i]][["Date_MODIS"]][1],
                      ST_MAIAC = round(mean(data_st[[i]][["AOD_550_maiac"]], na.rm=T),3),
                      ST_AER =  round(mean(data_st[[i]][["AOD_550_AER_mean"]], na.rm=T),3))
  df_st_salida <- rbind(df_st_salida,df_st)
}
df_st_salida $date <- as.Date(df_st_salida $date, format = "%Y-%m-%d")
st_tot<- merge(df_st_salida, fecha, by = "date", all = TRUE)


##    ------    DATOS MD
md <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MD-25KM-MAIAC-60-AER.csv")
md %>%
  group_by(Date_MODIS) %>%  
  group_split() -> data_md

df_md_salida <- data.frame()
for (i in 1:length(data_md)){
  df_md<- data.frame (date = data_md[[i]][["Date_MODIS"]][1],
                      MD_MAIAC = round(mean(data_md[[i]][["AOD_550_maiac"]], na.rm=T),3),
                      MD_AER =  round(mean(data_md[[i]][["AOD_550_AER_mean"]], na.rm=T),3))
  df_md_salida <- rbind(df_md_salida,df_md)
}
df_md_salida$date <- as.Date(df_md_salida$date, format = "%Y-%m-%d")
md_tot<- merge(df_md_salida, fecha, by = "date", all = TRUE)


##    ------    DATOS LP
lp <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/LP-25KM-MAIAC-60-AER.csv")
lp %>%
  group_by(Date_MODIS) %>%  
  group_split() -> data_lp

df_lp_salida <- data.frame()
for (i in 1:length(data_lp)){
  df_lp<- data.frame (date = data_lp[[i]][["Date_MODIS"]][1],
                      LP_MAIAC = round(mean(data_lp[[i]][["AOD_550_maiac"]], na.rm=T),3),
                      LP_AER =  round(mean(data_lp[[i]][["AOD_550_AER_mean"]], na.rm=T),3))
  df_lp_salida <- rbind(df_lp_salida,df_lp)
}
df_lp_salida$date <- as.Date(df_lp_salida$date, format = "%Y-%m-%d")
lp_tot<- merge(df_lp_salida, fecha, by = "date", all = TRUE)


##    ------    DATOS MX
mx <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MX-25KM-MAIAC-60-AER.csv")
mx %>%
  group_by(Date_MODIS) %>%  
  group_split() -> data_mx

df_mx_salida <- data.frame()
for (i in 1:length(data_mx)){
  df_mx<- data.frame (date = data_mx[[i]][["Date_MODIS"]][1],
                      MX_MAIAC = round(mean(data_mx[[i]][["AOD_550_maiac"]], na.rm=T),3),
                      MX_AER =  round(mean(data_mx[[i]][["AOD_550_AER_mean"]], na.rm=T),3))
  df_mx_salida <- rbind(df_mx_salida,df_mx)
}
df_mx_salida$date <- as.Date(df_mx_salida$date, format = "%Y-%m-%d")
mx_tot<- merge(df_mx_salida, fecha, by = "date", all = TRUE)


#
tot_1<- merge(sp_tot,st_tot, by = "date", all = TRUE)
#
tot_2<- merge(tot_1,ba_tot, by = "date", all = TRUE)
#
tot_3<- merge(tot_2,md_tot, by = "date", all = TRUE)
#
tot_4<- merge(tot_3,lp_tot, by = "date", all = TRUE)
#
tot_5<- merge(tot_4,mx_tot, by = "date", all = TRUE)

## Guardamos
write.csv(tot_5,"merge_maiac-aer.csv")
