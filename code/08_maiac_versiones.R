
# Union de los df de las dos versiones 6.0 y version 6.1
# Hacemos un merge
#vemos que la funcion merge nos devuelve solo las fechas que coinciden

#Solo unimos las medias diarias, no usamos los df con todos los datos


#### ---- SP ---- ####

data_6.1_SP <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/1_SP-25KM-MAIAC-60-AER_MEAN.csv")
data_6_SP <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/dia/1_SP-25KM-MAIAC-60-AER_MEAN.csv")

df_tot_SP <- merge(x = data_6.1_SP, y = data_6_SP, by = "date") # Equivalente

df_subt_SP <- data.frame(date = df_tot_SP$date,
                         maiac_61 = df_tot_SP$AOD_550_maiac_mean.x,
                         maiac_6 = df_tot_SP$AOD_550_maiac_mean.y,
                         aeronet_mean = ((df_tot_SP$AOD_550_AER_mean.x+df_tot_SP$AOD_550_AER_mean.y)/2))

write.csv(df_subt_SP,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/1_SP-MAIAC-V6-61-AER_DIA.csv")
#### ---- ST ---- ####

data_6.1_ST <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/2_ST-25KM-MAIAC-60-AER_MEAN.csv")
data_6_ST <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/dia/2_ST-25KM-MAIAC-60-AER_MEAN.csv")

df_tot_ST <- merge(x = data_6.1_ST, y = data_6_ST, by = "date") # Equivalente

df_subt_ST <- data.frame(date = df_tot_ST$date,
                         maiac_61 = df_tot_ST$AOD_550_maiac_mean.x,
                         maiac_6 = df_tot_ST$AOD_550_maiac_mean.y,
                         aeronet_mean = ((df_tot_ST$AOD_550_AER_mean.x+df_tot_ST$AOD_550_AER_mean.y)/2))
write.csv(df_subt_ST,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/2_ST-MAIAC-V6-61-AER_DIA.csv")

#### ---- BA ---- ####
data_6.1_BA <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/3_BA-25KM-MAIAC-60-AER_MEAN.csv")
data_6_BA <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/dia/3_BA-25KM-MAIAC-60-AER_MEAN.csv")

df_tot_BA <- merge(x = data_6.1_BA, y = data_6_BA, by = "date") # Equivalente
df_subt_BA <- data.frame(date = df_tot_BA$date,
                         maiac_61 = df_tot_BA$AOD_550_maiac_mean.x,
                         maiac_6 = df_tot_BA$AOD_550_maiac_mean.y,
                         aeronet_mean = ((df_tot_BA$AOD_550_AER_mean.x+df_tot_BA$AOD_550_AER_mean.y)/2))
write.csv(df_subt_BA,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/3_BA-MAIAC-V6-61-AER_DIA.csv")

#### ---- LP ---- ####

data_6.1_LP <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/5_LP-25KM-MAIAC-60-AER_MEAN.csv")
data_6_LP <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/dia/5_LP-25KM-MAIAC-60-AER_MEAN.csv")

df_tot_LP <- merge(x = data_6.1_LP, y = data_6_LP, by = "date") # Equivalente
df_subt_LP <- data.frame(date = df_tot_LP$date,
                         maiac_61 = df_tot_LP$AOD_550_maiac_mean.x,
                         maiac_6 = df_tot_LP$AOD_550_maiac_mean.y,
                         aeronet_mean = ((df_tot_LP$AOD_550_AER_mean.x+df_tot_LP$AOD_550_AER_mean.y)/2))

write.csv(df_subt_LP,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/5_LP-MAIAC-V6-61-AER_DIA.csv")

#### ---- MD ---- ####

data_6.1_MD <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/4_MD-25KM-MAIAC-60-AER_MEAN.csv")
data_6_MD <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/dia/4_MD-25KM-MAIAC-60-AER_MEAN.csv")

df_tot_MD<- merge(x = data_6.1_MD, y = data_6_MD, by = "date") # Equivalente

df_subt_MD <- data.frame(date = df_tot_MD$date,
                         maiac_61 = df_tot_MD$AOD_550_maiac_mean.x,
                         maiac_6 = df_tot_MD$AOD_550_maiac_mean.y,
                         aeronet_mean = ((df_tot_MD$AOD_550_AER_mean.x+df_tot_MD$AOD_550_AER_mean.y)/2))
write.csv(df_subt_MD,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/4_MD-MAIAC-V6-61-AER_DIA.csv")


#### ---- MX ---- ####

data_6.1_MX <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/6_MX-25KM-MAIAC-60-AER_MEAN.csv")
data_6_MX <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/dia/6_MX-25KM-MAIAC-60-AER_MEAN.csv")

df_tot_MX<- merge(x = data_6.1_MX, y = data_6_MX, by = "date") # Equivalente

df_subt_MX <- data.frame(date = df_tot_MX$date,
                         maiac_61 = df_tot_MX$AOD_550_maiac_mean.x,
                         maiac_6 = df_tot_MX$AOD_550_maiac_mean.y,
                         aeronet_mean = ((df_tot_MX$AOD_550_AER_mean.x+df_tot_MX$AOD_550_AER_mean.y)/2))

write.csv(df_subt_MX,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/6_MX-MAIAC-V6-61-AER_DIA.csv")
