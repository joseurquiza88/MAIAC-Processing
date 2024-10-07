
# Daily Merge between MODIS-MAIAC-AERONET C6.1-6.0
###########                          ------ SP ------


data_maiac <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/1_SP-MAIAC-V6-61-AER_DIA.csv")
data_modis <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/1_SP-25KM-MM-60-AER-C61.csv")



#Merge with the column called "date"

merge_sat <- merge(x = data_maiac, y = data_modis, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_550_MODIS_mean,
                        AOD_maiac_60 = merge_sat$maiac_6,
                        AOD_maiac_61 = merge_sat$maiac_61,
                        AOD_550_AER_mean = merge_sat$AOD_550_AER_mean)


write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61-AER-MOD/1_SP-25KM-60-M6M61-AER-MOD.csv")

##########                          ------ ST ------
# data_maiac <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/2_ST-MAIAC-V6-61-AER_DIA.csv")
# data_modis <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/2_ST-25KM-MM-60-AER-C61.csv")
data_maiac <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C61/dia/3km/6_MX-3km-MAIAC-60-AER_MEAN_c61.csv")
data_modis <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/Latam/dia/3km/6_MX-3km-modis-60-AER.csv")
data_maiac$date  <- strptime(data_maiac$date, format = "%Y-%m-%d")
data_modis$date  <- strptime(data_modis$date, format = "%Y-%m-%d")

#Merge with the column called "date"

merge_sat <- merge(x = data_maiac, y = data_modis, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_550_maiac_mean.y,
                        AOD_modis_AER = merge_sat$AOD_550_AER_mean.y,
                        AOD_maiac= merge_sat$AOD_550_maiac_mean.x,
                        AOD_maiac_AER = merge_sat$AOD_550_AER_mean.y)


#write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61-AER-MOD/1_ST-25KM-60-M6M61-AER-MOD.csv")

write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC-MODIS/Latam_C61/6_MX-3km-MAIAC-61-AER_MEAN_MODIS.csv")

##########                          ------ BA ------
data_maiac <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/3_BA-MAIAC-V6-61-AER_DIA.csv")
data_modis <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/3_BA-25KM-MM-60-AER-C61.csv")



#Merge with the column called "date"

merge_sat <- merge(x = data_maiac, y = data_modis, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_550_MODIS_mean,
                        AOD_maiac_60 = merge_sat$maiac_6,
                        AOD_maiac_61 = merge_sat$maiac_61,
                        AOD_550_AER_mean = merge_sat$AOD_550_AER_mean)


write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61-AER-MOD/3_BA-25KM-60-M6M61-AER-MOD.csv")



##########                          ------ MD ------
data_maiac <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/4_MD-MAIAC-V6-61-AER_DIA.csv")
data_modis <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/4_MD-25KM-MM-60-AER-C61.csv")



#Merge with the column called "date"

merge_sat <- merge(x = data_maiac, y = data_modis, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_550_MODIS_mean,
                        AOD_maiac_60 = merge_sat$maiac_6,
                        AOD_maiac_61 = merge_sat$maiac_61,
                        AOD_550_AER_mean = merge_sat$AOD_550_AER_mean)


write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61-AER-MOD/4_MD-25KM-60-M6M61-AER-MOD.csv")


##########                          ------ LP ------
data_maiac <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/5_LP-MAIAC-V6-61-AER_DIA.csv")
data_modis <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/5_LP-25KM-MM-60-AER-C61.csv")



#Merge with the column called "date"

merge_sat <- merge(x = data_maiac, y = data_modis, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_550_MODIS_mean,
                        AOD_maiac_60 = merge_sat$maiac_6,
                        AOD_maiac_61 = merge_sat$maiac_61,
                        AOD_550_AER_mean = merge_sat$AOD_550_AER_mean)


write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61-AER-MOD/5_LP-25KM-60-M6M61-AER-MOD.csv")

##########                          ------ MX ------
data_maiac <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/6_MX-MAIAC-V6-61-AER_DIA.csv")
data_modis <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/6_MX-25KM-MM-60-AER-C61.csv")



#Merge with the column called "date"

merge_sat <- merge(x = data_maiac, y = data_modis, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_550_MODIS_mean,
                        AOD_maiac_60 = merge_sat$maiac_6,
                        AOD_maiac_61 = merge_sat$maiac_61,
                        AOD_550_AER_mean = merge_sat$AOD_550_AER_mean)


write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61-AER-MOD/6_MX-25KM-60-M6M61-AER-MOD.csv")
