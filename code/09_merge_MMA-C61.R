
# Daily Merge between MODIS-MAIAC-AERONET C6.1
###########                          ------ BA ------
data_modis_BA <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MODIS/MODIS_dia/BA-25KM-MODIS-60-AER-DIA.csv")
data_maiac_BA <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/3_BA-25KM-MAIAC-60-AER_MEAN.csv")

#Merge with the column called "date"

merge_sat <- merge(x = data_modis_BA, y = data_maiac_BA, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_modis,
                        AOD_maiac = merge_sat$AOD_550_maiac_mean,
                        AOD_550_AER_mean = ((merge_sat$AOD_aeronet+merge_sat$AOD_550_AER_mean)/2))

names(merge_sat) <- c("date, AOD_550_MODIS_mean, AOD_550_MAIAC_mean, AOD_550_AER_mean")

write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/3_BA-25KM-MM-60-AER-C61.csv")


###########                          ------ SP ------
data_modis_SP <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MODIS/MODIS_dia/SP-25KM-MODIS-60-AER-DIA.csv")
data_maiac_SP <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/1_SP-25KM-MAIAC-60-AER_MEAN.csv")

#Merge with the column called "date"

merge_sat <- merge(x = data_modis_SP, y = data_maiac_SP, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_modis,
                        AOD_maiac = merge_sat$AOD_550_maiac_mean,
                        AOD_550_AER_mean = ((merge_sat$AOD_aeronet+merge_sat$AOD_550_AER_mean)/2))

names(merge_sat) <- c("date, AOD_550_MODIS_mean, AOD_550_MAIAC_mean, AOD_550_AER_mean")

write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/1_SP-25KM-MM-60-AER-C61.csv")




###########                          ------ ST ------
data_modis_ST <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MODIS/MODIS_dia/ST-25KM-MODIS-60-AER-DIA.csv")
data_maiac_ST <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/2_ST-25KM-MAIAC-60-AER_MEAN.csv")

#Merge with the column called "date"

merge_sat <- merge(x = data_modis_ST, y = data_maiac_ST, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_modis,
                        AOD_maiac = merge_sat$AOD_550_maiac_mean,
                        AOD_550_AER_mean = ((merge_sat$AOD_aeronet+merge_sat$AOD_550_AER_mean)/2))

names(merge_sat) <- c("date, AOD_550_MODIS_mean, AOD_550_MAIAC_mean, AOD_550_AER_mean")

write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/2_ST-25KM-MM-60-AER-C61.csv")

###########                          ------ MD ------
data_modis_MD <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MODIS/MODIS_dia/MD-25KM-MODIS-60-AER-DIA.csv")
data_maiac_MD <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/4_MD-25KM-MAIAC-60-AER_MEAN.csv")

#Merge with the column called "date"

merge_sat <- merge(x = data_modis_MD, y = data_maiac_MD, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_modis,
                        AOD_maiac = merge_sat$AOD_550_maiac_mean,
                        AOD_550_AER_mean = ((merge_sat$AOD_aeronet+merge_sat$AOD_550_AER_mean)/2))

names(merge_sat) <- c("date, AOD_550_MODIS_mean, AOD_550_MAIAC_mean, AOD_550_AER_mean")

write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/4_MD-25KM-MM-60-AER-C61.csv")

###########                          ------ LP ------
data_modis_LP <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MODIS/MODIS_dia/LP-25KM-MODIS-60-AER-DIA.csv")
data_maiac_LP <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/5_LP-25KM-MAIAC-60-AER_MEAN.csv")

#Merge with the column called "date"

merge_sat <- merge(x = data_modis_LP, y = data_maiac_LP, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_modis,
                        AOD_maiac = merge_sat$AOD_550_maiac_mean,
                        AOD_550_AER_mean = ((merge_sat$AOD_aeronet+merge_sat$AOD_550_AER_mean)/2))

names(merge_sat) <- c("date, AOD_550_MODIS_mean, AOD_550_MAIAC_mean, AOD_550_AER_mean")

write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/5_LP-25KM-MM-60-AER-C61.csv")

###########                          ------ MX ------
data_modis_MX <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MODIS/MODIS_dia/MX-25KM-MODIS-60-AER-DIA.csv")
data_maiac_MX <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/6_MX-25KM-MAIAC-60-AER_MEAN.csv")

#Merge with the column called "date"

merge_sat <- merge(x = data_modis_MX, y = data_maiac_MX, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_modis,
                        AOD_maiac = merge_sat$AOD_550_maiac_mean,
                        AOD_550_AER_mean = ((merge_sat$AOD_aeronet+merge_sat$AOD_550_AER_mean)/2))

names(merge_sat) <- c("date, AOD_550_MODIS_mean, AOD_550_MAIAC_mean, AOD_550_AER_mean")

write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/6_MX-25KM-MM-60-AER-C61.csv")

