
dire <- paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/LandCover/" ,sep="/")
# Local path where the .HDF files are located
id <- dir(dire, pattern = ".csv")
#Important: be located in the path where the files are located
setwd(dire) 
data_tot_year_buffer<- data.frame()
for (file in 1:length(id)){
  print(id[file])#1,2,3
  
  data <- read.csv(id[file])
  site<- substr(id[file],11,12)
  buffer<- substr(id[file],14,16)
  data <- data[complete.cases(data), ]
  data_tot_year<- data.frame()
  data$entero_IGBP <- trunc(data$LandCover_IGBP) 
  data$entero_UMD <- trunc(data$LandCover_UMD) 
  data$entero_LAI <- trunc(data$LandCover_LAI) 
  data$entero_BGC <- trunc(data$LandCover_BGC) 
  data$entero_PFT <- trunc(data$LandCover_PFT) 
  data_tot<-data.frame()
  for(i in 2015:2022){
    year <- i
    data_sub <- data[data$year == year,]
    len <- nrow(data_sub)
    data_sub_IGBP <- data.frame(clasificacion = "IGBP",num = table(data_sub$entero_IGBP))
    data_sub_UMD <- data.frame(clasificacion = "UMD",num = table(data_sub$entero_UMD))
    data_sub_LAI <- data.frame(clasificacion = "LAI",num = table(data_sub$entero_LAI))
    data_sub_BGC <- data.frame(clasificacion = "BGC",num = table(data_sub$entero_BGC))
    data_sub_PFT <- data.frame(clasificacion = "PFT",num = table(data_sub$entero_PFT))
    
    
    data_tot <- rbind(data_sub_IGBP,data_sub_UMD,data_sub_LAI,data_sub_BGC,data_sub_PFT)
    data_tot$year <- year
    
    data_tot_year <- rbind(data_tot_year,data_tot)
    

  }
  data_tot_year$porc <- round(((data_tot_year$num.Freq/len)*100),2)
  data_tot_year$num_pixel <- len
  data_tot_year$buffer <- buffer
  data_tot_year$site <- site
  data_tot_year_buffer <- rbind(data_tot_year_buffer,data_tot_year)

}
names(data_tot_year_buffer) <- c("clasificacion","num_clasificacion","frecuencia", "year",         
                                 "porcentaje","numPixel","buffer","site")
data_tot_year_buffer

write.csv(data_tot_year_buffer,"D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/LandCover/Clasificacion_LandCover2.csv")








