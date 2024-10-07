#In this code, an average of AERONET measurements is made for 
# a given time interval centered  at satellite overpass to compare 
# it with the average of MAIAC retrievals


time_correlation <- function(path_aeronet,path_modis,time_buffer,formato_fecha){
  #path_aeronet AERONET file path
  # path_maiac MAIAC file path
  #time_buffer Time window considered in minutes. 
  #According to the literature: 15 min - 30min - 60min - 90min - 120min
  
  # Open AERONET data
  data_aeronet <- read.csv(path_aeronet, header=TRUE, sep=",", dec=".", na.strings = "NA", stringsAsFactors = FALSE)
  # Date formats
  data_aeronet$date <- as.POSIXct(strptime(data_aeronet$date, format = "%d/%m/%Y %H:%M", "GMT"))
  # Open MAIAC data
  data_sat <- read.csv(path_modis, header=TRUE, sep=",",dec=".", stringsAsFactors = FALSE, na.strings = "NA")
  #NAs are removed
  data_modis <- data_sat[complete.cases(data_sat$AOD),]
  # Date formats
  data_modis$date  <- strptime(data_modis$dia, tz= "GMT", format = "%d/%m/%Y")
  data_modis$dateHour  <- strptime(paste(data_modis$dia,data_modis$hora,sep=" "), tz= "GMT", format = "%d/%m/%Y %H:%M:%S")
  #data_modis$hour  <- strptime(data_modis$hora, tz= "GMT", format = "%H:%M:%S")
  
  MODIS_aeronet <- data.frame()
  AOD <- data.frame()
  
  for (i in 1: nrow(data_modis)){ 
    if (i %% 50 == 0) {
      print (i)
    }
    #Day-month-year agreement between AERONET and MAIAC is sought.
    table_aeronet<- data_aeronet 
    eq_year <- which(year(table_aeronet$date) == year(data_modis[i,]$date))
    
    table_aeronet<- table_aeronet[eq_year,] 
    
    eq_month <- which(month(table_aeronet$date) == month(data_modis[i,]$date))
    table_aeronet<- table_aeronet[eq_month,] 
    
    eq_day <- which(day(table_aeronet$date) == day(data_modis[i,]$date))
    table_aeronet<- table_aeronet[eq_day,]
    dim_table <- dim(table_aeronet)
    
    if(dim_table[1] == 0){
      out_data <- data.frame(NA, NA, NA, NA,NA,NA,NA,NA,NA,NA)   
      
    }else{ 
      #If there is a match, the AERONET time window is searched.
      table_dif <-data.frame()
      
      
      mach <- which(abs(difftime(table_aeronet$date, data_modis[i,]$dateHour,units = "mins")) <time_buffer)
      
      
      table_dif <- table_aeronet[mach,]
      dim_table <- dim(table_dif)
      if(dim_table[1] == 0){  
        df <- data.frame()
        df <- data.frame(NA, NA,NA, NA, NA,NA,NA,NA)
        names(df) <- c("Date_MODIS","timestamp", "AOD_550_modis", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")   
      }else{
        #The output file is created with co-located MAIAC and AERONET data.
        out_data <- data.frame(mean(table_dif[,5],  na.rm=TRUE),
                               median(table_dif[,5],  na.rm=TRUE),
                               sd(table_dif[,5], na.rm=TRUE), (dim_table[1]))
        names(out_data) <- c("mean", "mediana","sd","dim")
        df <- data.frame() 
        #df <- data.frame(data_maiac[i,2],data_maiac[i,16], data_maiac[i,10:13], substr(table_dif[1,1],1,10),out_data[,1:4])
        df <- data.frame(data_modis[i,8],data_modis[i,9], data_modis[i,4], substr(table_dif[1,1],1,10),out_data[,1:4])
        #df <- data.frame(data_maiac[i,1],data_maiac[i,15], data_maiac[i,9:12], substr(table_dif[1,1],1,10),out_data[,1:4])
        names(df) <- c("Date_MODIS","timestamp", "AOD_550_modis", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
      }
      AOD <- rbind(AOD, df)
      
      names(AOD) <- c("Date_MODIS","timestamp", "AOD_550_modis", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
      AOD <- AOD[complete.cases(AOD),]
    }
  }
  return(AOD)
}

######     -------  EXAMPLE for one station     -------  ######
buffer_time <- 60 #minutes
buffer_spatial <-5
city <- "SL"
num <- 5
formato_fecha<-"%Y-%m-%d %H:%M:%S"
data_modis_BA <- paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/modis/USA/",city,"/",city,"-",buffer_spatial,"KM-MODIS.csv", sep="")
#data_modis_BA_60 <- "D:/Josefina/paper_git/paper_modis/datasets/V02/modis/Latam_C60/BA/prueba_25km_BA_tot.csv"

data_aeronet_BA <-paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/datasets_interp_s_L02/USA/",num,"_",city,"_2015-2022_interp-s_V02_L2.csv",sep="")
combinate_BA <- time_correlation (path_aeronet=data_aeronet_BA,path_modis=data_modis_BA,time_buffer=buffer_time,formato_fecha=formato_fecha)# Save the file with co-located data from AERONET and modis on local path
write.csv (combinate_BA,paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/USA/tot/",buffer_spatial,"km/",buffer_time,"/",num,"_",city,"-",buffer_spatial,"km-modis-",buffer_time,"-AER.csv",sep=""))

###############################################################################

###############################################################################
# PROMEDIOS DIARIOS
buffer_time <- 120 #minutes
for (i in 1:1){
  promedios <- function(combinate){
    rbind_combinate <- data.frame()
    combinate$date <-   as.POSIXct(strptime(combinate$Date_MODIS, format = "%Y-%m-%d", "GMT"))
    combinate%>%
      group_by(date) %>%  
      group_split() ->combinate_group
    
    for (i in 1:length(combinate_group)){
      df <- data.frame( date = combinate_group[[i]][["date"]][1],
                        AOD_550_maiac_mean = mean(combinate_group[[i]][["AOD_550_modis"]],na.rm=T),
                        AOD_550_AER_mean = mean(combinate_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
      rbind_combinate <- rbind(rbind_combinate,df)
    }
    return(rbind_combinate)
    
  }
  buffer_spatial <-25
  city <- "SL"
  num <- 5
  #combinate_SP <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/Latam_C61/tot/1_SP-25KM-MAIAC-60-AER.csv")
  combinate_SP <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/USA/tot/",buffer_spatial,"km/",buffer_time,"/",num,"_",city,"-",buffer_spatial,"km-modis-",buffer_time,"-AER.csv",sep=""))
  SP_com_promedios <- promedios(combinate=combinate_SP)

  #write.csv (SP_com_promedios,paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/Latam/dia/",buffer_spatial,"km/",buffer_time,"/",num,"_",city,"-",buffer_spatial,"km-modis-",buffer_time,"-AER.csv",sep=""))
  write.csv (SP_com_promedios,paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MODIS/USA/dia/",buffer_spatial,"km/",buffer_time,"/",num,"_",city,"-",buffer_spatial,"km-modis-",buffer_time,"-AER.csv",sep=""))
}
rm(list = ls())
