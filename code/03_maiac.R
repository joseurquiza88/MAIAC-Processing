#In this code, an average of AERONET measurements is made for 
# a given time interval centered  at satellite overpass to compare 
# it with the average of MODIS - MAIAC - AERONET retrievals



time_correlation_MMA <- function(path_aeronet,path_modis){
  #path_aeronet AERONET file path
  # path_modis modis file path
  #time_buffer Time window considered in minutes. 
  #According to the literature: 15 min - 30min - 60min - 90min - 120min
  
  # Open AERONET data
  data_aeronet <- read.csv(path_aeronet, header=TRUE, sep=",", dec=".", na.strings = "NA", stringsAsFactors = FALSE)
  # Date formats
  data_aeronet$date <- as.POSIXct(strptime(data_aeronet$timestamp, format = "%Y-%m-%d %H:%M", "GMT"))
  # Open modis data
  data_sat <- read.csv(path_modis, header=TRUE, sep=",",dec=".", stringsAsFactors = FALSE, na.strings = "NA")
  
  
  
  
  #NAs are removed
  data_modis <- data_sat  [complete.cases(data_sat),]
  # Date formats
  data_modis$date  <- strptime(data_modis$Date_MODIS, tz= "GMT", format = "%Y-%m-%d")
  data_modis<- data.frame(date = data_modis$date,
                          AOD_550 = data_modis$AOD_550_modis
  )

  data_modis$hora_2  <- strptime( data_modis$date, tz= "GMT", format = "%Y-%m-%d")
  data_modis$date  <- strptime( data_modis$date, tz= "GMT", format = "%Y-%m-%d")
  MODIS_aeronet <- data.frame()
  AOD <- data.frame()
  
  for (i in 1: nrow(data_modis)){ 
    if (i %% 50 == 0) {
      print (i)
    }
    #Day-month-year agreement between AERONET and modis is sought.
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
      
      
      mach <- which(abs(difftime(table_aeronet$date, data_modis[i,]$hora_2,units = "days")) <1)
      
      
      table_dif <- table_aeronet[mach,]
      dim_table <- dim(table_dif)
      if(dim_table[1] == 0){  
        df <- data.frame()
        df <- data.frame(NA, NA,NA, NA, NA,NA,NA,NA)
        names(df) <- c("Date_MODIS", "AOD_550_modis", "satellite",
                       "Date_AERONET","AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")#, "AOT_550_2", "AOT_550_3")
      }else{
        #The output file is created with co-located modis and AERONET data.
        out_data <- data.frame(mean(table_dif[,6],  na.rm=TRUE),mean(table_dif[,9],  na.rm=TRUE),
                              #median(tabla_dif[,5],  na.rm=TRUE),
                              #sd(tabla_dif[,5], na.rm=TRUE)
                              (dim_table[1]))
        names(out_data) <- c("AOD_550_MAIAC_mean", "AOD_550_AER_mean","dim")
        df <- data.frame() 

        
        df<- data.frame(data_modis[i,1:2], out_data[,1:3])
        names(df) <- c("date","AOD_550_MODIS_mean","AOD_550_MAIAC_mean", "AOD_550_AER_mean","dim")
        
      }
      AOD <- rbind(AOD, df)
      
      names(AOD) <- c("date","AOD_550_MODIS_mean","AOD_550_MAIAC_mean", "AOD_550_AER_mean","dim")
      
    }
  }
  return(AOD)
}


######     -------  EXAMPLE for one station     -------  ######



data_modis_BA <- "D:/Josefina/papers_escritos/paper_maiac/datasets/processed/BA-25KM-MODIS-60-AER.csv"
data_aeronet_BA <-"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/BA-25KM-MAIAC-60-AER.csv"
combinate_BA <- time_correlation_MMA (path_aeronet=data_aeronet_BA,path_modis=data_modis_BA)
# Save the file with co-located data from AERONET and modis on local path
write.csv (combinate_BA,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/MMA/BA-25KM-MM-60-AER.csv")

# SP
data_modis_SP <- "D:/Josefina/papers_escritos/paper_maiac/datasets//processed/SP-25KM-MODIS-60-AER.csv"
data_aeronet_SP <-"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/SP-25KM-MAIAC-60-AER.csv"
combinate_SP <- time_correlation_MMA (path_aeronet=data_aeronet_SP,path_modis=data_modis_SP)
write.csv (combinate_SP,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/MMA/SP-25KM-MM-60-AER.csv")


# ST
data_modis_ST <- "D:/Josefina/papers_escritos/paper_maiac/datasets/processed/ST-25KM-MODIS-60-AER.csv"
data_aeronet_ST <-"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/ST-25KM-MAIAC-60-AER.csv"
combinate_ST <- time_correlation_MMA (path_aeronet=data_aeronet_ST,path_modis=data_modis_ST)
write.csv (combinate_ST,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/MMA/ST-25KM-MMA-60-AER.csv")

# MD
data_modis_MD <- "D:/Josefina/papers_escritos/paper_maiac/datasets/processed/MD-25KM-MODIS-60-AER.csv"
data_aeronet_MD <-"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/MD-25KM-MAIAC-60-AER.csv"
combinate_MD <- time_correlation_MMA (path_aeronet=data_aeronet_MD,path_modis=data_modis_MD)
write.csv (combinate_MD,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/MMA/MD-25KM-MMA-60-AER.csv")


# LP
data_modis_LP <- "D:/Josefina/papers_escritos/paper_maiac/datasets/processed/LP-25KM-MODIS-60-AER.csv"
data_aeronet_LP <-"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/LP-25KM-MAIAC-60-AER.csv"
combinate_LP <- time_correlation_MMA (path_aeronet=data_aeronet_LP,path_modis=data_modis_LP)
write.csv (combinate_LP,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/MMA/LP-25KM-MMA-60-AER.csv")


# MX
data_modis_MX <- "D:/Josefina/papers_escritos/paper_maiac/datasets//processed/MX-25KM-MODIS-60-AER.csv"
data_aeronet_MX <-"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/MX-25KM-MAIAC-60-AER.csv"
combinate_MX <- time_correlation_MMA (path_aeronet=data_aeronet_MX,path_modis=data_modis_MX)
write.csv (combinate_MX,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/MMA/MX-25KM-MMA-60-AER.csv")



path_aeronet <- "D:/Josefina/papers_escritos/paper_maiac/datasets/processed/BA-25KM-MAIAC-60-AER.csv"
path_modis <- "D:/Josefina/papers_escritos/paper_maiac/datasets/processed/BA-25KM-MODIS-60-AER.csv"
