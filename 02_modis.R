#In this code, an average of AERONET measurements is made for 
# a given time interval centered  at satellite overpass to compare 
# it with the average of MODIS retrievals


time_correlation <- function(path_aeronet,path_modis,time_buffer){
  #path_aeronet AERONET file path
  # path_modis modis file path
  #time_buffer Time window considered in minutes. 
  #According to the literature: 15 min - 30min - 60min - 90min - 120min
  
  # Open AERONET data
  data_aeronet <- read.csv(path_aeronet, header=TRUE, sep=",", dec=".", na.strings = "NA", stringsAsFactors = FALSE)
  # Date formats
  data_aeronet$date <- as.POSIXct(strptime(data_aeronet$date, format = "%Y-%m-%d %H:%M", "GMT"))
  # Open modis data
  data_sat <- read.csv(path_modis, header=TRUE, sep=",",dec=".", stringsAsFactors = FALSE, na.strings = "NA")
    
  

  
  #NAs are removed
  data_modis <- data_sat  [complete.cases(data_sat$AOD),]
  # Date formats
  data_modis$date  <- strptime(data_modis$dia, tz= "GMT", format = "%d/%m/%Y")
  data_modis $timestamp <- paste( data_modis$dia, data_modis$hora, sep = " ")
  data_modis $hour <- strptime( data_modis$timestamp, tz= "GMT", format = "%d/%m/%Y %H:%M")
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
      
      
      mach <- which(abs(difftime(table_aeronet$date, data_modis[i,]$hour,units = "mins")) <time_buffer)
      
      
      table_dif <- table_aeronet[mach,]
      dim_table <- dim(table_dif)
      if(dim_table[1] == 0){  
        df <- data.frame()
        df <- data.frame(NA, NA,NA, NA, NA,NA,NA,NA)
        names(df) <- c("Date_MODIS", "AOD_550_modis", "satellite",
                       "Date_AERONET","AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")#, "AOT_550_2", "AOT_550_3")
      }else{
        #The output file is created with co-located modis and AERONET data.
        out_data <- data.frame(mean(table_dif[,5],  na.rm=TRUE),
                               median(table_dif[,5],  na.rm=TRUE),
                               sd(table_dif[,5], na.rm=TRUE), (dim_table[1]))
        names(out_data) <- c("mean", "mediana","sd","dim")
        df <- data.frame() 
        df <- data.frame(data_modis[i,10],data_modis[i,4], data_modis[i,8], substr(table_dif[1,1],1,10),out_data[,1:4])
        names(df) <- c("Date_MODIS", "AOD_550_modis", "satellite",
                            "Date_AERONET","AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")#, "AOT_550_2", "AOT_550_3")
        
      }
      AOD <- rbind(AOD, df)
      
      names(df) <- c("Date_MODIS", "AOD_550_modis", "satellite",
                     "Date_AERONET","AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")#, "AOT_550_2", "AOT_550_3")
      AOD <- AOD[complete.cases(AOD),]
    }
  }
  return(AOD)
}


######     -------  EXAMPLE for one station     -------  ######

buffer_time <- 60 #minutes

#Change directory
data_modis_BA <- "D:/Josefina/papers_escritos/paper_maiac/datasets/modis/BA-25KM-MODIS.csv"
data_aeronet_BA <-"D:/Josefina/papers_escritos/paper_maiac/datasets/aeronet/datasets_interp_s/BA_2015-2022_interp-s.csv"
combinate_BA <- time_correlation (path_aeronet=data_aeronet_BA,path_modis=data_modis_BA,time_buffer=buffer_time)
# Save the file with co-located data from AERONET and modis on local path
write.csv (combinate_BA,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/BA-25KM-MODIS-60-AER.csv")

# SP
data_modis_SP <- "D:/Josefina/papers_escritos/paper_maiac/datasets/modis/SP-25KM-MODIS.csv"
data_aeronet_SP <-"D:/Josefina/papers_escritos/paper_maiac/datasets/aeronet/datasets_interp_s/SP_2015-2022_interp-s.csv"
combinate_SP <- time_correlation (path_aeronet=data_aeronet_SP,path_modis=data_modis_SP,time_buffer=buffer_time)
write.csv (combinate_SP,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/SP-25KM-MODIS-60-AER.csv")


# ST
data_modis_ST <- "D:/Josefina/papers_escritos/paper_maiac/datasets/modis/ST-25KM-MODIS.csv"
data_aeronet_ST <-"D:/Josefina/papers_escritos/paper_maiac/datasets/aeronet/datasets_interp_s/ST_2015-2022_interp-s.csv"
combinate_ST <- time_correlation (path_aeronet=data_aeronet_ST,path_modis=data_modis_ST,time_buffer=buffer_time)
write.csv (combinate_ST,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/ST-25KM-MODIS-60-AER.csv")

# MD
data_modis_MD <- "D:/Josefina/papers_escritos/paper_maiac/datasets/modis/MD-25KM-MODIS.csv"
data_aeronet_MD <-"D:/Josefina/papers_escritos/paper_maiac/datasets/aeronet/datasets_interp_s/MD_2015-2022_interp-s.csv"
combinate_MD <- time_correlation (path_aeronet=data_aeronet_MD,path_modis=data_modis_MD,time_buffer=buffer_time)
write.csv (combinate_MD,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/MD-25KM-MODIS-60-AER.csv")


# LP
data_modis_LP <- "D:/Josefina/papers_escritos/paper_maiac/datasets/modis/LP-25KM-MODIS.csv"
data_aeronet_LP <-"D:/Josefina/papers_escritos/paper_maiac/datasets/aeronet/datasets_interp_s/LP_2015-2022_interp-s.csv"
combinate_LP <- time_correlation (path_aeronet=data_aeronet_LP,path_modis=data_modis_LP,time_buffer=buffer_time)
write.csv (combinate_LP,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/LP-25KM-MODIS-60-AER.csv")


# MX
data_modis_MX <- "D:/Josefina/papers_escritos/paper_maiac/datasets/modis/MX-25KM-MODIS.csv"
data_aeronet_MX <-"D:/Josefina/papers_escritos/paper_maiac/datasets/aeronet/datasets_interp_s/MX_2015-2022_interp-s.csv"
combinate_MX <- time_correlation (path_aeronet=data_aeronet_MX,path_modis=data_modis_MX,time_buffer=buffer_time)
write.csv (combinate_MX,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/MX-25KM-MODIS-60-AER.csv")


### ---- Daily mean

dire <- "D:/Josefina/paper_git/paper_maiac/datasets/processed/MODIS/MODIS_tot/" 
# Local path where the .HDF files are located
id <- dir(dire, pattern = ".csv")
#Important: be located in the path where the files are located
setwd(dire) 
for (i in 1:length(id)){
  data_modis <- read.csv(id[i])
  data_modis %>%
    group_by(Date_MODIS) %>%  
    group_split() -> data
  df_rbind <- data.frame()
  for (p in 1:length(data)){ 
    df <- data.frame(date = data[[p]][["Date_MODIS"]][1],# only first date
                     AOD_modis = mean(data[[p]][["AOD_550_modis"]],na.rm=T),
                     AOD_aeronet = mean(data[[p]][["AOD_550_AER_sd"]],na.rm=T))
    df_rbind <- rbind(df_rbind ,df)
  }
  print(substr(id[i],1,20))
  name <- paste("D:/Josefina/paper_git/paper_maiac/datasets/processed/MODIS/MODIS_dia/",substr(id[i],1,20),"-DIA.csv",sep = "")
  write.csv(df_rbind,name)
}

