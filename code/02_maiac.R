
#In this code, an average of AERONET measurements is made for 
# a given time interval centered  at satellite overpass to compare 
# it with the average of MAIAC retrievals


time_correlation <- function(path_aeronet,path_maiac,time_buffer){
   #path_aeronet AERONET file path
   # path_maiac MAIAC file path
   #time_buffer Time window considered in minutes. 
   #According to the literature: 15 min - 30min - 60min - 90min - 120min
  
  # Open AERONET data
   data_aeronet <- read.csv(path_aeronet, header=TRUE, sep=",", dec=".", na.strings = "NA", stringsAsFactors = FALSE)
   # Date formats
   data_aeronet$date <- as.POSIXct(strptime(data_aeronet$date, format = "%Y-%m-%d %H:%M", "GMT"))
   # Open MAIAC data
   data_sat <- read.csv(path_maiac, header=TRUE, sep=",",dec=".", stringsAsFactors = FALSE, na.strings = "NA")
   #NAs are removed
   data_maiac <- data_sat[complete.cases(data_sat$AOD_055),]
   # Date formats
   data_maiac$date  <- strptime(data_maiac$date, tz= "GMT", format = "%Y%j")
   data_maiac$hour  <- strptime(data_maiac$timestamp, tz= "GMT", format = "%Y%j%H%M")
  
  MODIS_aeronet <- data.frame()
  AOD <- data.frame()

  for (i in 1: nrow(data_maiac)){ 
    if (i %% 50 == 0) {
      print (i)
    }
    #Day-month-year agreement between AERONET and MAIAC is sought.
    table_aeronet<- data_aeronet 
    eq_year <- which(year(table_aeronet$date) == year(data_maiac[i,]$date))
    
    table_aeronet<- table_aeronet[eq_year,] 
    
    eq_month <- which(month(table_aeronet$date) == month(data_maiac[i,]$date))
    table_aeronet<- table_aeronet[eq_month,] 
    
    eq_day <- which(day(table_aeronet$date) == day(data_maiac[i,]$date))
    table_aeronet<- table_aeronet[eq_day,]
    dim_table <- dim(table_aeronet)
    
    if(dim_table[1] == 0){
      out_data <- data.frame(NA, NA, NA, NA,NA,NA,NA,NA,NA,NA)   
      
    }else{ 
      #If there is a match, the AERONET time window is searched.
      table_dif <-data.frame()
      
      
      mach <- which(abs(difftime(table_aeronet$date, data_maiac[i,]$hour,units = "mins")) <time_buffer)
      
      
      table_dif <- table_aeronet[mach,]
      dim_table <- dim(table_dif)
      if(dim_table[1] == 0){  
        df <- data.frame()
        df <- data.frame(NA, NA,NA, NA, NA,NA,NA,NA,NA,NA,NA)
        names(df) <- c("Date_MODIS","timestamp", "satellite","AOD_470","AOD_550_maiac","uncert", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
        
      }else{
        #The output file is created with co-located MAIAC and AERONET data.
        out_data <- data.frame(mean(table_dif[,5],  na.rm=TRUE),
                             median(table_dif[,5],  na.rm=TRUE),
                             sd(table_dif[,5], na.rm=TRUE), (dim_table[1]))
        names(out_data) <- c("mean", "mediana","sd","dim")
        df <- data.frame() 
        #df <- data.frame(data_maiac[i,2],data_maiac[i,16], data_maiac[i,10:13], substr(table_dif[1,1],1,10),out_data[,1:4])
        df <- data.frame(data_maiac[i,1],data_maiac[i,14], data_maiac[i,9:12], substr(table_dif[1,1],1,10),out_data[,1:4])
        names(df) <- c("Date_MODIS","timestamp", "satellite","AOD_470","AOD_550_maiac","uncert", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
      }
      AOD <- rbind(AOD, df)
      
      names(AOD) <- c("Date_MODIS","timestamp", "satellite","AOD_470","AOD_550_maiac","uncert", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
      AOD <- AOD[complete.cases(AOD),]
    }
  }
  return(AOD)
}


######     -------  EXAMPLE for one station     -------  ######

buffer_time <- 60 #minutes
# BA
data_maiac_BA <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/BA-25KM-MAIAC.csv"
data_maiac_BA <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/BA-25KM-MAIAC_C61.csv"
data_aeronet_BA <-"D:/Josefina/paper_git/paper_maiac/datasets/aeronet/datasets_interp_s_v2/3_BA_2015-2022_interp-s.csv"
combinate_BA <- time_correlation (path_aeronet=data_aeronet_BA,path_maiac=data_maiac_BA,time_buffer=buffer_time)
# Save the file with co-located data from AERONET and MAIAC on local path
write.csv (combinate_BA,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/tot/3_BA-25KM-MAIAC-60-AER.csv")

# SP
data_maiac_SP <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/SP-25KM-MAIAC.csv"
data_maiac_SP <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/SP-25KM-MAIAC_C61.csv"
data_aeronet_SP <-"D:/Josefina/paper_git/paper_maiac/datasets/aeronet/datasets_interp_s_v2/1_SP_2015-2022_interp-s.csv"
combinate_SP <- time_correlation (path_aeronet=data_aeronet_SP,path_maiac=data_maiac_SP,time_buffer=buffer_time)
write.csv (combinate_SP,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/SP-25KM-MAIAC-60-AER.csv")


# ST
data_maiac_ST <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/ST-25KM-MAIAC.csv"
data_maiac_ST <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/ST-25KM-MAIAC_C61.csv"
data_aeronet_ST <-"D:/Josefina/paper_git/paper_maiac/datasets/aeronet/datasets_interp_s_v2/2_ST_2015-2022_interp-s.csv"
combinate_ST <- time_correlation (path_aeronet=data_aeronet_ST,path_maiac=data_maiac_ST,time_buffer=buffer_time)
write.csv (combinate_ST,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/ST-25KM-MAIAC-60-AER.csv")

# MD
data_maiac_MD <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/MD-25KM-MAIAC.csv"
data_maiac_MD <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/MD-25KM-MAIAC_C61.csv"
data_aeronet_MD <-"D:/Josefina/paper_git/paper_maiac/datasets/aeronet/datasets_interp_s_v2/4_MD_2015-2022_interp-s.csv"
combinate_MD <- time_correlation (path_aeronet=data_aeronet_MD,path_maiac=data_maiac_MD,time_buffer=buffer_time)
write.csv (combinate_MD,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/MD-25KM-MAIAC-60-AER.csv")


# LP
data_maiac_LP <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/LP-25KM-MAIAC.csv"
data_maiac_LP <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/LP-25KM-MAIAC_C61.csv"
data_aeronet_LP <-"D:/Josefina/paper_git/paper_maiac/datasets/aeronet/datasets_interp_s_v2/5_LP_2015-2022_interp-s.csv"
combinate_LP <- time_correlation (path_aeronet=data_aeronet_LP,path_maiac=data_maiac_LP,time_buffer=buffer_time)
write.csv (combinate_LP,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/LP-25KM-MAIAC-60-AER.csv")


# MX
data_maiac_MX <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/MX-25KM-MAIAC_C61.csv"
data_aeronet_MX <-"D:/Josefina/paper_git/paper_maiac/datasets/aeronet/datasets_interp_s_v2/6_MX_2015-2022_interp-s.csv"
combinate_MX <- time_correlation (path_aeronet=data_aeronet_MX,path_maiac=data_maiac_MX,time_buffer=buffer_time)
write.csv (combinate_MX,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/MX-25KM-MAIAC-60-AER.csv")



# USA GSFC
data_maiac_GFSC <- "D:/Josefina/Proyectos/MAIAC/USA/dataset/prueba_30km_GSFC_tot.csv"
data_aeronet_GFSC <-"D:/Josefina/paper_git/paper_maiac/datasets/aeronet/USA/1_GSFC-2015-2022_interp-s.csv"
combinate_GFSC <- time_correlation (path_aeronet=data_aeronet_GFSC,path_maiac=data_maiac_GFSC,time_buffer=buffer_time)
# Save the file with co-located data from AERONET and MAIAC on local path
write.csv (combinate_GFSC,"D:/Josefina/paper_git/paper_maiac/datasets/processed/USA/GSFC-25KM-MAIAC-60-AER.csv")



# prueba_30km_CTECH_tot
data_maiac_CTECH <- "D:/Josefina/Proyectos/MAIAC/USA/dataset/prueba_30km_CTECH_tot.csv"
data_aeronet_CTECH <-"D:/Josefina/paper_git/paper_maiac/datasets/aeronet/USA/4_CalTech-2015-2022_interp-s.csv"
combinate_CTECH <- time_correlation (path_aeronet=data_aeronet_CTECH,path_maiac=data_maiac_CTECH,time_buffer=buffer_time)
# Save the file with co-located data from AERONET and MAIAC on local path
write.csv (combinate_CTECH ,"D:/Josefina/paper_git/paper_maiac/datasets/processed/USA/CTECH-25KM-MAIAC-60-AER.csv")




# prueba_30km_MDC_tot
data_maiac_MDC <- "D:/Josefina/Proyectos/MAIAC/USA/dataset/prueba_30km_MDC_tot.csv"
data_aeronet_MDC <-"D:/Josefina/paper_git/paper_maiac/datasets/aeronet/USA/3_MDScience_C_interp-s.csv"
combinate_MDC <- time_correlation (path_aeronet=data_aeronet_MDC,path_maiac=data_maiac_MDC,time_buffer=buffer_time)
# Save the file with co-located data from AERONET and MAIAC on local path
write.csv (combinate_MDC,"D:/Josefina/paper_git/paper_maiac/datasets/processed/USA/MDC-25KM-MAIAC-60-AER.csv")

# prueba_30km_MDC_tot
data_maiac_GTECH <- "D:/Josefina/Proyectos/MAIAC/USA/dataset/prueba_30km_GTECH_tot.csv"
data_aeronet_GTECH <-"D:/Josefina/paper_git/paper_maiac/datasets/aeronet/USA/2_GTech-2015-2022_interp-s.csv"
combinate_GTECH <- time_correlation (path_aeronet=data_aeronet_GTECH,path_maiac=data_maiac_GTECH,time_buffer=buffer_time)
# Save the file with co-located data from AERONET and MAIAC on local path
write.csv (combinate_GTECH,"D:/Josefina/paper_git/paper_maiac/datasets/processed/USA/GTECH-25KM-MAIAC-60-AER.csv")



# prueba_30km_MDC_tot
data_maiac_UH <- "D:/Josefina/Proyectos/MAIAC/USA/dataset/prueba_30km_UH_tot.csv"
data_aeronet_UH <-"D:/Josefina/paper_git/paper_maiac/datasets/aeronet/USA/6_UH-2015-2022_interp-s.csv"
combinate_UH <- time_correlation (path_aeronet=data_aeronet_UH,path_maiac=data_maiac_UH,time_buffer=buffer_time)
# Save the file with co-located data from AERONET and MAIAC on local path
write.csv (combinate_UH,"D:/Josefina/paper_git/paper_maiac/datasets/processed/USA/UH-25KM-MAIAC-60-AER.csv")


###############################################################################
#####
rbind_combinate_MDC <- data.frame()
combinate_MDC$date <-   as.POSIXct(strptime(combinate_MDC$Date_MODIS, format = "%Y-%m-%d", "GMT"))
combinate_MDC%>%
  group_by(date) %>%  
  group_split() ->combinate_MDC_group

for (i in 1:length(combinate_MDC_group)){
  df <- data.frame( date = combinate_MDC_group[[i]][["date"]][1],
                    AOD_550_maiac_mean = mean(combinate_MDC_group[[i]][["AOD_550_maiac"]],na.rm=T),
                    AOD_550_maiac_mean = mean(combinate_MDC_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
  rbind_combinate_MDC <- rbind(rbind_combinate_MDC,df)
  }

write.csv(rbind_combinate_MDC,"D:/Josefina/paper_git/paper_maiac/datasets/processed/USA/MDC-25KM-MAIAC-60-AER_MEAN.csv")

#########

#####
rbind_combinate_CTECH <- data.frame()
combinate_CTECH$date <-   as.POSIXct(strptime(combinate_CTECH$Date_MODIS, format = "%Y-%m-%d", "GMT"))
combinate_CTECH%>%
  group_by(date) %>%  
  group_split() ->combinate_CTECH_group

for (i in 1:length(combinate_CTECH_group)){
  df <- data.frame( date = combinate_CTECH_group[[i]][["date"]][1],
                    AOD_550_maiac_mean = mean(combinate_CTECH_group[[i]][["AOD_550_maiac"]],na.rm=T),
                    AOD_550_maiac_mean = mean(combinate_CTECH_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
  rbind_combinate_CTECH <- rbind(rbind_combinate_CTECH,df)
}

write.csv(rbind_combinate_CTECH,"D:/Josefina/paper_git/paper_maiac/datasets/processed/USA/CTECH-25KM-MAIAC-60-AER_MEAN.csv")


#####
rbind_combinate_GFSC <- data.frame()
combinate_GFSC$date <-   as.POSIXct(strptime(combinate_GFSC$Date_MODIS, format = "%Y-%m-%d", "GMT"))
combinate_GFSC%>%
  group_by(date) %>%  
  group_split() ->combinate_GFSC_group

for (i in 1:length(combinate_GFSC_group)){
  df <- data.frame( date = combinate_GFSC_group[[i]][["date"]][1],
                    AOD_550_maiac_mean = mean(combinate_GFSC_group[[i]][["AOD_550_maiac"]],na.rm=T),
                    AOD_550_maiac_mean = mean(combinate_GFSC_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
  rbind_combinate_GFSC <- rbind(rbind_combinate_GFSC,df)
}

write.csv(rbind_combinate_GFSC,"D:/Josefina/paper_git/paper_maiac/datasets/processed/USA/GFSC-25KM-MAIAC-60-AER_MEAN.csv")

#####
rbind_combinate_GTECH <- data.frame()
combinate_GTECH$date <-   as.POSIXct(strptime(combinate_GTECH$Date_MODIS, format = "%Y-%m-%d", "GMT"))
combinate_GTECH%>%
  group_by(date) %>%  
  group_split() ->combinate_GTECH_group

for (i in 1:length(combinate_GTECH_group)){
  df <- data.frame( date = combinate_GTECH_group[[i]][["date"]][1],
                    AOD_550_maiac_mean = mean(combinate_GTECH_group[[i]][["AOD_550_maiac"]],na.rm=T),
                    AOD_550_maiac_mean = mean(combinate_GTECH_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
  rbind_combinate_GTECH <- rbind(rbind_combinate_GTECH,df)
}

write.csv(rbind_combinate_GTECH,"D:/Josefina/paper_git/paper_maiac/datasets/processed/USA/GTECH-25KM-MAIAC-60-AER_MEAN.csv")



#####
rbind_combinate_UH <- data.frame()
combinate_UH$date <-   as.POSIXct(strptime(combinate_UH$Date_MODIS, format = "%Y-%m-%d", "GMT"))
combinate_UH%>%
  group_by(date) %>%  
  group_split() ->combinate_UH_group

for (i in 1:length(combinate_UH_group)){
  df <- data.frame( date = combinate_UH_group[[i]][["date"]][1],
                    AOD_550_maiac_mean = mean(combinate_UH_group[[i]][["AOD_550_maiac"]],na.rm=T),
                    AOD_550_maiac_mean = mean(combinate_UH_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
  rbind_combinate_UH <- rbind(rbind_combinate_UH,df)
}

write.csv(rbind_combinate_UH,"D:/Josefina/paper_git/paper_maiac/datasets/processed/USA/UH-25KM-MAIAC-60-AER_MEAN.csv")

#####
rbind_combinate_BA <- data.frame()
combinate_BA$date <-   as.POSIXct(strptime(combinate_BA$Date_MODIS, format = "%Y-%m-%d", "GMT"))
combinate_BA%>%
  group_by(date) %>%  
  group_split() ->combinate_BA_group

for (i in 1:length(combinate_BA_group)){
  df <- data.frame( date = combinate_BA_group[[i]][["date"]][1],
                    AOD_550_maiac_mean = mean(combinate_BA_group[[i]][["AOD_550_maiac"]],na.rm=T),
                    AOD_550_AER_mean = mean(combinate_BA_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
  rbind_combinate_BA <- rbind(rbind_combinate_BA,df)
}

write.csv(rbind_combinate_BA,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/dia/3_BA-25KM-MAIAC-60-AER_MEAN.csv")
#####
rbind_combinate_SP <- data.frame()
combinate_SP$date <-   as.POSIXct(strptime(combinate_SP$Date_MODIS, format = "%Y-%m-%d", "GMT"))
combinate_SP%>%
  group_by(date) %>%  
  group_split() ->combinate_SP_group

for (i in 1:length(combinate_SP_group)){
  df <- data.frame( date = combinate_SP_group[[i]][["date"]][1],
                    AOD_550_maiac_mean = mean(combinate_SP_group[[i]][["AOD_550_maiac"]],na.rm=T),
                    AOD_550_AER_mean = mean(combinate_SP_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
  rbind_combinate_SP <- rbind(rbind_combinate_SP,df)
}

write.csv(rbind_combinate_SP,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/dia/1_SP-25KM-MAIAC-60-AER_MEAN.csv")



#####
rbind_combinate_ST <- data.frame()
combinate_ST$date <-   as.POSIXct(strptime(combinate_ST$Date_MODIS, format = "%Y-%m-%d", "GMT"))
combinate_ST%>%
  group_by(date) %>%  
  group_split() ->combinate_ST_group

for (i in 1:length(combinate_ST_group)){
  df <- data.frame( date = combinate_ST_group[[i]][["date"]][1],
                    AOD_550_maiac_mean = mean(combinate_ST_group[[i]][["AOD_550_maiac"]],na.rm=T),
                    AOD_550_AER_mean = mean(combinate_ST_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
  rbind_combinate_ST <- rbind(rbind_combinate_ST,df)
}

write.csv(rbind_combinate_ST,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/dia/2_ST-25KM-MAIAC-60-AER_MEAN.csv")


#####
#####
rbind_combinate_LP <- data.frame()
combinate_LP$date <-   as.POSIXct(strptime(combinate_LP$Date_MODIS, format = "%Y-%m-%d", "GMT"))
combinate_LP%>%
  group_by(date) %>%  
  group_split() ->combinate_LP_group

for (i in 1:length(combinate_LP_group)){
  df <- data.frame( date = combinate_LP_group[[i]][["date"]][1],
                    AOD_550_maiac_mean = mean(combinate_LP_group[[i]][["AOD_550_maiac"]],na.rm=T),
                    AOD_550_AER_mean = mean(combinate_LP_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
  rbind_combinate_LP <- rbind(rbind_combinate_LP,df)
}
write.csv(rbind_combinate_LP,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/dia/5_LP-25KM-MAIAC-60-AER_MEAN.csv")


#####
rbind_combinate_MD <- data.frame()
combinate_MD$date <-   as.POSIXct(strptime(combinate_MD$Date_MODIS, format = "%Y-%m-%d", "GMT"))
combinate_MD%>%
  group_by(date) %>%  
  group_split() ->combinate_MD_group

for (i in 1:length(combinate_MD_group)){
  df <- data.frame( date = combinate_MD_group[[i]][["date"]][1],
                    AOD_550_maiac_mean = mean(combinate_MD_group[[i]][["AOD_550_maiac"]],na.rm=T),
                    AOD_550_AER_mean = mean(combinate_MD_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
  rbind_combinate_MD <- rbind(rbind_combinate_MD,df)
}

write.csv(rbind_combinate_MD,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/4_MD-25KM-MAIAC-60-AER_MEAN.csv")


#####
rbind_combinate_MX <- data.frame()
combinate_MX$date <-   as.POSIXct(strptime(combinate_MX$Date_MODIS, format = "%Y-%m-%d", "GMT"))
combinate_MX%>%
  group_by(date) %>%  
  group_split() ->combinate_MX_group

for (i in 1:length(combinate_MX_group)){
  df <- data.frame( date = combinate_MX_group[[i]][["date"]][1],
                    AOD_550_maiac_mean = mean(combinate_MX_group[[i]][["AOD_550_maiac"]],na.rm=T),
                    AOD_550_AER_mean = mean(combinate_MX_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
  rbind_combinate_MX <- rbind(rbind_combinate_MX,df)
}

write.csv(rbind_combinate_MX,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/6_MX-25KM-MAIAC-60-AER_MEAN.csv")
