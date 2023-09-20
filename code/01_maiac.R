

#1.0 MAIAC PROCESSING
#The goal of this code is to process MAIAC algorithm images in .HDF format
#(MCD19A2 V6.0)

#Local path
dire <- "D:/Josefina/Proyectos/MAIAC/prueba_17/datos/mex/2022/04-07-2022/" 
dire <- "D:/Josefina/Proyectos/MAIAC/" 
dire <- "D:/Josefina/Proyectos/MAIAC/2015-v6.1" 
# Local path where the .HDF files are located
id <- dir(dire, pattern = ".hdf")
#Important: be located in the path where the files are located
setwd(dire) 

for (i in 1:1){
  # Buffer function
  for (i in 1:1){
    #Location of AERONET stations
    aeronet <- data.frame(-58.50641, -34.55542) #CEILAP-BA (34.555S, 58.506W)
    #aeronet <- data.frame(-46.735, -23.561) #Sao_Paulo (23.561S, 46.735W)
    #aeronet <- data.frame(-70.662, -33.457)# santiago 33.457S, 70.662W)
    #aeronet <- data.frame(-68.066, -16.539)# La Paz 16.539S, 68.066W
    #aeronet <- data.frame(-75.578, 6.261) #Medellin ( 6.261N, 75.578W)
    #aeronet <- data.frame(-99.182, 19.334) #Mexico_City ( 19.334N, 99.182W)
    #aeronet <- data.frame(-76.83983, 38.99250) #GSFC La 38.99250° North Longi: 76.83983° West
    
    names(aeronet) <- c("Longitude", "Latitude")
    coordinates(aeronet) <- ~Longitude+Latitude
    proj4string(aeronet) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    custom.buffer <- function(p, r) {        #
      stopifnot(length(p) == 1)
      cust <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",    
                      p@coords[[2]], p@coords[[1]])
      projected <- spTransform(p, CRS(cust))                           
      buffered <- gBuffer(projected, width=r, byid=TRUE)               
      spTransform(buffered, p@proj4string)                             
    }
    # buffered_1000 <- custom.buffer(aeronet, 1000)
    # buffered_3000 <- custom.buffer(aeronet, 3000) 
    # buffered_5000 <- custom.buffer(aeronet, 5000) 
    # buffered_15000 <- custom.buffer(aeronet, 15000) 
    buffered_30000 <- custom.buffer(aeronet, 30000)
    buffered_60000 <- custom.buffer(aeronet, 60000)
    buffered_40000 <- custom.buffer(aeronet, 40000)
  }
  
  #   Funcion de QA flags
  QA2Char <- function(num) {
    
    if (!is.na(num)) {
      # Decimal to binary
      char <- paste(sapply(strsplit(paste(rev(intToBits(num))),""),`[[`,2),collapse="")
      # Extract the last 16 digits
      char <- substr(char, 17, 32)
      
      # QA array
      qa.arr <- c(substr(char, 1, 1), # 15 Reserved
                  substr(char, 2, 3), # 13-14 Aerosol Model
                  substr(char, 4, 4), # 12 Glint Mask
                  substr(char, 5, 8), # 8-11 QA AOD 
                  substr(char, 9, 11), # 5-7 Ajacency Mask
                  substr(char, 12, 13), # 3-4 Land Water Snow/Ice Mask
                  substr(char, 14, 16)) # 0-2 Cloud Mask
    } else {
      qa.arr <- rep(NA, 7)
    }
    
    return(qa.arr)
  }
  
  #  Function to obtain info AOD 470 and 550 MAIAC
  readMCD19A2 <- function(file.name, latlong.range = NULL) {
    
    # Open the HDF file and get the data sets

    #sds <- get_subdatasets(file.name) gdalUtils
    sds = gdal_subdatasets(file.name) #library star
    # Get orbit information
    info <- GDALinfo(file.name,returnScaleOffset=FALSE)
    subdataset_metadata <- attr(info,"mdata")
    orbitas<-(subdataset_metadata)[59]
    orbit <- gsub(pattern = 'Orbit_time_stamp=', replacement = '', x = orbitas) 
    # Seperate the string array by spaces
    orbit <- unlist(strsplit(orbit, split = ' ')) 
    # Remove NA
    sub.idx <- which(nchar(orbit) != 0)
    orbit <- orbit[sub.idx]
    
    #For each orbit we obtain its information
    maiac.lst <- list()
    maiac.df.tot <- data.frame()
    for (nband in 1 : length(orbit)) {
      # Convert the dataset of interest to a raster (.tiff format)
      # Optical_Depth_047
      gdal_translate(sds[1], dst_dataset = paste0('tmp047', basename(file.name), '.tiff'), b = nband) # mask is band number
      r.047 <- raster(paste0('tmp047', basename(file.name), '.tiff'))
      
      # Optical_Depth_055
      gdal_translate(sds[2], dst_dataset = paste0('tmp055', basename(file.name), '.tiff'), b = nband)
      r.055 <- raster(paste0('tmp055', basename(file.name), '.tiff'))
      
      # AOD_Uncertainty
      gdal_translate(sds[3], dst_dataset = paste0('tmpuncert', basename(file.name), '.tiff'), b = nband)
      r.uncert <- raster(paste0('tmpuncert', basename(file.name), '.tiff'))

      # AOD_QA
      gdal_translate(sds[6], dst_dataset = paste0('tmpqa', basename(file.name), '.tiff'), b = nband)
      r.qa <- raster(paste0('tmpqa', basename(file.name), '.tiff'))
      
      # Convert each variables of interest into a data frame
      df.047 <- raster::as.data.frame(r.047, xy = T)
      names(df.047)[3] <- 'AOD_047'
      df.055 <- raster::as.data.frame(r.055, xy = T)
      names(df.055)[3] <- 'AOD_055'
      df.uncert <- raster::as.data.frame(r.uncert, xy = T)
      names(df.uncert)[3] <- 'AOD_Uncertainty'
      df.qa <- raster::as.data.frame(r.qa, xy = T)
      names(df.qa)[3] <- 'AOD_QA'
      # Combine in a single data frame all the variables
      maiac.df <- data.frame(x = df.047$x, y = df.047$y, AOD_047 = df.047$AOD_047, AOD_055 = df.055$AOD_055, AOD_Uncertainty = df.uncert$AOD_Uncertainty, AOD_QA = df.qa$AOD_QA)
      
      #Delete temporary tiff files
      file.remove(dir('./', paste0('tmp047', basename(file.name), '*')))
      file.remove(dir('./', paste0('tmp055', basename(file.name), '*')))
      file.remove(dir('./', paste0('tmpuncert', basename(file.name), '*')))
      file.remove(dir('./', paste0('tmpqa', basename(file.name), '*')))
      
      #  Projection transformation 
      SINU2 <- as.character(r.047@crs)
      SINU <- as.character(r.047@srs)
      # Convert projection to WGS84
      coordinates(maiac.df) <- ~x + y
      proj4string(maiac.df) <- CRS(SINU)
      maiac.df.new <- spTransform(maiac.df, CRS('+proj=longlat +datum=WGS84'))
      maiac.df.new <- as.data.frame(maiac.df.new)
      names(maiac.df.new)[(ncol(maiac.df) + 1) : (ncol(maiac.df) + 2)] <- c('Lon', 'Lat') #le pongo los titulos lat long
      
      # cut out area of interest using lat/long
      if (!is.null(latlong.range)) {
        
        if (latlong.range[1] >= -180 & latlong.range[1] <= 180 & latlong.range[2] >= -180 & latlong.range[2] <= 180 &
            latlong.range[3] >= -90 & latlong.range[3] <= 90 & latlong.range[4] >= -90 & latlong.range[4] <= 90 &
            latlong.range[1] <= latlong.range[2] & latlong.range[3] <= latlong.range[4]) {

          maiac.df.sub <- subset(maiac.df.new, Lon >= latlong.range[1] &
                                   Lon <= latlong.range[2] &
                                   Lat >= latlong.range[3] &
                                   Lat <= latlong.range[4])
          
          
        }      
      }
      if (nrow(maiac.df.sub) > 0) { 
        # Add data
        maiac.df.sub$Year <- as.numeric(substr(orbit[nband], start = 1, stop = 4))
        maiac.df.sub$DOY <- as.numeric(substr(orbit[nband], start = 5, stop = 7))
        maiac.df.sub$Hour <- as.numeric(substr(orbit[nband], start = 8, stop = 9))
        maiac.df.sub$Minute <- as.numeric(substr(orbit[nband], start = 10, stop = 11))
        maiac.df.sub$AOD_Type <- substr(orbit[nband], start = 12, stop = 12) #si es aqua o terra
        maiac.df.sub$date <- as.numeric(substr(orbit[nband], start = 1, stop = 7))
        maiac.df.sub$hora <- as.numeric(substr(orbit[nband], start = 8, stop =11))
        maiac.df.sub$timestamp <- as.numeric(substr(orbit[nband], start = 1, stop =11))
        
        # Convert QA flags with the function
        # Convert the QA string to QA flags
        qa.flags <- t(sapply(maiac.df.sub$AOD_QA, QA2Char))
        qa.flags <- as.data.frame(qa.flags)
        names(qa.flags) <- c("QA_Reserved", "QA_Aerosolmodel", "QA_Glintmask", "QA_AOD", "QA_Adjmask", "QA_Landmask", "QA_Cloudmask")
        maiac.df.sub <- cbind(maiac.df.sub, qa.flags)
        
      }else {
        maiac.df.sub$Year <- integer()
        maiac.df.sub$DOY <- integer()
        maiac.df.sub$Hour <- integer()
        maiac.df.sub$Minute <- integer()
        maiac.df.sub$AOD_Type <- factor()
        maiac.df.sub$QA_Reserved <- factor()
        maiac.df.sub$QA_Aerosolmodel <- factor()
        maiac.df.sub$QA_Glintmask <- factor()
        maiac.df.sub$QA_AOD <- factor()
        maiac.df.sub$QA_Adjmask <- factor()
        maiac.df.sub$QA_Landmask <- factor()
        maiac.df.sub$QA_Cloudmask <- factor()
        maiac.df.sub$date <- integer()
        maiac.df.sub$hora <- integer()
        maiac.df.sub$timestamp<- integer()
      }

      maiac.df.sub_1 <- subset(maiac.df.sub, select = c(Year, DOY, Hour, Minute, Lat, Lon, AOD_Type, AOD_047, AOD_055,AOD_Uncertainty, AOD_QA,QA_Reserved,QA_Aerosolmodel, QA_Glintmask,QA_AOD, QA_Adjmask, QA_Landmask, 
                                                        QA_Cloudmask,date,hora,timestamp))
      
      #Better quality filter. QA = 0000
      maiac.df.sub_2 <- maiac.df.sub_1[maiac.df.sub_1$QA_AOD =="0000",]
      
      # Generate the output dataframe
      if ((nrow(maiac.df.sub_2))!=0){
        
        maiac.df.sub_2 <- data.frame(maiac.df.sub_2)
        
        names(maiac.df.sub_2)<-  c("Year", "DOY", "Hour", "Minute", "Lat", "Lon", "AOD_Type", "AOD_047", "AOD_055"," AOD_Uncertainty", "AOD_QA","QA_Reserved","QA_Aerosolmodel", "QA_Glintmask","QA_AOD", "QA_Adjmask", "QA_Landmask", 
                                   "QA_Cloudmask","date","hora","timestamp")#,"AOD_055_cal")

        df <- data.frame(date=maiac.df.sub_2$date[1],hora=maiac.df.sub_2$hora[1],Year=maiac.df.sub_2$Year[1],  DOY = maiac.df.sub_2$DOY[1], 
                         Hour = maiac.df.sub_2$Hour[1], Minute = maiac.df.sub_2$Minute[1], 
                         Lat = maiac.df.sub_2$Lat[1], Lon=maiac.df.sub_2$Lon[1], 
                         AOD_Type = maiac.df.sub_2$AOD_Type[1],  
                         AOD_047=round(mean(maiac.df.sub_2$AOD_047,na.rm=TRUE),5), 
                         AOD_055=round(mean(maiac.df.sub_2$AOD_055,na.rm=TRUE),5),
                         
                         AOD_Uncertainty=maiac.df.sub_2$` AOD_Uncertainty`[1] ,
                         
                         QA_AOD= as.character(maiac.df.sub_2$QA_AOD[1]),timestamp=maiac.df.sub_2$timestamp[1],
                         AOD_055_cal=round(mean(maiac.df.sub_2$AOD_055_cal,na.rm=TRUE),5))#
     
        if(is.na(maiac.df.sub_2$Year)){

          df <- data.frame(date=maiac.df.sub_1$date[1],hora=maiac.df.sub_1$hora[1], Year=maiac.df.sub_1$Year[1], 
                           DOY = maiac.df.sub_1$DOY[1], 
                           Hour = maiac.df.sub_1$Hour[1],
                           Minute = maiac.df.sub_1$Minute[1],
                           Lat ="NA", Lon="NA", 
                           AOD_Type = maiac.df.sub_1$AOD_Type[1],  
                           AOD_047="NA", 
                           AOD_055="NA", 
                          
                           AOD_Uncertainty="NA",
                           QA_AOD= "NA",timestamp=maiac.df.sub_1$timestamp[1],
                           AOD_055_cal="NA")
          
        }
        
      }else{
        

        df <- data.frame(date=maiac.df.sub_1$date[1],hora=maiac.df.sub_1$hora[1], Year=maiac.df.sub_1$Year[1], 
                         DOY = maiac.df.sub_1$DOY[1], 
                         Hour = maiac.df.sub_1$Hour[1],
                         Minute = maiac.df.sub_1$Minute[1],
                         Lat ="NA", Lon="NA", 
                         AOD_Type = maiac.df.sub_1$AOD_Type[1],  
                         AOD_047="NA", 
                         AOD_055="NA", 
                         
                         AOD_Uncertainty="NA",
                         QA_AOD= "NA",timestamp=maiac.df.sub_1$timestamp[1],
                         AOD_055_cal="NA")                                                                                                                                   
        
      }
   
      maiac.df.tot <- rbind(maiac.df.tot,df)
      
      
    }
    return(maiac.df.tot)
    
  }
}

################             RUN THE FUNCTION             ################

#Interest buffer
buf_30000 <- buffered_30000
buf_60000 <- buffered_60000
buf_40000 <- buffered_40000

hdf_df_30000 <- data.frame()

for (i in 1:length(id)){
  print(i)
  print(Sys.time())
  df_30000 <- readMCD19A2(file.name = id[1], latlong.range = c(buf_30000@bbox[1],buf_30000@bbox[3],buf_30000@bbox[2],buf_30000@bbox[4]))
  df_60000 <- readMCD19A2(file.name = id[i], latlong.range = c(buf_40000@bbox[1],buf_40000@bbox[3],buf_40000@bbox[2],buf_40000@bbox[4]))
  
  hdf_df_30000  <- rbind( hdf_df_30000 ,df_30000)
}
dire
write.csv(hdf_df_30000 ,file = "prueba_30km_mex_04-07_2022.csv")
"bd0026"