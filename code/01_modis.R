#1.0 MODIS PROCESSING

#The goal of this code is to process MODIS algorithm images in .HDF format
print("a")

#Local path


dire <- paste("D:/Josefina/Proyectos/MODIS/MOD/arg/2022/07-11_2022/" ,sep="/")
# Local path where the .HDF files are located
id <- dir(dire, pattern = ".hdf")
#Important: be located in the path where the files are located
setwd(dire) 

for (i in 1:1){
  #Location of AERONET stations
  aeronet <- data.frame(-58.50641, -34.55542) #CEILAP-BA (34.555S, 58.506W)
  #aeronet <- data.frame(-46.735, -23.561) #Sao_Paulo (23.561S, 46.735W)
  #aeronet <- data.frame(-70.662, -33.457)# santiago 33.457S, 70.662W)
  #aeronet <- data.frame(-68.066, -16.539)# La Paz 16.539S, 68.066W
  #aeronet <- data.frame(-75.578, 6.261) #Medellin ( 6.261N, 75.578W)
  #aeronet <- data.frame(-99.182, 19.334) #Mexico_City ( 19.334N, 99.182W)
  
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
  buffered_1000 <- custom.buffer(aeronet, 1000)
  buffered_3000 <- custom.buffer(aeronet, 3000) 
  buffered_5000 <- custom.buffer(aeronet, 5000) 
  buffered_15000 <- custom.buffer(aeronet, 15000) 
  buffered_30000 <- custom.buffer(aeronet, 25000)
}

##########                MODIS FUNCTION                ##########
readMODIS <- function(file, latlong.range = NULL, border.shp = NULL) {
  
  modis.df.tot <- data.frame()
  # Open the HDF file and get the data sets
  sds <- get_subdatasets(file)
  # Date
  day <- substr(file,11,17)
  hour <- substr(file,19,22)
  data <- paste(day, hour, sep = " ")
  
  date<- as.POSIXct(strptime(data, format = "%Y%j %H%M", "GMT"))
  # Convert the dataset of interest to a raster (.tiff format)
  #Image_Optical_Depth_Land_And_Ocean" 
  gdal_translate(sds[12], dst_dataset = paste0('tmp550', basename(file), '.tiff'))#, b = nband) # mask is band number
  r.550 <- raster(paste0('tmp550', basename(file), '.tiff'))
  # Convert into a data frame
  df.550 <- raster::as.data.frame(r.550, xy = T)
  names(df.550)[3] <- 'AOD_550'
  
  # Land_Ocean_Quality_Flag == 3 
  gdal_translate(sds[8], dst_dataset = paste0('tmpqa', basename(file), '.tiff'))#, b = nband)
  r.qa <- raster(paste0('tmpqa', basename(file), '.tiff'))
  # Convert into a data frame
  df.qa <- raster::as.data.frame(r.qa, xy = T)
  names(df.qa)[3] <- 'AOD_qa'
  
  #Latitude
  gdal_translate(sds[52], dst_dataset = paste0('tmplat', basename(file), '.tiff'))#, b = nband) # mask is band number
  r.lat <- raster(paste0('tmplat', basename(file), '.tiff'))
  latitude <- rasterToPoints(r.lat)  
  colnames(latitude) <- c("x", "y", "Lat")
  # Convert into a data frame
  latitude <- as.data.frame (latitude)
  
  #longitude
  gdal_translate(sds[53], dst_dataset = paste0('tmplon', basename(file), '.tiff'))#, b = nband)
  r.lon <- raster(paste0('tmplon', basename(file), '.tiff'))
  longitude <- rasterToPoints(r.lon)  
  colnames(longitude) <- c("x", "y", "Lon")
  # Convert into a data frame
  longitude <- as.data.frame (longitude)
  
  # Combine in a single data frame all the variables
  modis.df.new <- data.frame(date=date,Lon = longitude$Lon, Lat = latitude$Lat, AOD_550 = df.550$AOD_550, AOD_QA = df.qa$AOD_qa)
  
  # Mask 
  modis.df.new$AOD_QA[ modis.df.new$AOD_QA ==-9999] <- NA   
  modis.df.new$AOD_QA[ modis.df.new$AOD_QA ==0] <- NA       
  modis.df.new$AOD_QA[ modis.df.new$AOD_QA ==1] <- NA         
  modis.df.new$AOD_QA[ modis.df.new$AOD_QA==2] <- NA     
  
  #Delete temporary tiff files
  file.remove(dir('./', paste0('tmp550', basename(file), '*')))
  file.remove(dir('./', paste0('tmpqa', basename(file), '*')))
  file.remove(dir('./', paste0('tmplon', basename(file), '*')))
  file.remove(dir('./', paste0('tmplat', basename(file), '*')))
  
  
  # cut out area of interest using lat/long
  if (!is.null(latlong.range)) { 
    
    if (latlong.range[1] >= -180 & latlong.range[1] <= 180 & latlong.range[2] >= -180 & latlong.range[2] <= 180 &
        latlong.range[3] >= -90 & latlong.range[3] <= 90 & latlong.range[4] >= -90 & latlong.range[4] <= 90 &
        latlong.range[1] <= latlong.range[2] & latlong.range[3] <= latlong.range[4]) {
      
      
      
      modis.df.sub <- subset(modis.df.new, Lon >= latlong.range[3] &
                               Lon <= latlong.range[4] &
                               Lat >= latlong.range[1] &
                               Lat <= latlong.range[2])
    } 
  }
  
  # Add data
  if (nrow(modis.df.sub) > 0) { 
    
    
    modis.df.sub$date <- modis.df.sub$date 
    modis.df.sub$Lon <- modis.df.sub$Lon
    modis.df.sub$Lat <- modis.df.sub$Lat
    modis.df.sub$AOD_550 <- modis.df.sub$AOD_550
    modis.df.sub$AOD_QA <- modis.df.sub$AOD_QA
  }else {
    #Else NULL
    modis.df.sub$date <- integer()
    modis.df.sub$Lon<- integer()
    modis.df.sub$Lat <- integer()
    modis.df.sub$AOD_550 <- integer()
    modis.df.sub$AOD_QA <- integer()
  }
  
  # Generate the output dataframe
  modis.df.sub$day <- substr(modis.df.sub$date,1,10)
  modis.df.sub$hour <- substr(modis.df.sub$date,12,21)
  df <- data.frame(day=modis.df.sub$day[1], 
                   hour=modis.df.sub$hour[1],
                   AOD=round(mean(modis.df.sub$AOD_550,na.rm=TRUE),5),
                   n = length(modis.df.sub$AOD_550),
                   null = sum(is.na(modis.df.sub$AOD_550)))
  modis.df.tot <- rbind(modis.df.tot,df)
  names(modis.df.tot) <- c("day", "hour","AOD","n","null") 
  return(modis.df.tot)
}