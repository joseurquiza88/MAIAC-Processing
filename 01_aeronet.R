
#The objective of this code is to interpolate the measurements of each AERONET 
# station to the wavelength provided by MAIAC-MODIS

### Function for estimating AOD550 from the Angstrom coefficient
path <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/datasets_interp_s_L02/original/"
path_write="D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/datasets_interp_s_L02/Latam/"
interpolate <- function(path,path_write){
  df <- data.frame()
  setwd(path)
  archive <- dir(path, pattern = ".csv")
  for(i in 1:length(archive)){
    print(i)
    # Read the files of the entered path
    data <- read.csv(archive[i],  header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE, na.strings = "-999")
    #Date is placed in the proper format
    data$date <- strptime(paste(data$Date.dd.mm.yyyy., data$Time.hh.mm.ss., sep=" "), format="%d:%m:%Y %H:%M", tz="GMT") 
    # The name of the current file is taken to be saved with the same name.
    name <- substr(archive[i],1,12)
    
    # Calculation of alpha, SEGUN WENMIN QUIN 2021
    #data$alfa <- -(log(data$AOD_440nm/data$AOD_675nm) )/log(440/675)
    #data$aod_550 <- data$AOD_440nm * (550/440)**data$alfa

    # Calculation of AOD550
    #data$aod_550 <- data$AOD_675nm*(500/675)**(-(data$Angstrom_Exponent_440_675))# change name BO-COLUMN
    #Create a new dataframe
    # data$aod_550 <-  data$AOD_500nm * ((550/500)**(-data$Angstrom_Exponent_440_675))
    data$aod_550 <-  data$AOD_500nm * ((550/500)**(-data$X440.675_Angstrom_Exponent))
    
    df <- data.frame (data$date,data$aod_550)
    # Columns name
    names(df) <- c("date", "aod_550")
    # The new dataframe is saved in the entered path as "path_write"
    write.csv(df,(paste(path_write,name,"_interp-a.csv",sep="")) , row.names = FALSE)
    
  }
  
}
# Run the function
interpolate(path,path_write)


################################################################################
###   Function for estimating AOD550 - QUADRATIC INTERPOLATION FUNCTION      ##
# This function performs an interpolation taking 3 points,
# based on the Lagrange polynomials.

interpolate_s <- function(path,path_write){
  df <- data.frame()
  # Local path archive
  setwd(path)
  archive <- dir(path, pattern = ".csv")
  interpol_cuad <- function(x, x0, y0, x1, y1, x2, y2){ 
    a = ((x - x1)*(x-x2))/((x0-x1)*(x0-x2))
    b = ((x - x0)*(x-x2))/((x1-x0)*(x1-x2))
    c = ((x - x0)*(x-x1))/((x2-x0)*(x2-x1))
    y = (y0*a) + (y1*b) + (y2*c)
    return(y)
  }
  for(i in 1:length(archive)){
    print(i)
    # Read the files of the entered path
    data <- read.csv(archive[i],  header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE, na.strings = "-999")
    #Date is placed in the proper format
    date <- strptime(paste(data$Date.dd.mm.yyyy., data$Time.hh.mm.ss., sep=" "), format="%d:%m:%Y %H:%M", tz="GMT") 
    # The name of the current file is taken to be saved with the same name.
    name <- substr(archive[i],1,12)
    
    
    #Unknown value
    x = log(550)
    # First interpolation
    #Point 1
    x0= log(440)
    y0= log(data$AOD_440nm)
    #Point 2
    x1= log(500)    
    y1=log(data$AOD_500nm) 
    #Point 3
    x2= log(675) #500        
    y2= log(data$AOD_675nm)
    # Function 1
    y <- exp(interpol_cuad(x, x0, y0, x1, y1, x2, y2))
    
    #A dataframe is generated with the first interpolation
    data_aeronet <- data.frame(date, y)
    names(data_aeronet) <- c("date", "AOT_550")
    
    # Second interpolation
    x2= log(870)
    y2= log(data$AOD_870nm)
    # Put de information into the dataframe
    data_aeronet$AOT_550_2  <- exp(interpol_cuad(x, x0, y0, x1, y1, x2, y2))
    
    #Third interpolation
    x2= log(1020)
    y2= log(data$AOD_1020nm)
    # Put de information into the dataframe
    data_aeronet$AOT_550_3 <- exp(interpol_cuad(x, x0, y0, x1, y1, x2, y2))
    # Mean of the three interpolations
    data_aeronet$AOT_550_mod <- rowMeans(data_aeronet[,2:4], na.rm = TRUE)
    # Columns name
    names(data_aeronet) <- c("date", "aod_550","aod_550_2","aod_550","aod_550_mod")
    # The new dataframe is saved in the entered path as "path_write"
    write.csv(data_aeronet,(paste(path_write,name,"_interp-s.csv",sep="")) , row.names = FALSE)
    
  }
  
}


interpolate(path="D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets",path_write="D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets_interp/")

interpolate_s (path="D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets",path_write="D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets_interp_s_v2/")

interpolate_s (path="D:/Josefina/paper_git/paper_maiac/datasets/aeronet/datasets",path_write="D:/Josefina/paper_git/paper_maiac/datasets/aeronet/datasets_interp_s_v2/")

interpolate (path="D:/Josefina/Proyectos/aeronet/datos/GSFC",path_write="D:/Josefina/Proyectos/aeronet/datos/GSFC/interpolado/")
interpolate_s (path="D:/Josefina/Proyectos/aeronet/datos/GSFC",path_write="D:/Josefina/Proyectos/aeronet/datos/GSFC/interpolado/")
interpolate_s (path="D:/Josefina/Proyectos/aeronet/datos/MD_Science_Center",path_write="D:/Josefina/Proyectos/aeronet/datos/MD_Science_Center/interpolado/")

# VERSION 02
interpolate(path="D:/Josefina/Proyectos/aeronet/datos/AERONET_02112023_L02/original",path_write="D:/Josefina/Proyectos/aeronet/datos/AERONET_02112023_L02/interpolate")

interpolate_s (path="D:/Josefina/Proyectos/aeronet/datos/AERONET_02112023_L02/original",path_write="D:/Josefina/Proyectos/aeronet/datos/AERONET_02112023_L02/interpolate/")
