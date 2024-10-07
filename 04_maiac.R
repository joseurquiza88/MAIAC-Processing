
####                ---  STATISTICAL FUNCTIONS  --- ######
funcion_estaditicas <- function(m,o, type){
  m <- m
  o <- o
  tabla <- data.frame()
    if(type == "bias"){
    resta<- m - o 
    suma <- sum(resta)
    n <- length(resta)
    bias <- round((suma/n),5)
    
    return(bias)
  }
  
  
  if (type == "rmse"){
    rmse <- sqrt(mean((m - o)^2,na.rm=T))
    return(round(rmse,5))
    
    
  }
  if (type=="nrmse_mm"){
    nrmse_mm <- (sqrt(mean((m - o)^2)))/ diff(range(o, na.rm = TRUE))
    return(round(nrmse_mm,5))
  }
  
  if(type=="nrmse_sd"){
    nrmse_sd <- (sqrt(mean((m - o)^2))) / sd(o)
    return(round(nrmse_sd,5))
  }
  if(type=="nrmse_mean"){
    nrmse_mean <- (sqrt(mean((m - o)^2))) / mean(o)
    return(round(nrmse_mean,5))
  }
  if(type=="nmbe_mm"){
    resta<- m - o 
    suma <- sum(resta)
    n <- length(resta)
    bias <- suma/n
    nmbe_mm <- bias/ diff(range(o, na.rm = TRUE))
    return(round(nmbe_mm,5))
  }
  if(type=="nmbe_mean"){
    resta<- m - o 
    suma <- sum(resta)
    n <- length(resta)
    bias <- suma/n
    nmbe_mean <- bias/ mean(o)
    return(round(nmbe_mean,5))
  }
  if(type=="mean_obs"){
    mean_obs <- mean(o,na.rm=T)
    return(round(mean_obs,5))
  }
  
  if(type=="mean_mod"){
    mean_mod <- mean(m,na.rm=T)
    return(round(mean_mod,5))
  }
  
  if(type == "long_o"){
    long_o <- length(o)
    return(long_o)
  }
  
  if(type == "long_m"){
    long_m <- length(m)
    return(long_m)
  }
  
  if (type == "r2_aj"){
    modelo_lineal <- lm(m ~ o) #
    r2_aj <- round((as.numeric(summary(modelo_lineal)[9])),5)
    return(r2_aj)
  }
  
  if (type == "r"){
    r <- round((as.numeric(cor.test(m,o)[4])),5)
    return(r)
  }
  if (type == "sd_mod"){
    sd_mod <- sd(m)
    return(sd_mod )
  }
  if (type == "sd_obs"){
    sd_obs <- sd(o)
    return(sd_obs)
  }
  if (type == "crmsd"){
    m_mean = mean(m)
    o_mean = mean(o)
    crmsd_1 = ((m - m_mean)-(o - o_mean))**2
    crmsd_2 = sqrt(sum(crmsd_1)/ length(m))
    crmsd = round((crmsd_2/o_mean),4)
    #if (norm == TRUE){
    
    return(crmsd)
  }
  if (type == "intercep"){
    modelo_lineal <- lm(m ~ o) #
    cf <- coef(modelo_lineal)
    intercept <- cf[1]
    
    return(intercept)
  }
  if (type == "slope"){
    modelo_lineal <- lm(m ~ o) #
    cf <- coef(modelo_lineal)
    slope <- cf[2]
    
    return(slope)
  }
  
  
  if(type=="tabla"){

    type_1 <-  funcion_estaditicas(m,o,"mean_mod")
    names(type_1) <- "mean modelado"
    
    type_2 <-  funcion_estaditicas(m,o,"mean_obs")
    names(type_2) <- "mean obs"
    
    type_3 <-  funcion_estaditicas(m,o,"long_o")
    names(type_3) <- "len obs"
    
    type_4 <-  funcion_estaditicas(m,o,"long_m")
    names(type_4) <- "len mod"
    
    type_5 <-  funcion_estaditicas(m,o,"rmse")
    names(type_5) <- "rmse"
    
    type_6 <-  funcion_estaditicas(m,o,"nrmse_mean")
    names(type_6) <- "nrmse mean"
    
    type_7 <-  funcion_estaditicas(m,o,"bias")
    names(type_7) <- "bias"
    
    type_8 <-  funcion_estaditicas(m,o,"nmbe_mean")
    names(type_8) <- "nbias mean"

    type_9 <-  funcion_estaditicas(m,o,"r")
    names(type_9) <- "r"
    
    type_10 <-  funcion_estaditicas(m,o,"r2_aj")
    names(type_10) <- "R^2 aj"
    
    type_11 <-  funcion_estaditicas(m,o,"sd_mod")
    names(type_11) <- "sd_mod"
    
    type_12 <-  funcion_estaditicas(m,o,"sd_obs")
    names(type_12) <- "sd_obs"
    
    
    type_13 <-  funcion_estaditicas(m,o,"crmsd")
    names(type_13) <- "crmsd"
    
    type_14 <-  funcion_estaditicas(m,o,"intercep")
    names(type_14) <- "intercep"
    
    type_15 <-  funcion_estaditicas(m,o,"slope")
    names(type_15) <- "slope"
    
    # Final data frame 
    df <- cbind(type_1,type_2,type_3,type_4,type_5,type_6,type_7,type_8,
                type_9, type_10,type_11,type_12,type_13,type_14,type_15)
    colnames(df) <- c(names(type_1),names(type_2),names(type_3),
                      names(type_4), names(type_5), names(type_6),names(type_7),
                      names(type_8),names(type_9),names(type_10),names(type_11),
                      names(type_12),names(type_13),names(type_14),names(type_15))
 
    return(df)
  }
}

###########################################################################
# Con esta funcion recorremos y obtenemos las estadisticas de todas las carpetas


funcion_estadisticas_carpeta <- function(dire){
  setwd(dire)
  id <- dir(dire, pattern = ".csv")
  df <- data.frame()
  for (x in 1:length(id)){
    print(x)
    data <- read.csv(id[x])
    info <- funcion_estaditicas(m=data$AOD_550_maiac,o=data$AOD_550_AER_mean, type="tabla")
    name <- id[x]
    df_info <- data.frame(name,info)
    df <- rbind(df,df_info)
  }
  return(df)
}

dire_1 <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/USA_C61/tot/1km/"
dire_3 <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/USA_C61/tot/3km/"
dire_5 <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/USA_C61/tot/5km/"
dire_15 <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/USA_C61/tot/15km/"
dire_25 <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/USA_C61/tot/25km/"

df_1 <- funcion_estadisticas_carpeta(dire_1)
df_3 <- funcion_estadisticas_carpeta(dire_3)
df_5 <- funcion_estadisticas_carpeta(dire_5)
df_15 <- funcion_estadisticas_carpeta(dire_15)
df_25 <- funcion_estadisticas_carpeta(dire_25)

df_tot <- rbind(df_1,df_3, df_5, df_15,df_25)
df_tot$ciudad <- substr(df_tot$name,3,4)
df_tot$buffer <- substr(df_tot$name,6,9)
df_tot$temp <-substr(df_tot$name,17,18)
getwd()

write.csv(df_tot,"D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/USA_C60/tot/estadisticas_BTEMP-ESP-C61.csv")
########################################
#Queremos saber cual es el mejor buffer
# Minimo RMSE, Minimo Bias (abs), Maximo RMSE
id_df_output <- data.frame()
df_tot%>%
  group_by(ciudad) %>%  
  group_split() -> group_dat_ciudad
df_rbind <- data.frame()
#MINIMO - MAX
for(x in 1:length(group_dat_ciudad)){
  group_dat_ciudad[[x]][["name"]]
  minimo_rmse <- min(group_dat_ciudad[[x]][["rmse"]])
  minimo_bias <- min(abs(group_dat_ciudad[[x]][["bias"]]))
  
  maximo_r2 <- max(group_dat_ciudad[[x]][["R.2.aj"]])
  
  pos_rmse <- which.min(group_dat_ciudad[[x]][["rmse"]])
  pos_bias <- which.min(abs(group_dat_ciudad[[x]][["bias"]]))
  pos_r2 <- which.min(group_dat_ciudad[[x]][["R.2.aj"]])
  name_rmse <- group_dat_ciudad[[x]][["name"]][[pos_rmse]]
  name_bias <- group_dat_ciudad[[x]][["name"]][[pos_bias]]
  name_r2 <- group_dat_ciudad[[x]][["name"]][[pos_r2]]
  valor_min_bias <- group_dat_ciudad[[x]][["bias"]][[pos_bias]]
  df <- data.frame (name_rmse,minimo_rmse,valor_min_bias,name_bias,name_r2,maximo_r2,name_r2)
  
  df_rbind <- rbind(df_rbind,df)
}
df_rbind <- data.frame()
for(x in 1:length(group_dat_ciudad)){
  group_dat_ciudad[[x]][["name"]]
  minimo_rmse <- max(group_dat_ciudad[[x]][["rmse"]])
  minimo_bias <- max(abs(group_dat_ciudad[[x]][["bias"]]))
  
  maximo_r2 <- min(group_dat_ciudad[[x]][["R.2.aj"]])
  
  pos_rmse <- which.max(group_dat_ciudad[[x]][["rmse"]])
  pos_bias <- which.max(abs(group_dat_ciudad[[x]][["bias"]]))
  pos_r2 <- which.max(group_dat_ciudad[[x]][["R.2.aj"]])
  name_rmse <- group_dat_ciudad[[x]][["name"]][[pos_rmse]]
  name_bias <- group_dat_ciudad[[x]][["name"]][[pos_bias]]
  name_r2 <- group_dat_ciudad[[x]][["name"]][[pos_r2]]
  valor_min_bias <- group_dat_ciudad[[x]][["bias"]][[pos_bias]]
  df <- data.frame (name_rmse,minimo_rmse,valor_min_bias,name_bias,name_r2,maximo_r2)
  
  df_rbind <- rbind(df_rbind,df)
}



write.csv(df_rbind, "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/Latam_C61/tot/estadisticas_BTEMP-ESP-C613.csv")

