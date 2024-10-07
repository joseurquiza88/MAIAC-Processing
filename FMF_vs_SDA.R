# set de datos
#FMF
# https://aeronet.gsfc.nasa.gov/cgi-bin/webtool_aod_v3?stage=3&region=United_States_East&state=Maryland&site=GSFC&place_code=10&if_polarized=0
# SSA
#https://aeronet.gsfc.nasa.gov/cgi-bin/webtool_inv_v3?stage=3&region=South_America&state=Brazil&site=Sao_Paulo&place_code=10


fmf <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/FMF/originales/01-SP-FMF.csv",skip=6,na.strings = -999)
#fmf <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/FMF/01-GS-FMF.csv",na.strings = -999)

fmf$date  <- strptime(fmf$Date_.dd.mm.yyyy., format = "%d:%m:%Y")
fmf %>%
  group_by(date) %>%  
  group_split() ->combinate_fmf
rbind_combinate_fmf <- data.frame()
for (i in 1:length(combinate_fmf)){
  df <- data.frame( date = combinate_fmf[[i]][["date"]][1],
                    fmf_mean = mean(combinate_fmf[[i]][["FineModeFraction_500nm.eta."]],na.rm=T)
                    )
  rbind_combinate_fmf <- rbind(rbind_combinate_fmf,df)
}
rbind_combinate_fmf <- rbind_combinate_fmf[complete.cases(rbind_combinate_fmf$fmf_mean),]

###############
#SDA

SDA <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/SSA/originales/01-SP-SSA-B.csv",na.strings = -999)
#SDA <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/SDA/01-GS-SDA.csv",na.strings = -999)

SDA$date  <- strptime(SDA$Date.dd.mm.yyyy., format = "%d:%m:%Y")
SDA %>%
  group_by(date) %>%  
  group_split() ->combinate_SDA
rbind_combinate_SDA <- data.frame()
for (i in 1:length(combinate_SDA)){
  df <- data.frame( date = combinate_SDA[[i]][["date"]][1],
                    SDA_mean = mean(combinate_SDA[[i]][["Single_Scattering_Albedo.440nm."]],na.rm=T)
  )
  rbind_combinate_SDA <- rbind(rbind_combinate_SDA,df)
}
rbind_combinate_SDA <- rbind_combinate_SDA[complete.cases(rbind_combinate_SDA$SDA_mean),]
merge_sat <- merge(x = rbind_combinate_fmf, y = rbind_combinate_SDA, by = "date", all = TRUE) # Equivalente
merge_sat$month <- month(merge_sat$date)
# winter, June-July-August; spring,
# September-October-November; summer, December-
# January-February; fall, March-April-May.
merge_sat$season <-0
for(i in 1:length(merge_sat$month)){
  if(merge_sat$month[i] == 1 |merge_sat$month[i] == 2 |merge_sat$month[i] == 12){
    merge_sat$season[i] <- "Summer"
  }
  else if(merge_sat$month[i] == 3 |merge_sat$month[i] == 4 |merge_sat$month[i] == 5){
    merge_sat$season[i] <- "Fall"
  }
  
  else if(merge_sat$month[i] == 9 |merge_sat$month[i] == 10 |merge_sat$month[i] == 11){
    merge_sat$season[i] <- "Spring"
  }
  else if(merge_sat$month[i] == 6 |merge_sat$month[i] == 7 |merge_sat$month[i] == 8){
    merge_sat$season[i] <- "Winter"
  }
  else{
    merge_sat$season[i] <- "aaaaaaaaaaa"
    
  }
}

##########################################
SP_plot<- # Crea el gráfico de dispersión con ggplot2
  ggplot() +
  geom_point(merge_sat, mapping=aes(y = fmf_mean, x = SDA_mean , shape = season,color=season)) +#alpha =0.8,stroke=5,size=1
  scale_color_manual(values = c("Spring" = "green", "Summer" = "red", "Fall" = "blue", "Winter" = "black")) +
  scale_shape_manual(values = c("Spring" = 12, "Summer" = 2, "Fall" = 3, "Winter" = 8)) +  # Formas con bordes
  scale_y_continuous(limits=c(0.4, 1.),breaks = c(0.4,0.5,0.6,0.7,0.8,0.9,1))+
  scale_x_continuous(limits=c(0.7, 1.),breaks = c(0.7,0.75,0.8,0.85,0.9,0.95,1))+
  geom_vline(xintercept = 0.95, linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0.4, linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0.6, linetype = "dashed", color = "black")+
  geom_segment(aes(x = 0.85, xend = 0.85, y = 0.6, yend = Inf), 
               linetype = "dashed", color = "black")+
  geom_segment(aes(x = 0.9, xend = 0.9, y = 0.6, yend = Inf), 
               linetype = "dashed", color = "black")+
  geom_text(aes(x =0.75 , y =0.62 , label = "FH-A"),size=3) +
  geom_text(aes(x =0.87 , y =0.62 , label = "FM-A"),size=3) +
  geom_text(aes(x =0.93 , y =0.62 , label = "FS-A"),size=3) +
  geom_text(aes(x =0.97 , y =0.62 , label = "FN-A"),size=3) +
  geom_text(aes(x =0.75 , y =0.42 , label = "M-A"),size=3) +
  geom_text(aes(x =0.97 , y =0.42 , label = "MN-A"),size=3) +
  theme_minimal() +
  labs(title = "SP",
       x = expression(SDA[440]),
       y = "FMF",
       color = "Season",
       shape = "Season")+ theme_bw()+ theme(legend.position = "none")   # Eliminar la leyenda
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size =10))
#theme_minimal()
GS_plot
SP_plot <- P
plot_tot2 <- grid.arrange(SP_plot,GS_plot2,ncol=2)#,heights=c(2,0.7))
plot_tot2
GS_plot2<- GS_plot+ theme(legend.position = "none")

############################################################
# FUNCION MERGE FMF vs SDA
caracteristicas_aerosoles <- function(station, num_station,region){
  df_fmf <- paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/FMF/originales/",num_station,"-",station,"-FMF.csv",sep="")

  df_sda <- paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/SSA/originales/",num_station,"-",station,"-SSA.csv",sep="")
  
  fmf <- read.csv(df_fmf, sep=",",  na.strings = -999)
  SDA <- read.csv(df_sda, sep=",",  na.strings = -999)
  fmf$date  <- strptime(fmf$Date_.dd.mm.yyyy., format = "%d:%m:%Y")
  fmf %>%
    group_by(date) %>%  
    group_split() ->combinate_fmf
  rbind_combinate_fmf <- data.frame()
  for (i in 1:length(combinate_fmf)){
    df <- data.frame( date = combinate_fmf[[i]][["date"]][1],
                      fmf_mean = mean(combinate_fmf[[i]][["FineModeFraction_500nm.eta."]],na.rm=T)
    )
    rbind_combinate_fmf <- rbind(rbind_combinate_fmf,df)
  }
  rbind_combinate_fmf <- rbind_combinate_fmf[complete.cases(rbind_combinate_fmf$fmf_mean),]
  
  ###############
  #SDA
  SDA$date  <- strptime(SDA$Date.dd.mm.yyyy., format = "%d:%m:%Y")
  SDA %>%
    group_by(date) %>%  
    group_split() ->combinate_SDA
  rbind_combinate_SDA <- data.frame()
  for (i in 1:length(combinate_SDA)){
    df <- data.frame( date = combinate_SDA[[i]][["date"]][1],
                      SDA_mean = mean(combinate_SDA[[i]][["Single_Scattering_Albedo.440nm."]],na.rm=T)
    )
    rbind_combinate_SDA <- rbind(rbind_combinate_SDA,df)
  }
  rbind_combinate_SDA <- rbind_combinate_SDA[complete.cases(rbind_combinate_SDA$SDA_mean),]
  merge_sat <- merge(x = rbind_combinate_fmf, y = rbind_combinate_SDA, by = "date", all = TRUE) # Equivalente
  merge_sat$month <- month(merge_sat$date)
  # winter, June-July-August; spring,
  # September-October-November; summer, December-
  # January-February; fall, March-April-May.
  merge_sat$season <-0
  if(region == "Latam"){
    for(i in 1:length(merge_sat$month)){
      if(merge_sat$month[i] == 1 |merge_sat$month[i] == 2 |merge_sat$month[i] == 12){
        merge_sat$season[i] <- "Summer"
      }
      else if(merge_sat$month[i] == 3 |merge_sat$month[i] == 4 |merge_sat$month[i] == 5){
        merge_sat$season[i] <- "Fall"
      }
      
      else if(merge_sat$month[i] == 9 |merge_sat$month[i] == 10 |merge_sat$month[i] == 11){
        merge_sat$season[i] <- "Spring"
      }
      else if(merge_sat$month[i] == 6 |merge_sat$month[i] == 7 |merge_sat$month[i] == 8){
        merge_sat$season[i] <- "Winter"
      }
      else{
        merge_sat$season[i] <- "aaaaaaaaaaa"
        
      }
    }
  }
    else if(region == "USA"){
        for(i in 1:length(merge_sat$month)){
          if(merge_sat$month[i] == 1 |merge_sat$month[i] == 2 |merge_sat$month[i] == 12){
            merge_sat$season[i] <- "Winter"
          }
          else if(merge_sat$month[i] == 3 |merge_sat$month[i] == 4 |merge_sat$month[i] == 5){
            merge_sat$season[i] <- "Spring"
          }
          
          else if(merge_sat$month[i] == 9 |merge_sat$month[i] == 10 |merge_sat$month[i] == 11){
            merge_sat$season[i] <- "Fall"
          }
          else if(merge_sat$month[i] == 6 |merge_sat$month[i] == 7 |merge_sat$month[i] == 8){
            merge_sat$season[i] <- "Summer"
          }
          else{
            merge_sat$season[i] <- "aaaaaaaaaaa"
            
          }
        }
    }
      else{
        print("not region")
      }
  return(merge_sat)
  # name <- paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/AER_merge_SSA-FMF/",region,"/",num_station,"-",station,"-merge-SSA-FMF.csv",sep="")
  # 
  # write.csv(merge_sat,name)
  # 
}
#SDA <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/SDA/01-GS-SDA.csv",na.strings = -999)
station<-"LP"
num_station<-"05"
region <- "Latam"
a<-caracteristicas_aerosoles(station, num_station,region)

####################################################################
#####################################################################
##########################################
merge_sat = read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/AER_merge_SSA-FMF/Latam/05-LP-merge-SSA-FMF.csv")

plot<- # Crea el gráfico de dispersión con ggplot2
  ggplot() +
  geom_point(merge_sat, mapping=aes(y = fmf_mean, x = SDA_mean , shape = season,color=season)) +#alpha =0.8,stroke=5,size=1
  scale_color_manual(values = c("Spring" = "green", "Summer" = "red", "Fall" = "blue", "Winter" = "black")) +
  scale_shape_manual(values = c("Spring" = 12, "Summer" = 2, "Fall" = 3, "Winter" = 8)) +  # Formas con bordes
  scale_y_continuous(limits=c(0.4, 1.),breaks = c(0.4,0.5,0.6,0.7,0.8,0.9,1))+
  scale_x_continuous(limits=c(0.7, 1.),breaks = c(0.7,0.75,0.8,0.85,0.9,0.95,1))+
  geom_vline(xintercept = 0.95, linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0.4, linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0.6, linetype = "dashed", color = "black")+
  geom_segment(aes(x = 0.85, xend = 0.85, y = 0.6, yend = Inf), 
               linetype = "dashed", color = "black")+
  geom_segment(aes(x = 0.9, xend = 0.9, y = 0.6, yend = Inf), 
               linetype = "dashed", color = "black")+
  geom_text(aes(x =0.75 , y =0.62 , label = "FH-A"),size=3) +
  geom_text(aes(x =0.87 , y =0.62 , label = "FM-A"),size=3) +
  geom_text(aes(x =0.93 , y =0.62 , label = "FS-A"),size=3) +
  geom_text(aes(x =0.97 , y =0.62 , label = "FN-A"),size=3) +
  geom_text(aes(x =0.75 , y =0.42 , label = "M-A"),size=3) +
  geom_text(aes(x =0.97 , y =0.42 , label = "MN-A"),size=3) +
  theme_minimal() +
  labs(title = "SP",
       x = expression(SDA[440]),
       y = "FMF",
       color = "Season",#theme(legend.position = "none")   # Eliminar la leyenda
       shape = "Season")+ theme_bw()+ 
theme(legend.title = element_text(#family = "Roboto",
  
  size = 14,
  face = 2))+
  theme(legend.text = element_text(size =10))
#theme_minimal()
plot
SP_plot <- P
plot_tot2 <- grid.arrange(SP_plot,GS_plot2,ncol=2)#,heights=c(2,0.7))
plot_tot2
GS_plot2<- GS_plot+ theme(legend.position = "none")