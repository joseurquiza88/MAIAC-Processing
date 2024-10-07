
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]

metricas <- metricas[metricas$region == "latam",]


metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
unique(metricas$metrica)
metrica_interes <- "r2"   


#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

a<- ggplot(metricas_subset, aes(x = estacion, y = valor, shape = temporal, color = espacial)) +
  geom_point(alpha=0.6, fill="black",size=3) +
  scale_color_manual(values = c("#c51b8a","#2ca25f", "#de2d26", "#3182bd","#756bb1")) +  # Personaliza los colores
  scale_shape_manual(values = c(15,16,17,18)) +  # Personaliza las formas (shapes)
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  
  #ylim(c(0,1),breaks = c(0,0.1,0.2,0.3))+
  #scale_y_continuous(breaks = c(0,0.1,0.2,0.3)) +
  #ylim(0,1)+
  #scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  #ylim(0,0.12)+
  #ylim(-0.05,0.065)+
  #scale_y_continuous(breaks = seq(-1, 1, by = 0.1))+
  labs(title = "MODIS Latam",
       x = "Station",
       y = expression(R^2),
       color = "Spatial window",
       shape = "Temporal window") + theme_bw()+  theme(legend.title = element_text(family = "Roboto",
                                                                                   
                                                                                   size = 14,
                                                                                   face = 2))+
  theme(legend.text = element_text(size =12))
  #theme_minimal()


a
# b<- ggplot(metricas_subset, aes(x = estacion, y = valor,size = espacial, color =temporal)) +
#   geom_point(alpha=0.6, fill="black") +
#   scale_color_manual(values = c("#c51b8a","#2ca25f", "#de2d26", "#3182bd"))+#,"#756bb1")) +  # Personaliza los colores
#   scale_shape_manual(values = c(15,16,17,18)) +  # Personaliza las formas (shapes)
#   #ylim(0,1)+
#   labs(title = metrica_interes,
#        x = "Station",
#        y = metrica_interes,
#        size = "Espacial",
#        color = "Temporal") +
#   theme_minimal()
# 
# b
# 
# c<- ggplot(metricas_subset, aes(x = estacion, y = valor,size = temporal, color =espacial )) +
#   geom_point(alpha=0.4, fill="black") +
#   scale_color_manual(values = c("#c51b8a","#2ca25f", "#de2d26", "#3182bd","#756bb1")) +  # Personaliza los colores
#   scale_shape_manual(values = c(15,16,17,18)) +  # Personaliza las formas (shapes)
#   #ylim(0,1)+
#   labs(title = metrica_interes,
#        x = "Station",
#        y = metrica_interes,
#        color = "Temporal",
#        size = "Espacial") +
#   theme_minimal()
# 
# 
# c
# d<- # Crea el gráfico de dispersión con ggplot2
#   ggplot(metricas_subset, aes(y = temporal, x = espacial, size = valor, color=estacion)) +
#   geom_point(shape=1,stroke = 0.2) +
#   labs(title = metrica_interes,
#        x = "Buffer temporal",
#        y = "Buffer espacial",
#        size = metrica_interes) +
#   theme_minimal()
# d


e<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, size = temporal, color=estacion)) +
  geom_point(alpha=0.2,shape=c("o","v","s","P","*","h")) +
  scale_color_manual(values = c("#74c476","#fed976","#fb6a4a", "#74a9cf","#df65b0","#807dba")) +  # Personaliza los colores
  #"#005a32","#fd8d3c","#99000d","#023858","#ce1256","#3f007d"
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  #ylim(0,0.12)+
  #ylim(-0.05,0.065)+
  #scale_y_continuous(breaks = seq(-1, 1, by = 0.1))+
  labs(#title = "MODIS Latam",
    x = "Spatial window (km)",
    y = expression(R^2),
    color = "Station",
    size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(family = "Roboto",
                                     
                                    size = 14,
                                    face = 2))+
  theme(legend.text = element_text(size = 12))


#theme_minimal()

e

f<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = temporal,  size =espacial,color=estacion )) +
  geom_point(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  #ylim(0,0.12)+
  #ylim(-0.05,0.065)+
  #scale_y_continuous(breaks = seq(-1, 1, by = 0.1))+
  labs(title = "MODIS USA",
    x = "Temporal window (min)",
    y = expression(R^2),
    color = "Station",
    size = "Spatial window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
                                    
                                    size = 14,
                                    face = 2))+
  theme(legend.text = element_text(size =12))
#theme_minimal()

f

#plot_grid(a, plot_grid(e, f), ncol = 1)
plot_tot2 <- grid.arrange(e,f,ncol=2)#,heights=c(2,0.7))
#plot_tot3 <- grid.arrange(a,plot_tot2,ncol=1) 
ggsave("D:/Josefina/paper_git/paper_maiac/plot/V02/buff_R01_C61-USA.png",plot_tot2,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)

ggsave("D:/Josefina/paper_git/paper_maiac/plot/V02/buff_R02_C61-USA.png",a,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)

############################################################
##                     RMSE
############################################################

metrica_interes <-"rmse" 

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

rmse<- ggplot(metricas_subset, aes(x = estacion, y = valor, shape = temporal, color = espacial))+
  geom_point(alpha=0.6, fill="black",size=3) +
  scale_color_manual(values = c("#c51b8a","#2ca25f", "#de2d26", "#3182bd","#756bb1")) +  # Personaliza los colores
  scale_shape_manual(values = c(15,16,17,18)) +  # Personaliza las formas (shapes)
  #scale_y_continuous(limits=c(0, 0.125),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  # scale_y_continuous(limits=c(0, 0.29),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2,
  #                                              0.22,0.24,0.26,0.28))+
  
  labs(title = "MODIS Latam",
    x = "Station",
    y = "RMSE",
    color = "Spatial window",
    shape = "Temporal window") + theme_bw()+  theme(legend.title = element_text(#family = "Roboto",
                                                                                
                                                                                size = 14,
                                                                                face = 2))+
  theme(legend.text = element_text(size =12))
#theme_minimal()


rmse

e<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, size = temporal, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #scale_y_continuous(limits=c(0, 0.125),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  scale_y_continuous(limits=c(0, 0.26),breaks = c(0,0.04,0.08,0.12,0.16,0.2,
                                                   0.24))+
  
  labs(title = "MAIAC C6.1 USA",
    x = "Spatial window (km)",
    y = "RMSE",
    color = "Station",
    size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
                                    
                                    size = 14,
                                    face = 2))+
  theme(legend.text = element_text(size = 12))


#theme_minimal()

e
f<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = temporal,  size =espacial,color=estacion )) +
  geom_point(shape=1,stroke = 0.2) +
  #scale_y_continuous(limits=c(0, 0.125),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  scale_y_continuous(limits=c(0, 0.36),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2,
                                                  0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36 ))+
  
  labs(title = "MODIS USA",
       x = "Temporal window (min)",
    y = "RMSE",
    color = "Station",
    size = "Spatial window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
                                    
                                    size = 14,
                                    face = 2))+
  theme(legend.text = element_text(size =12))
#theme_minimal()

f

#plot_grid(a, plot_grid(e, f), ncol = 1)
plot_tot2 <- grid.arrange(e,f,ncol=2)#,heights=c(2,0.7))
#plot_tot3 <- grid.arrange(a,plot_tot2,ncol=1) 
ggsave("D:/Josefina/paper_git/paper_maiac/plot/V02/buff_RMSE01_C61_USA.png",plot_tot2,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)

ggsave("D:/Josefina/paper_git/paper_maiac/plot/V02/buff_RMSE02_C61_USA.png",rmse,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)



############################################################
##                     BIAS
############################################################

metrica_interes <-"bias" 

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

bias<- ggplot(metricas_subset, aes(x = estacion, y = valor, shape = temporal, color = espacial)) +
  geom_point(alpha=0.6, fill="black",size=3) +
  scale_color_manual(values = c("#c51b8a","#2ca25f", "#de2d26", "#3182bd","#756bb1")) +  # Personaliza los colores
  scale_shape_manual(values = c(15,16,17,18)) +  # Personaliza las formas (shapes)
  #scale_y_continuous(limits=c(-0.07, 0.07),breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06))+
  # scale_y_continuous(limits=c(-0.07, 0.18),breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18))+
  scale_y_continuous(limits=c(-0.07, 0.28),breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28))+
  
  
  labs(title = "MAIAC C6.1 USA",
    x = "Station",
    y = "Bias",
    color = "Spatial window",
    shape = "Temporal window") + theme_bw()+  theme(legend.title = element_text(#family = "Roboto",
      
      size = 14,
      face = 2))+
  theme(legend.text = element_text(size =12))
bias

e<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, size = temporal, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #scale_y_continuous(limits=c(-0.07, 0.07),breaks = c(-0.06,-0.04,-0.02,0,0.02,0.04,0.06))+
  scale_y_continuous(limits=c(-0.08, 0.18),breaks = c(-0.08,-0.04,0,0.04,0.08,0.12,0.16))+
  
  
  labs(title = "MAIAC C6.0 Latam",
       x = "Spatial window (km)",
    y = "Bias",
    color = "Station",
    size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))
e
f<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = temporal,  size =espacial,color=estacion )) +
  geom_point(shape=1,stroke = 0.2) +
  #scale_y_continuous(limits=c(-0.07, 0.07),breaks = c(-0.06,-0.04,-0.02,0,0.02,0.04,0.06))+
  scale_y_continuous(limits=c(-0.07, 0.28),breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28))+
  
  
  labs(title = "MODIS USA",
    x = "Temporal window (min)",
    y = "Bias",
    color = "Station",
    size = "Spatial window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size =12))
#theme_minimal()

f

#plot_grid(a, plot_grid(e, f), ncol = 1)
plot_tot2 <- grid.arrange(e,f,ncol=2)#,heights=c(2,0.7))
#plot_tot3 <- grid.arrange(a,plot_tot2,ncol=1) 
ggsave("D:/Josefina/paper_git/paper_maiac/plot/V02/buff_Bias01_C61-USA.png",plot_tot2,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)

ggsave("D:/Josefina/paper_git/paper_maiac/plot/V02/buff_Bias02_C61-USA.png",bias,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)


############################################################
##                     N
############################################################

metrica_interes <-"n" 

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

num<- ggplot(metricas_subset, aes(x = estacion, y = valor, shape = temporal, color = espacial)) +
  geom_point(alpha=0.6, fill="black",size=3) +
  scale_color_manual(values = c("#c51b8a","#2ca25f", "#de2d26", "#3182bd","#756bb1")) +  # Personaliza los colores
  scale_shape_manual(values = c(15,16,17,18)) +  # Personaliza las formas (shapes)
  scale_y_continuous(limits=c(0,2000),breaks = c(0,400,800,1200,1600,2000))+
  labs(title = "Collection C61 USA",
    x = "Station",
    y = "Number",
    color = "Spatial window",
    shape = "Temporal window") + theme_bw()+  theme(legend.title = element_text(#family = "Roboto",
      
      size = 14,
      face = 2))+
  theme(legend.text = element_text(size =12))
num
ggsave("D:/Josefina/paper_git/paper_maiac/plot/V02/buff_Num_C60.png",num,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)
############################################################
##                     REU
############################################################

metrica_interes <-"reu" 

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

reu<- ggplot(metricas_subset, aes(x = estacion, y = valor, shape = temporal, color = espacial)) +
  geom_point(alpha=0.6, fill="black",size=3) +
  scale_color_manual(values = c("#c51b8a","#2ca25f", "#de2d26", "#3182bd","#756bb1")) +  # Personaliza los colores
  scale_shape_manual(values = c(15,16,17,18)) +  # Personaliza las formas (shapes)
  scale_y_continuous(limits=c(0, 140),breaks = c(0,20,40,60,80,100,120,140))+
  labs(title = "MODIS USA",
       x = "Station",
       y = "REUx",
       color = "Spatial window",
       shape = "Temporal window") + theme_bw()+  theme(legend.title = element_text(#family = "Roboto",
         
         size = 14,
         face = 2))+
  theme(legend.text = element_text(size =12))
#theme_minimal()
reu

e<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, size = temporal, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 140),breaks = c(0,20,40,60,80,100,120,140))+
  labs(title = "Collection C6.1 Latam",
       x = "Spatial window (km)",
       y = "REU",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


#theme_minimal()

e
f<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = temporal,  size =espacial,color=estacion )) +
  geom_point(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 140),breaks = c(0,20,40,60,80,100,120,140))+
  labs(title = "MODIS USA",
       x = "Temporal window (min)",
       y = "REUx",
       color = "Station",
       size = "Spatial window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size =12))
#theme_minimal()

f

#plot_grid(a, plot_grid(e, f), ncol = 1)
plot_tot2 <- grid.arrange(e,f,ncol=2)#,heights=c(2,0.7))
#plot_tot3 <- grid.arrange(a,plot_tot2,ncol=1) 
ggsave("D:/Josefina/paper_git/paper_maiac/plot/V02/buff_RMSE01_C61_USA.png",plot_tot2,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)

ggsave("D:/Josefina/paper_git/paper_maiac/plot/V02/buff_RMSE02_C61_USA.png",rmse,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)


############################################################
##                     NRMSE
############################################################

metrica_interes <-"nrmse" 

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

nrmse<- ggplot(metricas_subset, aes(x = estacion, y = valor, shape = temporal, color = espacial)) +
  geom_point(alpha=0.6, fill="black",size=3) +
  scale_color_manual(values = c("#c51b8a","#2ca25f", "#de2d26", "#3182bd","#756bb1")) +  # Personaliza los colores
  scale_shape_manual(values = c(15,16,17,18)) +  # Personaliza las formas (shapes)
  scale_y_continuous(limits=c(0, 1.21),breaks = c(0,0.20,0.40,0.60,0.80,1.00,1.2))+
  labs(title = "Collection C61 USA",
       x = "Station",
       y = "NRMSE",
       color = "Spatial window",
       shape = "Temporal window") + theme_bw()+  theme(legend.title = element_text(#family = "Roboto",
         
         size = 14,
         face = 2))+
  theme(legend.text = element_text(size =12))
#theme_minimal()
nrmse

e<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, size = temporal, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1.21),breaks = c(0,0.20,0.40,0.60,0.80,1.00,1.2))+
  labs(title = "Collection C60 USA",
       x = "Spatial window (km)",
       y = "NRMSE",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


#theme_minimal()

e
f<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = temporal,  size =espacial,color=estacion )) +
  geom_point(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1.21),breaks = c(0,0.20,0.40,0.60,0.80,1.00,1.2))+
  labs(title = "Collection C60 USA",
       x = "Temporal window (min)",
       y = "NRMSE",
       color = "Station",
       size = "Spatial window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size =12))
#theme_minimal()

f

#plot_grid(a, plot_grid(e, f), ncol = 1)
plot_tot2 <- grid.arrange(e,f,ncol=2)#,heights=c(2,0.7))
#plot_tot3 <- grid.arrange(a,plot_tot2,ncol=1) 
ggsave("D:/Josefina/paper_git/paper_maiac/plot/V02/buff_RMSE01_C61_USA.png",plot_tot2,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)

ggsave("D:/Josefina/paper_git/paper_maiac/plot/V02/buff_RMSE02_C61_USA.png",rmse,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)
###############################################################################
##                        r2 promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "MODIS",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "r2 promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

R2_latam_60<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(title = "MODIS USA",
       x = "Spatial window (km)",
       y = expression(R^2),
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
                                    
                                    size = 14,
                                    face = 2))+
  theme(legend.text = element_text(size = 12))


R2_latam_60
###############################################################################
##                        r2 promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]
metricas <- metricas[metricas$region == "latam",]
metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "r2 promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

R2_latam_61<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(title = "Collection C61 Latam",
       x = "Spatial window (km)",
       y = expression(R^2),
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
                                    
                                    size = 14,
                                    face = 2))+
  theme(legend.text = element_text(size = 12))


R2_latam_61

###############################################################################
##                        r2 promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "r2 promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

R2_USA_61<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(title = "Collection C61 USA",
       x = "Spatial window (km)",
       y = expression(R^2),
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
                                    
                                    size = 14,
                                    face = 2))+
  theme(legend.text = element_text(size = 12))


R2_USA_61

###############################################################################
##                        r2 promedio espacial eu 60
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "r2 promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

R2_USA_60<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(title = "Collection C60 USA",
       x = "Spatial window (km)",
       y = expression(R^2),
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


R2_USA_60
##################################################################################
###############################################################################
##                        RMSE promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "latam",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "rmse promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

rmse_latam_60<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  #scale_y_continuous(limits=c(0, 0.125),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  labs(title = "MODIS Latam",
       x = "Spatial window (km)",
       y = "RMSE",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


rmse_latam_60
###############################################################################
##                        RMSE promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]
metricas <- metricas[metricas$region == "latam",]
metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "rmse promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

rmse_latam_61<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 0.125),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  labs(title = "Collection C61 Latam",
       x = "Spatial window (km)",
       y = "RMSE",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


rmse_latam_61

###############################################################################
##                        rmse promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "rmse promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

rmse_USA_61<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 0.125),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  labs(title = "Collection C61 USA",
       x = "Spatial window (km)",
       y = "RMSE",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


rmse_USA_61

###############################################################################
##                        rmse promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "rmse promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

rmse_USA_60<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 0.125),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  labs(title = "Collection C60 USA",
       x = "Spatial window (km)",
       y = "RMSE",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


rmse_USA_60

##################################################################################
###############################################################################
##                       BIAS  promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "latam",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "bias promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

bias_latam_60<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  #scale_y_continuous(limits=c(-0.07, 0.07),breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06))+
  labs(title = "MODIS Latam",
       x = "Spatial window (km)",
       y = "Bias",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


bias_latam_60
###############################################################################
##                       bias  promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]
metricas <- metricas[metricas$region == "latam",]
metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "bias promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

bias_latam_61<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(-0.07, 0.07),breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06))+
  labs(title = "Collection C61 Latam",
       x = "Spatial window (km)",
       y = "Bias",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


bias_latam_61

###############################################################################
##                        bias promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "bias promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

bias_USA_61<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(-0.07, 0.07),breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06))+
  labs(title = "Collection C61 USA",
       x = "Spatial window (km)",
       y = "Bias",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


bias_USA_61

###############################################################################
##                        bias promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "bias promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

bias_USA_60<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(-0.07, 0.07),breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06))+
  labs(title = "Collection C60 USA",
       x = "Spatial window (km)",
       y = "Bias",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


bias_USA_60
##################################################################################
###############################################################################
##                       reux  promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "latam",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "reu promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

reux_latam_60<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 100),breaks = c(0,20,40,60,80,100))+
  labs(title = "MODIS Latam",
       x = "Spatial window (km)",
       y = "REUx",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


reux_latam_60
###############################################################################
##                       reux  promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]
metricas <- metricas[metricas$region == "latam",]
metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "reu promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

reux_latam_61<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 100),breaks = c(0,20,40,60,80,100))+
  labs(title = "Collection C61 Latam",
       x = "Spatial window (km)",
       y = "REUx",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


reux_latam_61

###############################################################################
##                        reux promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "reu promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

reux_USA_61<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 100),breaks = c(0,20,40,60,80,100))+
  labs(title = "Collection C61 USA",
       x = "Spatial window (km)",
       y = "REUx",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


reux_USA_61

###############################################################################
##                        reux promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "reu promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

reux_USA_60<- # Crea el gráfico de dispersión con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 100),breaks = c(0,20,40,60,80,100))+
  labs(title = "Collection C60 USA",
       x = "Spatial window (km)",
       y = "REUx",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


reux_USA_60

############################################################
## Definir colores y formas para cada estación
colores <- c('#74c476', '#fed976', '#fb6a4a', '#74a9cf', '#df65b0', '#807dba')
formas <- c(1, 1, 2, 5, 6, 23)
metricas_subset_r2 <- metricas[metricas$metrica == "r2",]

r2<-ggplot(metricas_subset_r2, aes(y = valor, x = espacial, size = temporal, shape = estacion)) +
  geom_point(aes(color = estacion),stroke=0.5) +  # Aquí especificamos fill dentro de aes() y también dentro de geom_point()
  scale_shape_manual(values = formas) +
  scale_color_manual(values = c("#74c476","#fed976","#fb6a4a", "#74a9cf","#df65b0","#807dba")) +  # Personaliza los colores
  labs(x = "Spatial window (km)", y = expression(R^2), fill = "Station", size = "Temporal window") +
  theme_bw() +scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 19),
        axis.text.x = element_text(size = 19),
        axis.title.y = element_text(size = 19.5),
        axis.title.x = element_text(size = 19.5),
        legend.title = element_text(size = 20, face = 2),
        legend.text = element_text(size = 20))#)     # Elimina todas las leyendas
  # theme(legend.title = element_text(size = 14, face = 2),
  #       legend.text = element_text(size = 12),legend.title=none)+ 
  
r2
metricas_subset_rmse <- metricas[metricas$metrica == "rmse",]
rmse<-ggplot(metricas_subset_rmse, aes(y = valor, x = espacial, size = temporal, shape = estacion)) +
  geom_point(aes(color = estacion),stroke=0.5) +  # Aquí especificamos fill dentro de aes() y también dentro de geom_point()
  scale_shape_manual(values = formas) +
  scale_color_manual(values = c("#74c476","#fed976","#fb6a4a", "#74a9cf","#df65b0","#807dba")) +  # Personaliza los colores
  labs(x = "Spatial window (km)", y = "RMSE", fill = "Station", size = "Temporal window") +
  theme_bw() +scale_y_continuous(limits=c(0, 0.25),breaks = c(0,0.04,0.08,0.12,0.16,0.2,0.24))+
  
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 19),
        axis.text.x = element_text(size = 19),
        axis.title.y = element_text(size = 19.5),
        axis.title.x = element_text(size = 19.5),
        legend.title = element_text(size = 20, face = 2),
        legend.text = element_text(size = 20))#)    # Elimina todas las leyendas
# theme(legend.title = element_text(size = 14, face = 2),
#       legend.text = element_text(size = 12),legend.title=none)+ 
rmse

metricas_subset_bias <- metricas[metricas$metrica == "bias",]
bias<-ggplot(metricas_subset_bias, aes(y = valor, x = espacial, size = temporal, shape = estacion)) +
  geom_point(aes(color = estacion),stroke=0.5) +  # Aquí especificamos fill dentro de aes() y también dentro de geom_point()
  scale_shape_manual(values = formas) +
  scale_color_manual(values = c("#74c476","#fed976","#fb6a4a", "#74a9cf","#df65b0","#807dba")) +  # Personaliza los colores
  labs(x = "Spatial window (km)", y = "Bias", fill = "Station", size = "Temporal window") +
  theme_bw() +scale_y_continuous(limits=c(-0.08, 0.18),breaks = c(-0.08,-0.04,0,0.04,0.08,0.12,0.16,0.2))+
  
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 19),
        axis.text.x = element_text(size = 19),
        axis.title.y = element_text(size = 19.5),
        axis.title.x = element_text(size = 19.5),
        legend.title = element_text(size = 20, face = 2),
        legend.text = element_text(size = 20))#)    # Elimina todas las leyendas
# theme(legend.title = element_text(size = 14, face = 2),
#       legend.text = element_text(size = 12),legend.title=none)+ 
bias
metricas_subset_REU <- metricas[metricas$metrica == "reu",]

metricas_subset_REU$Station <- metricas_subset_REU$estacion
reu<-ggplot(metricas_subset_REU, aes(y = valor, x = espacial, size = temporal, shape = Station)) +
  geom_point(aes(color = Station),stroke=0.5) +  # Aquí especificamos fill dentro de aes() y también dentro de geom_point()
  scale_shape_manual(values = formas) +
  scale_color_manual(values = c("#74c476","#fed976","#fb6a4a", "#74a9cf","#df65b0","#807dba")) +  # Personaliza los colores
  labs(x = "Spatial window (km)", y = "REU", fill = "Station", size = "Temporal") +
  theme_bw() +scale_y_continuous(limits=c(0,100),breaks = c(0,20,40,60,80,100))+
  
  guides(
    color = guide_legend(override.aes = list(size = 5)),  # Ajusta el tamaño de los puntos en la leyenda
    shape = guide_legend(override.aes = list(size = 5))   # Ajusta el tamaño de las formas en la leyenda
  )+
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 19),
        axis.text.x = element_text(size = 19),
        axis.title.y = element_text(size = 19.5),
        axis.title.x = element_text(size = 19.5),
        legend.title = element_text(size = 20, face = 2),
               legend.text = element_text(size = 20))#) 
 # Elimina todas las leyendas
# theme(legend.title = element_text(size = 14, face = 2),
#       legend.text = element_text(size = 12),legend.title=none)+ 
reu
950 - 484
