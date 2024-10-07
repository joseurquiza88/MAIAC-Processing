## function to calculate monthly statistics

monthly_statistics <- function (path, group,path_write){
  setwd(path)
  archive <- dir(path, pattern = ".csv")
  for(i in 1:length(archive)){
    print(i)
    # Read the files of the entered path
    df<- read.csv(archive[i],  header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE, na.strings = "-999")
    name <- substr(archive[i],1,19)
    df$month <- substr(df$date,6,7)
    df$month_year <- substr(df$date,1,7)
    if (group == "month"){
      df%>%
        group_by(month) %>%  
        group_split() -> data_group
      rbind_df <- data.frame()
      for (x in 1:length(data_group)){
        data<- data.frame(month = data_group[[x]][["month"]][1],
        AOD_550_MODIS_mean = round(mean(data_group[[x]][["AOD_550_MODIS_mean"]],rm.na = T),2),
        AOD_550_MAIAC_mean = round(mean(data_group[[x]][["AOD_550_MAIAC_mean"]],rm.na = T),2),
        AOD_550_AER_mean = round(mean(data_group[[x]][["AOD_550_AER_mean"]],rm.na = T),2))
        rbind_df <- rbind(rbind_df,data)
      } 
    }
    else if(group == "month_year"){
        df%>%
          group_by(month_year) %>%  
          group_split() -> data_group
        rbind_df <- data.frame()
        for (x in 1:length(data_group)){
          data<- data.frame(month_year = data_group[[x]][["month_year"]][1],
                            AOD_550_MODIS_mean = round(mean(data_group[[x]][["AOD_550_MODIS_mean"]],rm.na = T),2),
                            AOD_550_MAIAC_mean = round(mean(data_group[[x]][["AOD_550_MAIAC_mean"]],rm.na = T),2),
                            AOD_550_AER_mean = round(mean(data_group[[x]][["AOD_550_AER_mean"]],rm.na = T),2))
          rbind_df <- rbind(rbind_df,data)
        }
        
    }
   
    # The new dataframe is saved in the entered path as "path_write"
    write.csv(rbind_df,(paste(path_write,name,"-",group,".csv",sep="")) , row.names = FALSE)
    
  }
}

path <- "D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA_v2/"
path_write <- "D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA_v2/statistics/"
group <- "month"
group <- "month_year"
monthly_statistics (path, group,path_write)
