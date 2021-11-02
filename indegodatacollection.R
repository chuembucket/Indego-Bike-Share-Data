library(tidyverse)

rides <- read.csv("C:/Users/Charlie/Documents/GIS/5thSq/IndegoMap/RideData/indego-trips-2016-q1.csv")

cd <- "C:/Users/Charlie/Documents/GIS/5thSq/IndegoMap/RideData/indego-trips"
years <- c("2016","2017","2018","2019","2020","2021")
qs <- c("q1","q2","q3","q4")

indegofull <- rides[0,]

for (year in years){
  for (q in qs){
    df <- read.csv(paste(paste(cd,paste(year,q, sep = "-"),sep = "-"),".csv", sep = ""))
    if("bike_type" %in% colnames(df)== F){
      df$bike_type <- rep(NA, nrow(df))
    }
    
    df <- separate(df, start_time, into = c('date','time'), sep = " ")
    
    if(substr(df$date[1], 1, 2) == '20'){
      df$date <- df$date %>% as.Date(format = "%Y-%m-%d")
    }else{
      df$date <- df$date %>% as.Date(format = "%m/%d/%Y")
    }
    
    
    indegofull <- rbind(indegofull, df)
    print(paste(year,q,sep="-"))
  }
}