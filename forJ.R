#indego project for 5th Sq

library(tidyverse)
library(lubridate)
library(sf)
library(rgdal)
library(gganimate)
library(tigris)
library(gt)
library(forcats)
options(tigris_use_cache = TRUE)

#data agg
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


#time variable cleaning 



indegofull$year <- indegofull$date %>% year()
indegofull$month <- indegofull$date %>% month()
indegofull$day <- indegofull$date %>% day()
indegofull$week <- indegofull$date %>% week()

indegofull$weekyear <- (indegofull$year + .01*indegofull$week) %>% as.factor()
indegofull$monthyear <- (indegofull$date %>% year() + .01*indegofull$date %>% month()) %>% as.factor()
indegofull$ymd <- (indegofull$year + .01*indegofull$month + .00001*indegofull$day) %>% as.factor()



#oldycity var
oldcity <- c(3015, 3046, 3047, 3124, 3169)

indegofull$oldcity <- ifelse(indegofull$start_station %in% oldcity | indegofull$end_station %in% oldcity, "y", 'n')

#summary stats
gg <- indegofull %>% count(year, oldcity)

gm <- data.frame(c(gg %>% filter(oldcity == 'y' ), gg %>% filter(oldcity == 'n' ))) %>% 
  select(c("year", 'n','n.1')) %>%
  rename('Old City' = n, 'Rest of Philly' = n.1)

gt(gm) %>%
  tab_header(title = md("Annual Indego Rides"))


#plotting

g<- ggplot(indegofull, aes(x= monthyear))+
  geom_area(stat = "count", aes(fill = electric, group = electric))+
  geom_vline(xintercept = .5)+
  geom_vline(xintercept = 12.5)+
  geom_vline(xintercept = 24.5)+
  geom_vline(xintercept = 36.5)+
  geom_vline(xintercept = 48.5)+
  geom_vline(xintercept = 60.5)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  labs(x = "Year", y = "Monthly Total Trips", title = "Increase in Ebike Rentals from Indego Bikeshare", fill = "Bike Type")

ggsave(g, filename = 'indegoebikes', path = "C:/Users/Charlie/Documents/GIS",device = 'png'  )



#geospatial

streets <- readOGR("C:/Users/Charlie/Documents/GIS/PhillyData", layer = "streets", verbose = T) %>% st_as_sf()

landuse <- readOGR("C:/Users/Charlie/Documents/GIS/PhillyData", layer = "Land_Use", verbose = T) %>% st_as_sf()
parksf <- landuse %>% filter(C_DIG1 == 7)
river <- area_water('PA', county = 'Philadelphia') %>%
  st_as_sf()%>%
  st_transform(crs=4326)


#stations
statlatlong <- data.frame(df$start_station, df$start_lat, df$start_lon)

statlatlong <- statlatlong[!duplicated(statlatlong),]
statlatlong <- statlatlong %>% rename('Station_ID' = 'df.start_station',
                                      'lat'='df.start_lat',
                                      'lon'='df.start_lon')
stations <- read.csv("C:/Users/Charlie/Documents/GIS/5thSq/IndegoMap/indego-stations-2021-07-01.csv")

#statlatlong$Station_ID <- statlatlong$Station_ID %>% as.numeric()
stats <- left_join(x = stations, y = statlatlong)

stats <- stats %>% rename('dateopen' = "Day.of.Go_live_date") 


stats <- stats %>% filter(!is.na(lat)) %>% st_as_sf(coords = c('lon','lat'),crs = (4326))
stats <- stats %>% st_transform(crs = (4326))

stats$oldcity <- ifelse(stats$Station_ID %in% oldcity, "y", 'n')



sf::sf_use_s2(FALSE)

bounds <- c(ymax = 40, ymin = 39.88, xmin =-75.23, xmax = -75.125)

street <- streets %>% st_crop(y=bounds)
riv <- river %>% st_crop(y = bounds)
parks <- parksf %>%  st_crop(y = bounds)

mapTheme <- theme(plot.title =element_text(size=12),
                  plot.subtitle = element_text(size=8),
                  plot.caption = element_text(size = 6),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_line(colour = 'transparent'),
                  panel.grid.minor=element_blank(),
                  legend.direction = "vertical", 
                  legend.position = "none",
                  plot.margin = margin(1, 1, 1, 1, 'cm'),
                  legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))

p <- ggplot()+
  geom_sf(data = riv, fill = 'lightblue')+ 
  geom_sf(data = street, alpha = .5)+
  geom_sf(data = parks, fill = 'lightgreen', color = NA, alpha = .5)+
  geom_sf(data = stats, aes(color = oldcity))+
  mapTheme


ggsave(p, filename = 'indemap', path = "C:/Users/Charlie/Documents/GIS", device = 'png')


#oldcity 5 stats 
oc <- indegofull %>% filter(oldcity == 'y')

ocstats <- stations %>% select(c("Station_ID", "Station_Name")) %>% filter(Station_ID %in% oldcity)

oc <- left_join(x = oc, y = ocstats, by = c("start_station", "Station_ID"))




oc$ocstat <- rep(NA, nrow(oc))

lapply(X = oc$ocstat, FUN = ocnaming, )


if(oc$start_station == 3015 | oc$end_station == 3015){
   
  }


oc$ocstat <-  ifelse(oc$start_station %in% oldcity & oc$end_station%in% oldcity, "Begin and End in Old City", 
                 ifelse(oc$start_station == 3015 | oc$end_station == 3015, "4th & Walnut", 
                      ifelse(oc$start_station == 3046 | oc$end_station == 3046, "2nd & Market",
                              ifelse(oc$start_station == 3047 | oc$end_station == 3047, "Independence Mall",
                                    ifelse(oc$start_station == 3124 | oc$end_station == 3124, "Race Street Pier",
                                            ifelse(oc$start_station == 3169 | oc$end_station == 3169, "2nd & Race", "NA"))))))

oc$ocstat <- fct_infreq(oc$ocstat)

ggplot(data= oc, aes(x = monthyear))+
  geom_bar(stat = 'count', position = "fill",  aes(fill = ocstat))+
  geom_vline(xintercept = .5)+
  geom_vline(xintercept = 12.5)+
  geom_vline(xintercept = 24.5)+
  geom_vline(xintercept = 36.5)+
  geom_vline(xintercept = 48.5)+
  geom_vline(xintercept = 60.5)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  labs(x = "Year", y = "Proportion of old city trips", fill = "OC Station")



#aniamting yearly trends+ e

indegofull$electric <- ifelse(is.na(indegofull$bike_type), 'standard', indegofull$bike_type) %>% factor(levels = c("standard", "electric"))

ggplot(indegofull, aes(x= monthyear))+
  geom_bar(stat = "count", aes(fill = electric))+
  theme(axis.text.x = element_blank())


  scale_fill_manual("Bike Type", values = c("Ebike" = "electric", "Aucoistu" = "standard"))


