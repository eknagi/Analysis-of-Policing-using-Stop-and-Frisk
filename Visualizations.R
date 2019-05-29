library(dplyr)
library(ggmap)
library(ggplot2)
library(ggmap)
library(gganimate)
library(gifski)



#Read in the dataset:
sqf.data<-read.csv(file="sqf_08_16.csv")

#Filter the data to include on CPW stops
sqf_filter <- sqf.data %>% filter(suspected.crime=='cpw')

#Remove data points with missing coordinates: 
sqf_filter= sqf_filter[is.na(sqf_filter$lon)==FALSE,]


#Creating a map of stops colored by race:

ggmap::register_google(key = "YOUR API KEY")

ggmap(get_map("New York", zoom=10))+ geom_point(aes(x = lon, y = lat,  colour = as.factor(suspect.race)), data = sqf_filter, size = 0.30) + 
  theme(legend.position="bottom")+ scale_x_continuous(limits = c(min(sqf_filter$lon, na.rm=T)-0.01, max(sqf_filter$lon, na.rm=T)+0.01), expand = c(0, 0)) +
  scale_y_continuous(limits = c(min(sqf_filter$lat, na.rm=T)-0.01, max(sqf_filter$lat, na.rm=T)+0.01), expand = c(0, 0))


#Animated map by year:
q=ggmap(get_map("New York", zoom=10))+ geom_point(aes(x = lon, y = lat,  colour = as.factor(year)), data = sqf_filter, size = 0.35) + 
  theme(legend.position="bottom")+ scale_x_continuous(limits = c(min(sqf_filter$lon, na.rm=T)-0.01, max(sqf_filter$lon, na.rm=T)+0.01), expand = c(0, 0)) + scale_y_continuous(limits = c(min(sqf_filter$lat, na.rm=T)-0.01, max(sqf_filter$lat, na.rm=T)+0.01), expand = c(0, 0))+ 
  theme(legend.position="bottom")  +
  transition_time(as.numeric(sqf_filter$year))

animate(q)