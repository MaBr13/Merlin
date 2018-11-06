library(ggmap)
library(ggplot2)
library(gridExtra)
library(ggpubr)

register_google(key = "AIzaSyC-bw1FEcHmDesL-zAPgW9RaWwolld2gRw")

#creating map

NSea <- get_map(location = c(lon=3.8, lat=54.5110 ), 
                maptype = "terrain-background",  source = "stamen", color="bw", zoom=6) #add a map
S <- ggmap(NSea)
S


OWEZ <- c(52.60636,4.389639)
K14 <- c(53.26889,3.628889)
Luchterduinen <- c(52.41667,4.166667)
Thornton <- c(51.5328,2.95509)
Den_Helder <- c(52.9563,4.7608)

Radars <- as.data.frame(rbind(OWEZ,K14,Luchterduinen, Thornton, Den_Helder))
names <- c("OWEZ","K14","Luchterduinen","Thornton", "Den Helder")
type <- c("Bird radar","Bird radar", "Bird radar","Bird radar", "Weather radar")
Radars <- as.data.frame(cbind(Radars,names,type))
names(Radars)[c(1,2,3,4)]<-paste(c("Latitude","Longitude","Radar","Type"))


d <- S+geom_point(data=Radars, aes_string(Radars$Longitude,Radars$Latitude, size=Radars$Radar,colour=Radars$Radar,shape=Radars$Type),show.legend = T) + 
  scale_colour_manual(values=c("darkmagenta", "dodgerblue4", "aquamarine3","orange", "yellow"), name="Radar")+
  scale_size_manual(values=c(5,5,5,5,5),name="Radar")+
  scale_shape_manual(values=c(19,17), name="Radar type")+
  ylab("Latitude")+
  xlab  ("Longitude")
d  
