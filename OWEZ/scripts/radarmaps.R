library(ggmap)
library(ggplot2)
library(gridExtra)
library(ggpubr)


#creating map

NSea <- get_map(location = c(lon=3.8, lat=52.5110 ), 
                maptype = "terrain-background",  source = "stamen", color="bw", zoom=7) #add a map
S <- ggmap(NSea)
S


OWEZ <- c(52.60636,4.389639)
K14 <- c(53.26889,3.628889)
Luchterduinen <- c(52.41667,4.166667)
Radars <- as.data.frame(rbind(OWEZ,K14))
Radars <- as.data.frame(rbind(Radars,Luchterduinen))
names <- c("OWEZ","K14","Luchterduinen")
Radars <- as.data.frame(cbind(Radars,names))
names(Radars)[c(1,2)]<-paste(c("Latitude","Longitude","Radar"))


d <- S+geom_point(data=Radars, aes_string(Radars$Longitude,Radars$Latitude,size=3, colour=Radars$Radar), show.legend = T) + 
  #scale_colour_manual(values=c("darkmagenta", "dodgerblue4", "aquamarine3"), name="Radar")+
  ylab("Latitude")+
  xlab  ("Longitude")
  
