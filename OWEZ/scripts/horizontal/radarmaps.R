library(ggmap)
library(ggplot2)
library(gridExtra)
library(ggpubr)

register_google(key = "AIzaSyC-bw1FEcHmDesL-zAPgW9RaWwolld2gRw")

#creating map

NSea <- get_map(location = c(lon=5.5, lat=54.5110 ), 
                maptype = "terrain-background",  source = "stamen", color="bw", zoom=6) #add a map
S <- ggmap(NSea)
S


OWEZ <- c(52.60636,4.389639)
K14 <- c(53.26889,3.628889)
Luchterduinen <- c(52.41667,4.166667)
Thornton <- c(51.5328,2.95509)
Den_Helder <- c(52.9563,4.7608)
De_Bilt <- c(52.1017, 5.1783)
Herwijnen <- c(51.83708 ,5.13797)
Zaventum <- c(50.9054, 4.4579)
Jabbeke <- c(51.1919, 3.0641)
Borkum <- c(53.5640, 6.7482)
Emden <- c(53.3387 ,7.0237)
Boostedt <- c(54.0043 ,10.0468)
Rostock <- c(54.1757, 12.058)
Thurnham <- c(51.2942,0.6064)
Chenies <- c(51.69,-0.53)
Ingham <- c(53.335,-0.5564)



Radars <- as.data.frame(rbind(OWEZ,K14,Luchterduinen, Thornton, Den_Helder, De_Bilt,Herwijnen,Zaventum,Jabbeke,Borkum,Emden,Boostedt,Rostock,Thurnham, Chenies,Ingham))
names <- c("OWEZ","K14","Luchterduinen","Thornton", "Den Helder","De_Bilt","Herwijnen","Zaventum","Jabbeke","Borkum","Emden","Boostedt","Rostock","Thurnham", "Chenies","Ingham")
type <- c("Bird radar","Bird radar", "Bird radar","Bird radar", "Weather radar","Weather radar","Weather radar","Weather radar","Weather radar","Weather radar","Weather radar","Weather radar","Weather radar","Weather radar","Weather radar","Weather radar")
Radars <- as.data.frame(cbind(Radars,names,type))
names(Radars)[c(1,2,3,4)]<-paste(c("Latitude","Longitude","Radar","Type"))


d <- S+geom_point(data=Radars, aes_string(Radars$Longitude,Radars$Latitude, size=Radars$Radar,colour=Radars$Radar,shape=Radars$Type),show.legend = T) + 
  scale_colour_manual(values=c("darkmagenta", "dodgerblue4", "aquamarine3","orange", "yellow","red","maroon","forestgreen","blue","seagreen","brown","black","turquoise","tomato","gold","purple"), name="Radar")+
  scale_size_manual(values=rep(5,16),name="Radar")+
  scale_shape_manual(values=c(19,17), name="Radar type")+
  ylab("Latitude")+
  xlab  ("Longitude")
d  

