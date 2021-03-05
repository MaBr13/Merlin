World <- st_read("ne_50m_admin_0_countries.shp")
belgium <- readRDS("gadm36_BEL_0_sp.rds")
UK <- World[World$NAME == "United Kingdom",]
Ireland <- World[World$NAME == "Ireland",]
Sweden <- World[World$NAME == "Sweden",]
Norway <- World[World$NAME == "Norway",]
Denmark <- World[World$NAME == "Denmark",]
Germany <- World[World$NAME == "Germany",]
TheNetherlands <- World[World$NAME == "Netherlands",]
Belgium <- World[World$NAME == "Belgium",]
France <- World[World$NAME == "France",]

UKCord <- UK$geometry
points <- list()
for (k in 1:length(UKCord)){
  for(i in 1:length(UKCord[[k]])){
    for(j in 1:length(UKCord[[k]][[i]])){
      points[[i]] <- point.in.polygon(point.x=c(Departures_Spring_int$Long_start),point.y=c(Departures_Spring_int$Lat_start),
                                               pol.x=c(UKCord[[k]][[i]][[j]][,1]),pol.y=c(UKCord[[k]][[i]][[j]][,2]))
      }
    }
  } 
}


for (i in 1:length(points)){
  for (k in 1:length(points[[i]])){
    if (points[[i]][[k]]>0)
      Departures_Spring_int$country[k] <- "UK"
    else
      Departures_Spring_int$country[k] <- NA
    }
  }
  
  Departures_Spring$country <- 
}


coordsS=data.frame("long"=Departures_Spring_int$Long_start,"lat"=Departures_Spring_int$Lat_start)
coordinates(coordsS) = ~long+lat
UKCord <- UK$geometry
coordinates(coordsS)= ~long+lat
p <- gDifference(coordsS,UKCord)

proj4string(coordsS)<- CRS("+init=epsg:4326")
proj4string(UKCord)<- CRS("+init=epsg:4326")
plot(spdf)


plot(Departures_Autumn_int$winddir_start,cex=1,bin=360,stack=TRUE,sep=0.0035,shrink=2)
axis.circular(at=circular(seq(0.7)))


ggplot(Departures_Autumn_int, aes(x=winddir_start)) + 
  geom_histogram(breaks=seq(0,360,10),col='black',fill="white") +
  geom_segment(aes(x=as.numeric(wdmAint),xend=as.numeric(wdmAint),y=0,yend=1200), colour="red", arrow = arrow(length = unit(0.5, "cm")))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))+
  theme_bw()

Departures_Autumn_int <- filter(Departures_Autumn_int, winddir_start>0)
Departures_Spring_int <- filter(Departures_Spring_int, winddir_start>0)

par(mfrow=c(2,2),mar = c(0.5,0.5,0.2,0.2),oma=c(0,0,0,0))
#Spring_int
par(mar=c(0,15,1.4,0))
rose.diag(Departures_Spring_int$winddir_start,bins=36,prop=2.8,axes=FALSE,shrink=0.9,ticks = FALSE,main="Spring (IM)")
axis.circular(at=circular(seq(0,7*pi/4,pi/4)),labels=c("E","NE","N","NW","W","SW","S","SE"),zero=pi/2,rotation = "clock",cex=1.1)
arrows.circular(wdmSint,col="black",lwd=2)
arrows.circular(circular(98.62,units = "degrees",modulo="2pi",
                         template = 'geographic') ,col="red",lwd=2)
#Autumn_int
par(mar=c(0,0,1.4,15))
rose.diag(Departures_Autumn_int$winddir_start,bins=36,prop=2.8,axes=FALSE,shrink=0.9,ticks = FALSE,main="Autumn (IM)")
axis.circular(at=circular(seq(0,7*pi/4,pi/4)),labels=c("E","NE","N","NW","W","SW","S","SE"),zero=pi/2,rotation = "clock",cex=1.1)
arrows.circular(wdmAint,col="black",lwd=2)
arrows.circular(circular(222,units = "degrees",modulo="2pi",
                         template = 'geographic'),col="red",lwd=2)
#Spring
par(mar=c(0,15,1.4,0))
rose.diag(Departures_Spring$winddir_mean,bins=36,prop=2.8,axes=FALSE,shrink=0.9,ticks = FALSE,main="Spring (FS)")
axis.circular(at=circular(seq(0,7*pi/4,pi/4)),labels=c("E","NE","N","NW","W","SW","S","SE"),zero=pi/2,rotation = "clock",cex=1.1)
arrows.circular(wdmS,col="black",lwd=2)
arrows.circular(circular(97.6,units = "degrees",modulo="2pi",
                         template = 'geographic'),col="red",lwd=2)
#Autumn
par(mar=c(0,0,1.4,15))
rose.diag(Departures_Autumn$winddir_mean,bins=36,prop=2.8,axes=FALSE,shrink=0.9,ticks = FALSE,main="Autumn (FS)")
axis.circular(at=circular(seq(0,7*pi/4,pi/4)),labels=c("E","NE","N","NW","W","SW","S","SE"),zero=pi/2,rotation = "clock",cex=1.1)
arrows.circular(wdmA,col="black",lwd=2)
arrows.circular(circular(221.23,units = "degrees",modulo="2pi",
                         template = 'geographic'),col="red",lwd=2)
dev.off()
