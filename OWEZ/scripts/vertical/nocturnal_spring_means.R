###################################
####noctural migration spring######

#plot nocturnal and diurnal migrants om spring
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(Spring[[k]], aes(Light, Nr.tracks))+
    geom_point(aes(colour=Spring[[k]]$Aspeed, shape=Spring[[k]]$Aspeed), size=2, show.legend = T) +
    scale_shape_manual(values = c(17,20,3,1),name="Air speed (kph)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    ggtitle(paste("Number of nocturnal and diurnal migrants and their air speed Spring",' ', datetime,sep='')) +
    xlab("Time of a day") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))+
    ggsave(filename=paste('nctrnS','_',datetime, ".jpeg", sep=''), 
           path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Spring",scale=2)
}
print(plot)
}
#plot "species" distirbution of nocturnal migrants

NocturnalS <- list()

for (k in 1:length(Spring)){
 NocturnalS[[k]] <- subset(Spring[[k]], Light==0, select=Timestamp:Wspeed)
}

for(k in 1:length(NocturnalS)){{
  s <- as.data.frame(NocturnalS[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalS[[k]], aes(Aspeed, Nr.tracks))+
    geom_point(aes(colour=NocturnalS[[k]]$Aspeed, shape=NocturnalS[[k]]$Aspeed), size=2, show.legend = T) +
    scale_shape_manual(values = c(17,20,3,1),name="Air speed (kph)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    ggtitle(paste("Number and distribution of nocturnal migrants Spring",' ', datetime,sep='')) +
    xlab("Time of a day") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_discrete(NocturnalS$Aspeed, labels=c("Potential passerines", "Other birds",
                                                 "Not birds", "Data n/a"))
    ggsave(filename=paste('nctrnmS','_',datetime, ".jpeg", sep=''), 
           path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Spring",scale=2)
}
  print(plot)
}


### Nocturnal Spring, wind rose analysis

for(k in 1:length(NocturnalS)){
  
  NocturnalS[[k]]$gspms <- (NocturnalS[[k]]$Mean.speed)*0.277778
}

for(k in 1:length(NocturnalS)){
  
  NocturnalS[[k]]$aspms <- (NocturnalS[[k]]$Mean.aspeed)*0.277778
}

for(k in 1:length(NocturnalS)){
  
  NocturnalS[[k]]$wspms <- (NocturnalS[[k]]$Mean.wspeed)*0.277778
}

#wind speeds and directions
p1 <- plot.windrose(spd=Spring[[1]]$wspms, dir = Spring[[1]]$Mean.wdir,spdseq = c(0,10,20,30,40,50))
p2 <- plot.windrose(spd=Spring[[2]]$wspms, dir = Spring[[2]]$Mean.wdir,spdseq = c(0,10,20,30,40,50))
p3 <- plot.windrose(spd=Spring[[3]]$wspms, dir = Spring[[3]]$Mean.wdir,spdseq = c(0,10,20,30,40,50))
p4 <- plot.windrose(spd=Spring[[4]]$wspms, dir = Spring[[4]]$Mean.wdir,spdseq = c(0,10,20,30,40,50))
grid.arrange(p1,p2,p3,p4,nrow=2,top="Wind speed and track direction Spring 2007-2010")
s1 <- plot.windrose(spd=Spring[[1]]$aspms, dir = Spring[[1]]$Mean.head,spdseq = c(0,15,20,30,40,50))
s2 <- plot.windrose(spd=Spring[[2]]$aspms, dir = Spring[[2]]$Mean.head,spdseq = c(0,15,20,30,40,50))
s3 <- plot.windrose(spd=Spring[[3]]$aspms, dir = Spring[[3]]$Mean.head,spdseq = c(0,15,20,30,40,50))
s4 <- plot.windrose(spd=Spring[[4]]$aspms, dir = Spring[[4]]$Mean.head,spdseq = c(0,15,20,30,40,50))
grid.arrange(s1,s2,s3,s4,nrow=2,top="Air speed and track direction Spring 2007-2010")

#wind rose better way

library(ggplot2)

#Wind speed and direction
  ggplot(NocturnalS[[1]], aes(Mean.wdir, Nr.tracks)) + 
  geom_bar(stat="identity", aes(fill=NocturnalS[[1]]$Wspeed, colour=NocturnalS[[1]]$Wspeed), width = 2) +
  scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Wind speed (kph)", drop=F)+
  scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Wind speed (kph)", drop=F)+
  ylab("Number of tracks per hour")+
  xlab("Mean wind direction per hour (degrees)")+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
#bird airspeed and direction  
  ggplot(NocturnalS[[1]], aes(Nr.tracks,Mean.head)) + 
    geom_bar(stat="identity", aes(fill=NocturnalS[[1]]$Aspeed, colour=NocturnalS[[1]]$Aspeed)) +
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    ylab("Mean migration direction per hour(degrees)")+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  

  