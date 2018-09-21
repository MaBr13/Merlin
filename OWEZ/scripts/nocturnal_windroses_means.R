NocturnalA <- list()
  for(k in 1:length(Autumn)){
    
    NocturnalA[[k]] <- subset(Autumn[[k]], Light==0, select=Timestamp:DirectionS)
  }

#wind rose (wind speed and direction)
for(k in 1:length(NocturnalA)){{
  s <- as.data.frame(NocturnalA[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalA[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=NocturnalA[[k]]$Wspeed, colour=NocturnalA[[k]]$Wspeed)) +
    scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Mean wind direction per hour (degrees) Autumn Noct",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,4000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirAN','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

#wind rose (air speed and direction)
for(k in 1:length(NocturnalA)){{
  s <- as.data.frame(NocturnalA[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalA[[k]], aes(Mean.head,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=NocturnalA[[k]]$Aspeed, colour=NocturnalA[[k]]$Aspeed)) +
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    ggtitle(paste("Mean heading per hour (degrees) Autumn Noct",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,4000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrasbdirAN','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

NocturnalS <- list()
for(k in 1:length(Spring)){
  
  NocturnalS[[k]] <- subset(Spring[[k]], Light==0, select=Timestamp:DirectionS)
}

#wind rose (wind speed and direction)
for(k in 1:length(NocturnalS)){{
  s <- as.data.frame(NocturnalS[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalS[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=NocturnalS[[k]]$Wspeed, colour=NocturnalS[[k]]$Wspeed)) +
    scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Mean wind direction per hour (degrees) Spring Noct",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,4000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirSN','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

#wind rose (air speed and direction)
for(k in 1:length(NocturnalS)){{
  s <- as.data.frame(NocturnalS[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalS[[k]], aes(Mean.head,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=NocturnalS[[k]]$Aspeed, colour=NocturnalS[[k]]$Aspeed)) +
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    ggtitle(paste("Mean heading per hour (degrees) Spring Noct",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,4000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrasbdirSN','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}
