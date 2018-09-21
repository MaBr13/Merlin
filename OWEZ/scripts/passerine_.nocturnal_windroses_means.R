NocturnalAP <- list()
for(k in 1:length(NocturnalA)){
  
  NocturnalAP[[k]] <- subset(NocturnalA[[k]], Aspeed=="0-50", select=Timestamp:DirectionS)
}

#wind rose (wind speed and direction)
for(k in 1:length(NocturnalA)){{
  s <- as.data.frame(NocturnalAP[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalAP[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=NocturnalAP[[k]]$Wspeed, colour=NocturnalAP[[k]]$Wspeed)) +
    scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Mean wind direction per hour (degrees) Autumn Noct Passerines",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,2000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirANP','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

#wind rose (air speed and direction)
for(k in 1:length(NocturnalAP)){{
  s <- as.data.frame(NocturnalAP[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalAP[[k]], aes(Mean.head,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=NocturnalAP[[k]]$Aspeed, colour=NocturnalAP[[k]]$Aspeed)) +
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)")+
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)")+
    ggtitle(paste("Mean heading per hour (degrees) Autumn Noct Passerines",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,2000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrasbdirANP','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

NocturnalSP <- list()
for(k in 1:length(NocturnalS)){
  
  NocturnalSP[[k]] <- subset(NocturnalS[[k]], Aspeed=="0-50", select=Timestamp:DirectionS)
}

#wind rose (wind speed and direction)
for(k in 1:length(NocturnalSP)){{
  s <- as.data.frame(NocturnalSP[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalSP[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=NocturnalSP[[k]]$Wspeed, colour=NocturnalSP[[k]]$Wspeed)) +
    scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Mean wind direction per hour (degrees) Spring Noct",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,3000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirSNP','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

#wind rose (air speed and direction)
for(k in 1:length(NocturnalSP)){{
  s <- as.data.frame(NocturnalSP[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalSP[[k]], aes(Mean.head,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=NocturnalSP[[k]]$Aspeed, colour=NocturnalSP[[k]]$Aspeed)) +
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)")+
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)")+
    ggtitle(paste("Mean heading per hour (degrees) Spring Noct",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,3000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrasbdirSNP','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}
