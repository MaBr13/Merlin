NocturnalAT <- list()
for(k in 1:length(Autumn)){
  
  NocturnalAT[[k]] <- subset(AutumnT[[k]], light==0, select=id:DirectionS)
}

#wind rose (wind speed and direction)
for(k in 1:length(NocturnalAT)){{
  s <- as.data.frame(NocturnalAT[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalAT[[k]], aes(winddir,id)) + 
    geom_bar(stat="identity", aes(fill=NocturnalAT[[k]]$Wspeed, colour=NocturnalAT[[k]]$Wspeed)) +
    scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Wind direction (degrees) Autumn Noct",' ', datetime,
                  sep='')) +
    ylab("Total number of tracks")+ylim(0,8000000000000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirANT','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

#wind rose (air speed and direction)
for(k in 1:length(NocturnalAT)){{
  s <- as.data.frame(NocturnalAT[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalAT[[k]], aes(trackheading,id)) + 
    geom_bar(stat="identity", aes(fill=NocturnalAT[[k]]$Aspeed, colour=NocturnalAT[[k]]$Aspeed)) +
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    ggtitle(paste("Heading (degrees) Autumn Noct",' ', datetime,
                  sep='')) +
    ylab("Total number of tracks")+ylim(0,8000000000000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrasbdirANT','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

NocturnalST <- list()
for(k in 1:length(Spring)){
  
  NocturnalST[[k]] <- subset(SpringT[[k]], light==0, select=id:DirectionS)
}

for(k in 1:length(NocturnalST)){{
  s <- as.data.frame(NocturnalST[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalST[[k]], aes(winddir,id)) + 
    geom_bar(stat="identity", aes(fill=NocturnalST[[k]]$Wspeed, colour=NocturnalST[[k]]$Wspeed)) +
    scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Wind direction (degrees) Autumn Noct",' ', datetime,
                  sep='')) +
    ylab("Total number of tracks")+ylim(0,8000000000000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirSNT','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

#wind rose (air speed and direction)
for(k in 1:length(NocturnalST)){{
  s <- as.data.frame(NocturnalST[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalST[[k]], aes(trackheading,id)) + 
    geom_bar(stat="identity", aes(fill=NocturnalST[[k]]$Aspeed, colour=NocturnalST[[k]]$Aspeed)) +
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    ggtitle(paste("Heading (degrees) Autumn Noct",' ', datetime,
                  sep='')) +
    ylab("Total number of tracks")+ylim(0,8000000000000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrasbdirSNT','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}