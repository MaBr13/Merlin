###################################
####noctural migration autumn######

#plot nocturnal and diurnal migrants
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(Autumn[[k]], aes(Light, Nr.tracks))+
    geom_point(aes(colour=Autumn[[k]]$Aspeed, shape=Autumn[[k]]$Aspeed), size=2, show.legend = T) +
    scale_shape_manual(values = c(17,20,3,1),name="Air speed (kph)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    ggtitle(paste("Number of nocturnal and diurnal migrants and their air speed Autumn",' ', datetime,sep='')) +
    xlab("Time of a day") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))+
    ggsave(filename=paste('nctrnA','_',datetime, ".jpeg", sep=''), 
           path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Autumn",scale=2)
}
  print(plot)
}
#plot "species" distirbution of nocturnal migrants

NocturnalA <- list()

for (k in 1:length(Autumn)){
  NocturnalA[[k]] <- subset(Autumn[[k]], Light==0, select=Timestamp:DirectionS)
}

for(k in 1:length(NocturnalA)){{
  s <- as.data.frame(NocturnalA[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(NocturnalA[[k]], aes(Aspeed, Nr.tracks))+
    geom_point(aes(colour=NocturnalA[[k]]$Aspeed, shape=NocturnalA[[k]]$Aspeed), size=2, show.legend = T) +
    scale_shape_manual(values = c(17,20,3,1),name="Air speed (kph)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    ggtitle(paste("Number and distribution of nocturnal migrants Autumn",' ', datetime,sep='')) +
    xlab("Time of a day") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_discrete(NocturnalA$Aspeed, labels=c("Potential passerines", "Other birds",
                                                 "Not birds", "Data n/a"))
  ggsave(filename=paste('nctrnmA','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Autumn",scale=2)
}
  print(plot)
}
