#################################################################
###################MERLIN RADAR DATA#############################
#################################################################

##visualization of Autumn number of nocturnal and diurnal migrants based on total number of tracks

AutumnT <- list()
for(k in 1:length(Autumn)){
  
  AutumnT[[k]] <- subset(Allyears[[k]], season==4, select=id:DirectionS)
}


for(k in 1:length(AutumnT)){{
  s <- as.data.frame(AutumnT[[k]])
  datetime <- paste0(format(s[1,15], format="%Y"))
  plot <- ggplot(AutumnT[[k]], aes(light, id))+
    ylim(0,8000000000000)+
    geom_bar(stat="identity" ,aes(colour=AutumnT[[k]]$Aspeed, fill=AutumnT[[k]]$Aspeed), show.legend = T) +
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    ggtitle(paste("Total number of migrants and their air speed Autumn",' ', datetime,sep='')) +
    xlab("Time of a day") + ylab("Number of tracks") + 
    scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))+
    ggsave(filename=paste('nctrnAT','_',datetime, ".jpeg", sep=''), 
           path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

for(k in 1:length(AutumnT)){{
  s <- as.data.frame(AutumnT[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(AutumnT[[1]], aes(winddir,id)) + 
    geom_bar(stat="identity", aes(fill=AutumnT[[1]]$Wspeed, colour=AutumnT[[1]]$Wspeed)) +
    scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Wind speed (kph)", drop=F)+
    scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Wind speed (kph)", drop=F)+
    ylab("Number of tracks")+ylim(0,8000000000000)+
    coord_polar(start = 0) +
    ggtitle(paste("Heading (degrees) Autumn",' ', datetime,
                  sep=''))
  scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirST','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}


