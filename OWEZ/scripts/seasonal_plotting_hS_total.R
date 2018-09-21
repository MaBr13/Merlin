#################################################################
###################MERLIN RADAR DATA#############################
#################################################################

##visualization of spring number of nocturnal and diurnal migrants based on total number of tracks

##take out the spring data

SpringT <- list()
for(k in 1:length(Spring)){
  
  SpringT[[k]] <- subset(Allyears[[k]], season==2, select=id:DirectionS)
}


for(k in 1:length(SpringT)){{
  s <- as.data.frame(SpringT[[k]])
  datetime <- paste0(format(s[1,15], format="%Y"))
  plot <- ggplot(SpringT[[k]], aes(light, id))+
    ylim(0,8000000000000)+
    geom_bar(stat="identity" ,aes(colour=SpringT[[k]]$Aspeed, fill=SpringT[[k]]$Aspeed), show.legend = T) +
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    ggtitle(paste("Total number of migrants and their air speed Spring",' ', datetime,sep='')) +
    xlab("Time of a day") + ylab("Number of tracks") + 
    scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))+
    ggsave(filename=paste('nctrnST','_',datetime, ".jpeg", sep=''), 
           path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

for(k in 1:length(SpringT)){{
  s <- as.data.frame(SpringT[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(SpringT[[1]], aes(winddir,id)) + 
    geom_bar(stat="identity", aes(fill=SpringT[[1]]$Wspeed, colour=SpringT[[1]]$Wspeed)) +
    scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Wind speed (kph)", drop=F)+
    scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Wind speed (kph)", drop=F)+
    ylab("Number of tracks")+ylim(0,8000000000000)+
    coord_polar(start = 0) +
    ggtitle(paste("Heading (degrees) Spring",' ', datetime,
                  sep=''))
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirST','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}


