#################################################################
###################MERLIN RADAR DATA#############################
#################################################################

##visualization of seasonal wind directions


#wind rose (wind speed and direction)
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(Spring[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=Spring[[k]]$Migrwind, colour=Spring[[k]]$Migrwind)) +
    scale_colour_hue(name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_hue(name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Mean wind direction per hour (degrees) Spring",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,4000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirS','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

#wind rose (wind speed and direction)
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(Autumn[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=Autumn[[k]]$Migrwind, colour=Autumn[[k]]$Migrwind)) +
    scale_colour_hue(name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_hue(name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Mean wind direction per hour (degrees) Autumn",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,4000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirA','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}
