#########################################################
###########MERLIN RADAR DATA ANALYSIS####################
#########################################################

#this script analyses yearly data that is averaged over one hour intervals
#means is a list that contains averaged per hour data of all the observed years stored as separate list parts

library(ggplot2)

#wind rose (wind speed and direction)

for(k in 1:length(means)){{
  s <- as.data.frame(means[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(means[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=means[[k]]$Wspeed, colour=means[[k]]$Wspeed)) +
    scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Mean wind direction per hour (degrees)",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,4000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdir','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

#wind rose (air speed and direction)
for(k in 1:length(means)){{
  s <- as.data.frame(means[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(means[[k]], aes(Mean.head,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=means[[k]]$Aspeed, colour=means[[k]]$Aspeed)) +
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    ggtitle(paste("Mean heading per hour (degrees)",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,4000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrasbdir','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}
