#################################################################
###################MERLIN RADAR DATA#############################
#################################################################

##################################
###########spring#################

##visualizations of the data that is averaged per hour

Spring <- list()
for(k in 1:length(means)){
  
  Spring[[k]] <- subset(means[[k]], Season==2, select=Timestamp:Rlight)
}

#ground speed and tracks
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])+
  datetime <- paste0(format(s[1,6], format="%Y"))
  date1 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/03/01")
  date2 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/06/01")
  plot <-  ggplot(Spring[[k]], aes(Date,Nr.tracks)) +
    geom_point(aes(colour = Spring[[k]]$Mean.speed), alpha=1) +
    scale_colour_gradient2(low='red', mid='yellow',high='black', breaks=c(0,50,100), limits=c(0,150),
                           labels=c("0-50","50-100", ">100"),
                           guide = guide_colorbar(title=expression(paste('Mean ground speed (kph)', sep=""))))+
    ggtitle(paste("Number of tracks and mean ground speed per hour",' ', datetime,sep='')) +
    xlab("Month") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_date(date_breaks="months", date_labels="%b", limits=c(as.Date(date1), as.Date(date2)))
  ggsave(filename=paste('trgspS','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Spring",scale=2)
}
  print(plot)
}

##plot seasonal overview of the data (groundspeed and tracks) VER2
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <-  ggplot(Spring[[k]], aes(Mean.speed,Nr.tracks)) +
    geom_point(size=0.5) +
    ggtitle(paste("Ground speeds used by birds Spring",' ', datetime,
                  sep='')) +
    xlab("Ground speed") + ylab("Number of tracks per hour") + xlim(25,130) + ylim(0,4000)+
    ggsave(filename=paste('trgspSv2','_',datetime, ".jpeg", sep=''), 
           path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Spring",scale=2)
}
  print(plot)
}


#air speed categories in different times
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  date1 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/03/01")
  date2 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/06/01")
  plot <-  ggplot(Spring[[k]], aes(Date,Nr.tracks)) +
    geom_point(aes(colour=Spring[[k]]$Aspeed, shape=Spring[[k]]$Aspeed), size=2, show.legend = T) +
    scale_shape_manual(values = c(17,20,3,1),name="Air speed (kph)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    ggtitle(paste("Number of tracks and mean air speed per hour Spring",' ', datetime,
                  sep='')) +
    xlab("Month") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_date(date_breaks="months", date_labels="%b", limits=c(as.Date(date1), as.Date(date2)))
  name <- paste0('traspScat','_',datetime, ".jpeg", sep='')
  ggsave(name, 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Spring",scale=2)
}
  print(plot)
}
##plot seasonal overview of the data (airspeed and tracks) VER2
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <-  ggplot(Spring[[k]], aes(Mean.aspeed,Nr.tracks)) +
    geom_point(size=0.5) +
    ggtitle(paste("Air speeds used by birds Spring",' ', datetime,
                  sep='')) +
    xlab("Air speed") + ylab("Number of tracks per hour") + xlim(25,130) + ylim(0,4000)+
    ggsave(filename=paste('traspSv2','_',datetime, ".jpeg", sep=''), 
           path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Spring",scale=2)
}
  print(plot)
}

#plot seasonal overview of the data (headings and tracks) VER1
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  date1 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/03/01")
  date2 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/06/01")
  plot <-  ggplot(Spring[[k]], aes(Date,Nr.tracks)) +
    geom_point(aes(colour=Spring[[k]]$DirectionS, shape=Spring[[k]]$DirectionS)) +
    scale_shape_manual(values = c(17,4,3,20),name="Direction (degrees)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen","black","grey"), name="Direction (degrees)", drop=F)+
    ggtitle(paste("Number of tracks and mean heading per hour Spring",' ', datetime,sep='')) +
    xlab("Month") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_date(date_breaks="months", date_labels="%b", limits=c(as.Date(date1), as.Date(date2)))
  ggsave(filename=paste('trhS','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Spring",scale=2)
}
  print(plot)
}


#plot seasonal overview of the data (headings and tracks) VER2
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <-  ggplot(Spring[[k]], aes(Mean.head,Nr.tracks)) +
    geom_point(size=0.5) +
    ggtitle(paste("Headings used by birds Spring",' ', datetime, sep='')) +
    xlab("Mean heading per hour (degrees)") + ylab("Number of tracks per hour") + ylim(0,4000) + xlim(0,360)
  ggsave(filename=paste('trhv2','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Spring",scale=2)
}
  print(plot)
}

#plot seasonal overview of the data (headings, winddir and tracks) VER1
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[1]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  date1 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/01/01")
  date2 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/12/31")
  plot <-  ggplot(Spring[[k]], aes(Mean.wdir, Nr.tracks)) +
    geom_point(aes(colour=Spring[[k]]$DirectionS, shape=Spring[[k]]$DirectionS)) +
    scale_shape_manual(values = c(17,4,3,20),name="Direction (degrees)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen","black","grey"), name="Direction (degrees)", drop=F)+
    ggtitle(paste("Number of tracks, mean wind dir and mean heading per hour",' ', datetime,sep='')) +
    xlab("Mean wind direction per hour (kph)") + ylab("Number of tracks per hour") + ylim(0,4000) +xlim(0,360)+
    ggsave(filename=paste('trhwdirS','_',datetime, ".jpeg", sep=''), 
           path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Spring",scale=2)
}
  print(plot)
}

#plot yearly overview of the data (headings and winddir)
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <-  ggplot(Spring[[k]], aes(Mean.head,Mean.wdir)) +
    geom_point(size=0.5) +
    ggtitle(paste("Headings used by birds and wind direction Spring",' ', datetime, sep='')) +
    xlab("Mean heading per hour (degrees)") + ylab("Mean wind direction per hour (kph)") + ylim(0,360) + xlim(0,360)
  ggsave(filename=paste('hwdirSv2','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Spring",scale=2)
}
  print(plot)
}

#headings and air speeds
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <-  ggplot(Spring[[k]], aes(Mean.head,Nr.tracks)) +
    geom_point(aes(colour=Spring[[k]]$Aspeed, shape=Spring[[k]]$Aspeed)) +
    scale_shape_manual(values = c(17,20,3,1),name="Air speed (kph)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    ggtitle(paste("Number of tracks,mean heading and mean air speed per hour",' ', datetime,
                  sep='')) +
    xlab("Mean heading per hour (degrees)") + ylab("Number of tracks per hour") + ylim(0,4000)
  ggsave(filename=paste('trasphS','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Spring",scale=2)
}
  print(plot)
}

#wind rose (wind speed and direction)
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(Spring[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=Spring[[k]]$Wspeed, colour=Spring[[k]]$Wspeed)) +
    scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
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

#wind rose (air speed and direction)
for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(Spring[[k]], aes(Mean.head,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=Spring[[k]]$Aspeed, colour=Spring[[k]]$Aspeed)) +
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    ggtitle(paste("Mean heading per hour (degrees) Spring",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,4000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrasbdirS','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

