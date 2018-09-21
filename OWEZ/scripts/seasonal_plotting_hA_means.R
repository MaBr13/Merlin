#################################################################
###################MERLIN RADAR DATA#############################
#################################################################

###########################
##########Autumn###########

##visualizations of the data that is averaged per hour

Autumn <- list()

for(k in 1:length(means)){
  
  Autumn[[k]] <- subset(means[[k]], Season==4, select=Timestamp:Rlight)
}

#ground speed and tracks
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  date1 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/09/01")
  date2 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/12/01")
  plot <-  ggplot(Autumn[[k]], aes(Date,Nr.tracks)) +
    geom_point(aes(colour = Autumn[[k]]$Mean.speed), alpha=1) +
    scale_colour_gradient2(low='red', mid='yellow',high='black', breaks=c(0,50,100), limits=c(0,150),
                           labels=c("0-50","50-100", ">100"),
                           guide = guide_colorbar(title=expression(paste('Mean ground speed (kph)', sep=""))))+
    ggtitle(paste("Number of tracks and mean ground speed per hour Autumn",' ', datetime,sep='')) +
    xlab("Month") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_date(date_breaks="months", date_labels="%b", limits=c(as.Date(date1), as.Date(date2)))
  ggsave(filename=paste('trgspA','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Autumn",scale=2)
}
  print(plot)
}

##plot seasonal overview of the data (groundspeed and tracks) VER2
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <-  ggplot(Autumn[[k]], aes(Mean.speed,Nr.tracks)) +
    geom_point(size=0.5) +
    ggtitle(paste("Ground speeds used by birds Autumn",' ', datetime,
                  sep='')) +
    xlab("Ground speed") + ylab("Number of tracks per hour") + xlim(25,130) + ylim(0,4000)+
    ggsave(filename=paste('trgspAv2','_',datetime, ".jpeg", sep=''), 
           path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Autumn",scale=2)
}
  print(plot)
}


#air speed categories in different times
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  date1 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/09/01")
  date2 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/12/01")
  plot <-  ggplot(Autumn[[k]], aes(Date,Nr.tracks)) +
    geom_point(aes(colour=Autumn[[k]]$Aspeed, shape=Autumn[[k]]$Aspeed), size=2, show.legend = T) +
    scale_shape_manual(values = c(17,20,3,1),name="Air speed (kph)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    ggtitle(paste("Number of tracks and mean air speed per hour Autumn",' ', datetime,
                  sep='')) +
    xlab("Month") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_date(date_breaks="months", date_labels="%b", limits=c(as.Date(date1), as.Date(date2)))
  name <- paste0('traspAcat','_',datetime, ".jpeg", sep='')
  ggsave(name, 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Autumn",scale=2)
}
  print(plot)
}
##plot seasonal overview of the data (airspeed and tracks) VER2
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <-  ggplot(Autumn[[k]], aes(Mean.aspeed,Nr.tracks)) +
    geom_point(size=0.5) +
    ggtitle(paste("Air speeds used by birds Autumn",' ', datetime,
                  sep='')) +
    xlab("Air speed") + ylab("Number of tracks per hour") + xlim(25,130) + ylim(0,4000)+
    ggsave(filename=paste('traspAv2','_',datetime, ".jpeg", sep=''), 
           path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Autumn",scale=2)
}
  print(plot)
}

#plot seasonal overview of the data (headings and tracks) VER1
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  date1 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/09/01")
  date2 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/12/01")
  plot <-  ggplot(Autumn[[k]], aes(Date,Nr.tracks)) +
    geom_point(aes(colour=Autumn[[k]]$DirectionA, shape=Autumn[[k]]$DirectionA)) +
    scale_shape_manual(values = c(17,4,3,20),name="Direction (degrees)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen","black","grey"), name="Direction (degrees)", drop=F)+
    ggtitle(paste("Number of tracks and mean heading per hour Autumn",' ', datetime,sep='')) +
    xlab("Month") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_date(date_breaks="months", date_labels="%b", limits=c(as.Date(date1), as.Date(date2)))
  ggsave(filename=paste('trhA','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Autumn",scale=2)
}
  print(plot)
}

#plot seasonal overview of the data (headings and tracks) VER2
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <-  ggplot(Autumn[[k]], aes(Mean.head,Nr.tracks)) +
    geom_point(size=0.5) +
    ggtitle(paste("Headings used by birds Autumn",' ', datetime, sep='')) +
    xlab("Mean heading per hour (degrees)") + ylab("Number of tracks per hour") + ylim(0,4000) + xlim(0,360)
  ggsave(filename=paste('trhAv2','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Autumn",scale=2)
}
  print(plot)
}

#plot seasonal overview of the data (headings, winddir and tracks) VER1
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[1]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  date1 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/01/01")
  date2 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/12/31")
  plot <-  ggplot(Autumn[[k]], aes(Mean.wdir, Nr.tracks)) +
    geom_point(aes(colour=Autumn[[k]]$DirectionA, shape=Autumn[[k]]$DirectionA)) +
    scale_shape_manual(values = c(17,4,3,20),name="Direction (degrees)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen","black","grey"), name="Direction (degrees)", drop=F)+
    ggtitle(paste("Number of tracks, mean wind dir and mean heading per hour Autumn",' ', datetime,sep='')) +
    xlab("Mean wind direction per hour (kph)") + ylab("Number of tracks per hour") + ylim(0,4000) +xlim(0,360)+
    ggsave(filename=paste('trhwdirA','_',datetime, ".jpeg", sep=''), 
           path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Autumn",scale=2)
}
  print(plot)
}

#plot yearly overview of the data (headings and winddir)
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <-  ggplot(Autumn[[k]], aes(Mean.head,Mean.wdir)) +
    geom_point(size=0.5) +
    ggtitle(paste("Headings used by birds and wind direction Autumn",' ', datetime, sep='')) +
    xlab("Mean heading per hour (degrees)") + ylab("Mean wind direction per hour (kph)") + ylim(0,360) + xlim(0,360)
  ggsave(filename=paste('hwdirAv2','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Autumn",scale=2)
}
  print(plot)
}

#headings and air speeds
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <-  ggplot(Autumn[[k]], aes(Mean.head,Nr.tracks)) +
    geom_point(aes(colour=Autumn[[k]]$Aspeed, shape=Autumn[[k]]$Aspeed)) +
    scale_shape_manual(values = c(17,20,3,1),name="Air speed (kph)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    ggtitle(paste("Number of tracks,mean heading and mean air speed per hour",' ', datetime,
                  sep='')) +
    xlab("Mean heading per hour (degrees)") + ylab("Number of tracks per hour") + ylim(0,4000)+
    ggsave(filename=paste('trasphA','_',datetime, ".jpeg", sep=''), 
           path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Autumn",scale=2)
}
  print(plot)
}

#wind rose (wind speed and direction)
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(Autumn[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=Autumn[[k]]$Wspeed, colour=Autumn[[k]]$Wspeed)) +
    scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Mean Wind speed ph (kph)", drop=F)+
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

#wind rose (air speed and direction)
for(k in 1:length(Autumn)){{
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(Autumn[[k]], aes(Mean.head,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=Autumn[[k]]$Aspeed, colour=Autumn[[k]]$Aspeed)) +
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Mean Air speed ph (kph)", drop=F)+
    ggtitle(paste("Mean heading per hour (degrees) Autumn",' ', datetime,
                  sep='')) +
    ylab("Mean number of tracks per hour")+ylim(0,4000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrasbdirA','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}


