##################################
###########spring#################

Spring <- list()
for(k in 1:length(means)){
  
  Spring[[k]] <- subset(means[[k]], Season==2, select=Timestamp:Direction)
}

#groundspeed and tracks
for(k in 1:length(means)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  date1 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/03/01")
  date2 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/06/01")
  plot <-  ggplot(Spring[[k]], aes(Date,Nr.tracks)) +
    geom_point(aes(colour = Spring[[k]]$Mean.speed), alpha=1) +
    scale_colour_gradient2(low='red', mid='yellow',high='black', breaks=c(0,50,100), limits=c(0,150),
                           labels=c("0-50","50-100", ">100"),
                           guide = guide_colorbar(title=expression(paste('Mean groundspeed (kph)', sep=""))))+
    ggtitle(paste("Number of tracks and mean groundspeed per hour",' ', datetime,sep='')) +
    xlab("Month") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_date(date_breaks="months", date_labels="%b", limits=c(as.Date(date1), as.Date(date2)))
  ggsave(filename=paste('trgspS','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/year",scale=2)
}
  print(plot)
}



for(k in 1:length(Spring)){{
  s <- as.data.frame(Spring[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  date1 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/03/01")
  date2 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/06/01")
  plot <-  ggplot(Spring[[k]], aes(Date,Nr.tracks)) +
    geom_point(aes(colour=Spring[[k]]$Aspeed, shape=Spring[[k]]$Aspeed), size=2, show.legend = T) +
    scale_shape_manual(values = c(17,20,3,1),name="Speed (kph)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Speed (kph)", drop=F)+
    ggtitle(paste("Number of tracks and mean groundspeed per hour Spring",' ', datetime,
                  sep='')) +
    xlab("Month") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_date(date_breaks="months", date_labels="%b", limits=c(as.Date(date1), as.Date(date2)))
  name <- paste0('trgspS','_',datetime, ".jpeg", sep='')
  ggsave(name, 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/Spring",scale=2)
  file.rename(from="blaa.png",to=name)
}
  print(plot)
}

###########################
##########autumn###########
Autumn <- list()

for(k in 1:length(means)){
  
  Autumn[[k]] <- subset(means[[k]], Season==4, select=Timestamp:Speed)
}

for(k in 1:length(means)){{
  s <- as.data.frame(means[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  date1 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/01/01")
  date2 <- format(as.Date(s[1,6], format="%Y/%m/%d"),"%Y/12/31")
  plot <-  ggplot(means[[k]], aes(Date,Nr.tracks)) +
    geom_point(aes(colour=means[[k]]$Speed, shape=means[[k]]$Speed)) +
    scale_shape_manual(values = c(17,20,3), name="Speed (kph)")+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0"), name="Speed (kph)")+
    ggtitle(paste("Number of tracks and mean groundspeed per hour",' ', datetime,
                  sep='')) +
    xlab("Month") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_date(date_breaks="months", date_labels="%b", limits=c(as.Date(date1), as.Date(date2)))
  ggsave(filename=paste('trgsp*','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal/year",scale=2)
}
  print(plot)
}
