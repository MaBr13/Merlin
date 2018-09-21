#to extract separate months from the list
#October
October <- list()

for(k in 1:length(means)){
  
  October[[k]] <- subset(means[[k]], Month==10, select=Timestamp:Month)
}

library(ggplot2)
Oct <- list(October[[1]],October[[2]])
##plot October overview of the data (groundspeed and tracks) VER1

for(k in 1:length(Oct)){{
  s <- as.data.frame(Oct[[k]])
  datetime <- paste0(format(s[1,4], format="%Y/%m"))
  date1 <- format(as.Date(s[1,4], format="%Y/%m/%d"),"%Y/10/01")
  date2 <- format(as.Date(s[1,4], format="%Y/%m/%d"),"%Y/10/31")
  plot <-  ggplot(Oct[[k]], aes(Date,Nr.tracks)) +
    geom_point(aes(colour = Oct[[k]]$Mean.speed), alpha=1) +
    scale_colour_gradientn(colours = heat.colors(10), limits=c(0,145), 
                           guide = guide_colorbar(title=expression(paste('Groundspeed (kph)', sep=""))))+
    ggtitle(paste("Number of tracks and mean groundspeed per hour",' ', datetime,
                  sep='')) +
    xlab("Days") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_date(date_breaks="days", date_labels="%d", limits=c(as.Date(date1), as.Date(date2)))
 
}
  print(plot)
}


for(k in 1:length(Oct)){{
  s <- as.data.frame(Oct[[k]])
  datetime <- paste0(format(s[1,4], format="%Y/%m"))
  date1 <- format(as.Date(s[1,4], format="%Y/%m/%d"),"%Y/10/01")
  date2 <- format(as.Date(s[1,4], format="%Y/%m/%d"),"%Y/10/31")
  plot <-  ggplot(Oct[[k]], aes(Date,Nr.tracks)) +
    geom_point(aes(colour = Oct[[k]]$Mean.head), alpha=1) +
    scale_colour_gradientn(colours = heat.colors(15), limits=c(0,360), 
                           guide = guide_colorbar(title=expression(paste('Mean heading (degrees)', sep=""))))+
    ggtitle(paste("Number of tracks and mean heading per hour",' ', datetime,sep='')) +
    xlab("Days") + ylab("Number of tracks per hour") + ylim(0,4000)+
    scale_x_date(date_breaks="days", date_labels="%d", limits=c(as.Date(date1), as.Date(date2)))
  
}
  print(plot)
}

ggsave(filename=paste('trh','_',datetime, ".jpeg", sep=''), 
       path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
s <- as.data.frame(October[[3]])
datetime <- paste0(format(s[1,4], format="%Y/%m"))
date1 <- format(as.Date(s[1,4], format="%Y/%m/%d"),"%Y/10/01")
date2 <- format(as.Date(s[1,4], format="%Y/%m/%d"),"%Y/10/31")
plot <-  ggplot(October[[3]], aes(Date,Nr.tracks)) +
  geom_point(aes(colour = October[[3]]$Mean.speed), alpha=1) +
  scale_colour_gradientn(colours = heat.colors(10), limits=c(0,145), 
                         guide = guide_colorbar(title=expression(paste('Groundspeed (kph)', sep=""))))+
  ggtitle(paste("Number of tracks and mean groundspeed per hour",' ', datetime,
                sep='')) +
  xlab("Days") + ylab("Number of tracks per hour") + ylim(0,4000)+
  scale_x_date(date_breaks="days", date_labels="%d", limits=c(as.Date(date1), as.Date(date2)))




#March
March <- list()
for(k in 1:length(means)){
  
  March[[k]] <- subset(means[[k]], Month==3, select=Timestamp:Month)
}
