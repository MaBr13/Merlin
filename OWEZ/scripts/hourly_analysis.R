######################################################################
############HOURLY BASIS RESEARCH DURING HIGH MIGRATION NIGHTS########
######################################################################


####***requires loading of starter pack data in bigger tables folder


require(ggplot2)

for (k in 1:length(Allyears)){
  Allyears[[k]]$Aspeed <- factor(Allyears[[k]]$Aspeed, levels = rev(levels(Allyears[[k]]$Aspeed)))
  
}

#SELECT DAYS WITH HIGH MIGRATION
Oct1a <- subset(Allyears[[1]], date=="2007-10-13", select = id:Wspeed)
Oct2a <- subset(Allyears[[1]], date=="2007-10-20", select = id:Wspeed)
Oct3a <- subset(Allyears[[2]], date=="2008-10-30", select = id:Wspeed)
Mar1a <- subset(Allyears[[2]], date=="2008-03-28", select = id:Wspeed)
Mar2a <- subset(Allyears[[4]], date=="2010-03-16", select = id:Wspeed)

Oct1m <- subset(means[[1]], date=="2007-10-13", select = Timestamp:Rlight)
Oct2m <- subset(means[[1]], date=="2007-10-20", select = Timestamp:Rlight)
Oct3m <- subset(means[[2]], date=="2008-10-30", select = Timestamp:Rlight)
Mar1m <- subset(means[[2]], date=="2008-03-28", select = Timestamp:Rlight)
Mar2m <- subset(means[[4]], date=="2010-03-16", select = Timestamp:Rlight)

DaysA <- list(Oct1a,Oct2a,Oct3a,Mar1a,Mar2a)
DaysM <- list(Oct1m,Oct2m,Oct3m,Mar1m,Mar2m)

####CALCULATE NEW WIND DIRECTION (DIRECTION THE WIND BLOWS TO)
for (k in 1:length(DaysM)){
  
  
  DaysM[[k]]$new.winddir <- ifelse(DaysM[[k]]$Mean.wdir<180.0001,DaysM[[k]]$Mean.wdir+180,DaysM[[k]]$Mean.wdir-180)  
}
####CALCULATE REAL HEADING

for (k in 1:length(DaysM)){
  
  DaysM[[k]]$groundspeedms <- DaysM[[k]]$Mean.speed/3.6
  DaysM[[k]]$windspeedms <- DaysM[[k]]$Mean.wspeed/3.6
  trackheadingR <- DaysM[[k]]$Mean.head*(pi/180)#formula for conversion to radians
  winddirR <- DaysM[[k]]$new.winddir*(pi/180)
  strack<- sin(trackheadingR)#calculate sinus and cosinus of track and wind direction
  ctrack <- cos(trackheadingR)
  swind <- sin(winddirR)
  cwind <- cos(winddirR)
  
  xa <- (DaysM[[k]]$groundspeedms*strack)-(DaysM[[k]]$windspeedms*swind)
  ya <- (DaysM[[k]]$groundspeedms*ctrack)-(DaysM[[k]]$windspeedms*cwind)
  
  heading<- atan2(xa,ya)
  DaysM[[k]]$airspeedms<-sqrt((xa^2)+(ya^2)) 
  DaysM[[k]]$r.heading <- heading*(180/pi)#formula for conversion back to angles
  
}
####GET RID OF NEGATIVE HEADING VALUES
for(k in 1:length(DaysM)){
  DaysM[[k]]$b.heading <-  ifelse(DaysM[[k]]$r.heading<0, 360+DaysM[[k]]$r.heading, DaysM[[k]]$r.heading)
}
###PLOT NUMBER OF TRACKS AND COMPOSITION OF MIGRANTS (to see how it changes during the course of the night)
for(k in 1:length(DaysA)){{
  s <- as.data.frame(DaysA[[k]])
  datetime <- paste0(format(s[1,15], format="%Y-%m-%d"))
  plot <-  ggplot(DaysA[[k]], aes(uur,id)) +
    geom_col(aes(colour=DaysA[[k]]$Aspeed, fill=DaysA[[k]]$Aspeed), show.legend = T) +
    scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
    scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
    ggtitle(paste("Number of tracks per hour",' ', datetime,sep='')) +
    xlab("Hour") + ylab("Number of tracks") +
    theme(axis.title.y = element_text(size=18), axis.title.x = element_blank(),legend.text=element_text(size=12), 
          legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
          axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(1,1,1,1), "cm"),
          plot.title = element_text(size = 18, face = "bold"))
  ggsave(filename=paste('Nrtrasp','_',datetime, ".png", sep=''), 
         path="C:/Users/mbradar/Documents/Merlin/OWEZ/plots/horizontal/Hourly",height=9,width=12,dpi=72)
}
  print(plot)
}

###SEE HOW THE DIRECTION CHANGES DURING THE COURSE OF THE NIGHT
for(k in 1:length(DaysM)){{
  s <- as.data.frame(DaysM[[k]])
  datetime <- paste0(format(s[1,6], format="%Y-%m-%d"))
plot <- ggplot(DaysM[[k]], aes(Timestamp,b.head)) + 
  geom_line(colour="cyan3",size=1.5) +
  geom_point(colour="darkcyan",size=3)+
  ggtitle(paste("Track direction per hour",' ', datetime,sep='')) + 
  ylab("heading (degrees)")+ylim(0,360)+xlab("Hour")+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), 
        plot.title = element_text(size = 16),plot.margin = unit(c(1,1,1,1), "cm"))
ggsave(filename=paste('trdirph','_',datetime, ".png", sep=''), 
       path="C:/Users/mbradar/Documents/Merlin/OWEZ/plots/horizontal/Hourly",height=9,width=12,dpi=72)
}
  print(plot)
}
