######################################################################
############HOURLY BASIS RESEARCH DURING HIGH MIGRATION NIGHTS########
######################################################################


####***requires loading of starter pack data in bigger tables folder


require(ggplot2)

for (k in 1:length(Allyears)){
  Allyears[[k]]$Aspeed <- factor(Allyears[[k]]$Aspeed, levels = rev(levels(Allyears[[k]]$Aspeed)))
  
}


library(lubridate)
for(k in 1:length(Allyears)){
  Allyears[[k]]$date <- with(Allyears[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  Allyears[[k]]$timestep <- with(Allyears[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
}

library(lubridate)
Sys.setenv(TZ="UTC")
Sys.setlocale(category = "LC_ALL", locale = "English_United Kingdom.1252")#set the time on your computer to match
#sunset and sunrise in UTC
sunset <- c(20071013165100, 20071020163600,20081030161400,20080328180800,20100316174600)
sunrise <-c(20071014060300,20071021061500,20081031063500,20080329052100,20100317055100)

sun <- list()

for (k in 1:length(DaysA)){
  sun <- getSunlightTimes(DaysA[[k]]$date,52.60636,4.389639,keep = c("sunrise","sunset"),tz="UTC")
}


#SELECT DAYS WITH HIGH MIGRATION
Oct1a <- subset(Allyears[[1]], timestep>="2007-10-13 16:00:00 UTC" & timestep<="2007-10-14 16:00:00 UTC" , select = id:Wspeed)
Oct2a <- subset(Allyears[[1]], timestep>="2007-10-20 16:00:00 UTC" & timestep<="2007-10-21 16:00:00 UTC", select = id:Wspeed)
Oct3a <- subset(Allyears[[2]], timestep>="2008-10-30 16:00:00 UTC" & timestep<="2008-10-31 16:00:00 UTC", select = id:Wspeed)
Mar1a <- subset(Allyears[[2]], timestep>="2008-03-28 16:00:00 UTC" & timestep<="2008-03-29 16:00:00 UTC", select = id:Wspeed)
Mar2a <- subset(Allyears[[4]], timestep>="2010-03-16 16:00:00 UTC" & timestep<="2010-03-17 16:00:00 UTC", select = id:Wspeed)

Oct1m <- subset(means[[1]], Timestamp>"2007-10-13 16:00:00" & Timestamp<"2007-10-14 16:00:00", select = Timestamp:Rlight)
Oct2m <- subset(means[[1]], Timestamp>"2007-10-20 16:00:00" & Timestamp<"2007-10-21 16:00:00", select = Timestamp:Rlight)
Oct3m <- subset(means[[2]], Timestamp>"2008-10-30 16:00:00" & Timestamp<"2008-10-31 16:00:00", select = Timestamp:Rlight)
Mar1m <- subset(means[[2]], Timestamp>"2008-03-28 16:00:00" & Timestamp<"2008-03-29 16:00:00", select = Timestamp:Rlight)
Mar2m <- subset(means[[4]], Timestamp>"2010-03-16 16:00:00" & Timestamp<"2010-03-17 16:00:00", select = Timestamp:Rlight)


DaysA <- list(Oct1a,Oct2a,Oct3a,Mar1a,Mar2a)
DaysM <- list(Oct1m,Oct2m,Oct3m,Mar1m,Mar2m)
library(dplyr)
for (k in 1:length(DaysA)){
 DaysA[[k]] <- DaysA[[k]] %>% arrange(timestep)
}

as.numeric(as.character(s[1,16]))

###PLOT NUMBER OF TRACKS AND COMPOSITION OF MIGRANTS (to see how it changes during the course of the night)
for(k in 1:length(DaysA)){{
  s <- as.data.frame(DaysA[[k]])
  datetime <- paste0(format(s[1,15], format="%Y-%m-%d"))
  plot <-  ggplot(DaysA[[k]],aes(DaysA[[k]]$timestep)) +
    geom_bar(aes(colour=DaysA[[k]]$Aspeed, fill=DaysA[[k]]$Aspeed), show.legend = T) +
    annotate("rect",xmin = ymd_hms(sunrise[k]), xmax = ymd_hms(sunset[k]), 
             ymin = 0, ymax = Inf, fill="navy",alpha=0.2 )+
    scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
    scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
    ggtitle(paste("Number of tracks per hour",' ', datetime,sep='')) +
    xlab("Hour") + ylab("Number of tracks") +
    theme(axis.title.y = element_text(size=18), axis.title.x = element_blank(),legend.text=element_text(size=12), 
          legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
          axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(1,1,1,1), "cm"),
          plot.title = element_text(size = 18, face = "bold"))
  ggsave(filename=paste('Nrtrasp','_',datetime, ".png", sep=''), 
         path="C:/Users/mbradar/Documents/MERLIN/OWEZ/plots/horizontal",height=9,width=12,dpi=72)
}
  print(plot)
}


###SEE HOW THE DIRECTION CHANGES DURING THE COURSE OF THE NIGHT
for(k in 1:length(DaysM)){{
  s <- as.data.frame(DaysM[[k]])
  datetime <- paste0(format(s[1,6], format="%Y-%m-%d"))
plot <- ggplot(DaysM[[k]], aes(Timestamp,b.heading)) + 
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

ggplot(DaysM[[2]], aes(Timestamp,b.heading)) + 
  geom_line(colour="cyan3",size=1.5) +
  geom_point(colour="darkcyan",size=3)+
  ggtitle(paste("Track direction per hour",' ', datetime,sep='')) + 
  ylab("heading (degrees)")+ylim(0,360)+xlab("Hour")+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), 
        plot.title = element_text(size = 16),plot.margin = unit(c(1,1,1,1), "cm"))
