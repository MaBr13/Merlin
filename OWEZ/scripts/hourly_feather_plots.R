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

#SELECT DAYS WITH HIGH MIGRATION
Oct1a <- subset(Allyears[[1]], timestep>"2007-10-13 16:00:00" & timestep<"2007-10-14 16:00:00" , select = id:Wspeed)
Oct2a <- subset(Allyears[[1]], timestep>"2007-10-20 16:00:00" & timestep<"2007-10-21 16:00:00", select = id:Wspeed)
Oct3a <- subset(Allyears[[2]], timestep>"2008-10-30 16:00:00" & timestep<"2008-10-31 16:00:00", select = id:Wspeed)
Mar1a <- subset(Allyears[[2]], timestep>"2008-03-28 16:00:00" & timestep<"2008-03-29 16:00:00", select = id:Wspeed)
Mar2a <- subset(Allyears[[4]], timestep>"2010-03-16 16:00:00" & timestep<"2010-03-17 16:00:00", select = id:Wspeed)

Oct1m <- subset(means[[1]], Timestamp>"2007-10-13 16:00:00" & Timestamp<"2007-10-14 16:00:00", select = Timestamp:Rlight)
Oct2m <- subset(means[[1]], Timestamp>"2007-10-20 16:00:00" & Timestamp<"2007-10-21 16:00:00", select = Timestamp:Rlight)
Oct3m <- subset(means[[2]], Timestamp>"2008-10-30 16:00:00" & Timestamp<"2008-10-31 16:00:00", select = Timestamp:Rlight)
Mar1m <- subset(means[[2]], Timestamp>"2008-03-28 16:00:00" & Timestamp<"2008-03-29 16:00:00", select = Timestamp:Rlight)
Mar2m <- subset(means[[4]], Timestamp>"2010-03-16 16:00:00" & Timestamp<"2010-03-17 16:00:00", select = Timestamp:Rlight)

DaysA <- list(Oct1a,Oct2a,Oct3a,Mar1a,Mar2a)
DaysM <- list(Oct1m,Oct2m,Oct3m,Mar1m,Mar2m)



####CALCULATE NEW WIND DIRECTION (DIRECTION THE WIND BLOWS TO)
for (k in 1:length(DaysM)){
  
  
  DaysM[[k]]$winddir.rad <- DaysM[[k]]$Mean.wdir*(pi/180)
  DaysM[[k]]$heading.rad <- DaysM[[k]]$Mean.head*(pi/180)
  DaysM[[k]]$tr.dir.rad <- DaysM[[k]]$Mean.tr.dir*(pi/180)
}


library(dplyr)

for(k in 1:length(DaysM)){
  DaysM[[k]] <- filter(DaysM[[k]],  !is.na(heading.rad))
}


library(plotrix)

#r - the length of vector (for example wind strength)
#theta - the angle (wind direction) in radians
#line-position a title
#las-orientation of the title
#adj-position of a text in the title (left, right)


###add colouring based on airspeed and windspeed

for (k in 1:length(DaysM)){
  DaysM[[k]]$Colour[DaysM[[k]]$Mean.aspeed>=3 & DaysM[[k]]$Mean.aspeed<=12]="maroon"
  DaysM[[k]]$Colour[DaysM[[k]]$Mean.aspeed>12 & DaysM[[k]]$Mean.aspeed<=15]="coral3"
  DaysM[[k]]$Colour[DaysM[[k]]$Mean.aspeed>15 & DaysM[[k]]$Mean.aspeed<=18]="firebrick1"
  DaysM[[k]]$Colour[DaysM[[k]]$Mean.aspeed>18 & DaysM[[k]]$Mean.aspeed<=27]="forestgreen"
  DaysM[[k]]$Colour[DaysM[[k]]$Mean.aspeed>27]="grey0"
 # DaysM[[k]]$Colour[DaysM[[k]]$Mean.aspeed=NA]="grey"
  DaysM[[k]]$Colour1[DaysM[[k]]$Mean.wspeed>=0 & DaysM[[k]]$Mean.wspeed<=5]="cadetblue1"
  DaysM[[k]]$Colour1[DaysM[[k]]$Mean.wspeed>5 & DaysM[[k]]$Mean.wspeed<=10]="cornflowerblue"
  DaysM[[k]]$Colour1[DaysM[[k]]$Mean.wspeed>10 & DaysM[[k]]$Mean.wspeed<=15]="steelblue"
  DaysM[[k]]$Colour1[DaysM[[k]]$Mean.wspeed>15 & DaysM[[k]]$Mean.wspeed<=20]="deepskyblue4"
  DaysM[[k]]$Colour1[DaysM[[k]]$Mean.wspeed>20 & DaysM[[k]]$Mean.wspeed<=25]="dodgerblue3"
  DaysM[[k]]$Colour1[DaysM[[k]]$Mean.wspeed>25 & DaysM[[k]]$Mean.wspeed<=30]="mediumblue"
  DaysM[[k]]$Colour1[DaysM[[k]]$Mean.wspeed>30 & DaysM[[k]]$Mean.wspeed<=45]="blue"
  DaysM[[k]]$Colour1[DaysM[[k]]$Mean.wspeed>35 & DaysM[[k]]$Mean.wspeed<=40]="navy"
  DaysM[[k]]$Colour1[DaysM[[k]]$Mean.wspeed>40]="yellow"
}


###feather.plot2 function is in another script called changed_featherf

for (k in 1:length(DaysM)){
  s <- as.data.frame(DaysM[[k]])
  datetime <- paste0(format(s[1,6], format="%Y-%m-%d"))
  png(paste0("C:/Users/mbradar/Documents/Merlin/OWEZ/plots/horizontal/DaysM_heading",datetime,".png"),height=4,width=8,unit="in",res=500)
  feather.plot2(DaysM[[k]]$Mean.aspeed,DaysM[[k]]$heading.rad, colour = DaysM[[k]]$Colour,xlabels= DaysM[[k]]$Timestamp)
  mtext("Heading", adj=0,side=3, line=0.5, cex=1, las=1)
  dev.off()
  png(paste0("C:/Users/mbradar/Documents/Merlin/OWEZ/plots/horizontal/DaysM_winds",datetime,".png"),height=4,width=8,unit="in",res=500)
  feather.plot2(DaysM[[k]]$Mean.wspeed,DaysM[[k]]$winddir.rad ,colour= DaysM[[k]]$Colour1,xlabels= DaysM[[k]]$Timestamp)
  mtext("Wind direction", adj=0,side=3, line=0.5, cex=1, las=1)
  dev.off()
  png(paste0("C:/Users/mbradar/Documents/Merlin/OWEZ/plots/horizontal/DaysM_trdir",datetime,".png"),height=4,width=8,unit="in",res=500)
  feather.plot2(DaysM[[k]]$Mean.aspeed,DaysM[[k]]$tr.dir.rad ,colour=DaysM[[k]]$Colour,xlabels= DaysM[[k]]$Timestamp)
  mtext("Track direction", adj=0,side=3, line=0.5, cex=1, las=1)
  dev.off()
}
  
  

