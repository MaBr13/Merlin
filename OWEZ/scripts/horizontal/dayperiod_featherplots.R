######################################################################
############DAILY SEASONAL FEATHER PLOTS########
######################################################################


####CALCULATE NEW WIND DIRECTION (DIRECTION THE WIND BLOWS TO)

#dawn.spring <- dawn.spring[-c(3,4)]
for (k in 1:length(dawn.spring)){
  
  dawn.spring[[k]]$winddir.rad <- dawn.spring[[k]]$Mean.wdir*(pi/180)
  dawn.spring[[k]]$heading.rad <- dawn.spring[[k]]$Mean.head*(pi/180)
  dawn.spring[[k]]$tr.dir.rad <- dawn.spring[[k]]$Mean.tr.dir*(pi/180)
}

for (k in 1:length(dawn.spring)){
  
  dawn.spring[[k]]$winddir.rad <- as.numeric(dawn.spring[[k]]$winddir.rad )
  dawn.spring[[k]]$heading.rad <- as.numeric(dawn.spring[[k]]$heading.rad)
  dawn.spring[[k]]$tr.dir.rad <- as.numeric(dawn.spring[[k]]$tr.dir.rad )
}


library(dplyr)

for(k in 1:length(dawn.spring)){
  dawn.spring[[k]] <- filter(dawn.spring[[k]],  !is.na(heading.rad))
}

for(k in 1:length(dawn.spring)){
  dawn.spring[[k]] <- filter(dawn.spring[[k]],  !is.na(tr.dir.rad))
}


library(plotrix)

#r - the length of vector (for example wind strength)
#theta - the angle (wind direction) in radians
#line-position a title
#las-orientation of the title
#adj-position of a text in the title (left, right)


###add colouring based on airspeed and windspeed

for (k in 1:length(dawn.spring)){
  dawn.spring[[k]]$Colour[dawn.spring[[k]]$Mean.aspeed>=3 & dawn.spring[[k]]$Mean.aspeed<=12]="maroon"
  dawn.spring[[k]]$Colour[dawn.spring[[k]]$Mean.aspeed>12 & dawn.spring[[k]]$Mean.aspeed<=15]="coral3"
  dawn.spring[[k]]$Colour[dawn.spring[[k]]$Mean.aspeed>15 & dawn.spring[[k]]$Mean.aspeed<=18]="firebrick1"
  dawn.spring[[k]]$Colour[dawn.spring[[k]]$Mean.aspeed>18 & dawn.spring[[k]]$Mean.aspeed<=27]="forestgreen"
  dawn.spring[[k]]$Colour[dawn.spring[[k]]$Mean.aspeed>27]="grey0"
  # dawn.spring[[k]]$Colour[dawn.spring[[k]]$Mean.aspeed=NA]="grey"
  dawn.spring[[k]]$Colour1[dawn.spring[[k]]$Mean.wspeed>=0 & dawn.spring[[k]]$Mean.wspeed<=5]="cadetblue1"
  dawn.spring[[k]]$Colour1[dawn.spring[[k]]$Mean.wspeed>5 & dawn.spring[[k]]$Mean.wspeed<=10]="cornflowerblue"
  dawn.spring[[k]]$Colour1[dawn.spring[[k]]$Mean.wspeed>10 & dawn.spring[[k]]$Mean.wspeed<=15]="steelblue"
  dawn.spring[[k]]$Colour1[dawn.spring[[k]]$Mean.wspeed>15 & dawn.spring[[k]]$Mean.wspeed<=20]="deepskyblue4"
  dawn.spring[[k]]$Colour1[dawn.spring[[k]]$Mean.wspeed>20 & dawn.spring[[k]]$Mean.wspeed<=25]="dodgerblue3"
  dawn.spring[[k]]$Colour1[dawn.spring[[k]]$Mean.wspeed>25 & dawn.spring[[k]]$Mean.wspeed<=30]="mediumblue"
  dawn.spring[[k]]$Colour1[dawn.spring[[k]]$Mean.wspeed>30 & dawn.spring[[k]]$Mean.wspeed<=45]="blue"
  dawn.spring[[k]]$Colour1[dawn.spring[[k]]$Mean.wspeed>35 & dawn.spring[[k]]$Mean.wspeed<=40]="navy"
  dawn.spring[[k]]$Colour1[dawn.spring[[k]]$Mean.wspeed>40]="yellow"
}


###feather.plot2 function is in another script called changed_featherf

for (k in 1:length(dawn.spring)){
  s <- as.data.frame(dawn.spring[[k]])
  datetime <- paste0(format(s[1,13], format="%Y-%m-%d"))
  png(paste0("C:/Users/mbradar/Documents/Merlin/OWEZ/plots/horizontal/daily.feather/dawn.spring/dawn.spring_heading",datetime,".png"),height=4,width=8,unit="in",res=500)
  feather.plot2(dawn.spring[[k]]$Mean.aspeed,dawn.spring[[k]]$heading.rad, colour = dawn.spring[[k]]$Colour,fp.type="m",xlabels= dawn.spring[[k]]$n.date)
  mtext("Heading", adj=0,side=3, line=0.5, cex=1, las=1)
  dev.off()
  png(paste0("C:/Users/mbradar/Documents/Merlin/OWEZ/plots/horizontal/daily.feather/dawn.spring/dawn.spring_wind",datetime,".png"),height=4,width=8,unit="in",res=500)
  feather.plot2(dawn.spring[[k]]$Mean.wspeed,dawn.spring[[k]]$winddir.rad ,colour= dawn.spring[[k]]$Colour1,fp.type="m",xlabels= dawn.spring[[k]]$n.date)
  mtext("Wind direction", adj=0,side=3, line=0.5, cex=1, las=1)
  dev.off()
  png(paste0("C:/Users/mbradar/Documents/Merlin/OWEZ/plots/horizontal/daily.feather/dawn.spring/dawn.spring_tr.dir",datetime,".png"),height=4,width=8,unit="in",res=500)
  feather.plot2(dawn.spring[[k]]$Mean.aspeed,dawn.spring[[k]]$tr.dir.rad ,colour=dawn.spring[[k]]$Colour,fp.type="m",xlabels= dawn.spring[[k]]$n.date)
  mtext("Track direction", adj=0,side=3, line=0.5, cex=1, las=1)
  dev.off()
}


