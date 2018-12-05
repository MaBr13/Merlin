######################################################################
############DAILY SEASONAL FEATHER PLOTS########
######################################################################

####CALCULATE NEW WIND DIRECTION (DIRECTION THE WIND BLOWS TO)
for (k in 1:length(Autumn)){
  
  
  Autumn[[k]]$winddir.rad <- Autumn[[k]]$Mean.wdir*(pi/180)
  Autumn[[k]]$heading.rad <- Autumn[[k]]$Mean.head*(pi/180)
  Autumn[[k]]$tr.dir.rad <- Autumn[[k]]$Mean.tr.dir*(pi/180)
}


library(dplyr)

for(k in 1:length(Autumn)){
  Autumn[[k]] <- filter(Autumn[[k]],  !is.na(heading.rad))
}


library(plotrix)

#r - the length of vector (for example wind strength)
#theta - the angle (wind direction) in radians
#line-position a title
#las-orientation of the title
#adj-position of a text in the title (left, right)


###add colouring based on airspeed and windspeed

for (k in 1:length(Autumn)){
  Autumn[[k]]$Colour[Autumn[[k]]$Mean.aspeed>=3 & Autumn[[k]]$Mean.aspeed<=12]="maroon"
  Autumn[[k]]$Colour[Autumn[[k]]$Mean.aspeed>12 & Autumn[[k]]$Mean.aspeed<=15]="coral3"
  Autumn[[k]]$Colour[Autumn[[k]]$Mean.aspeed>15 & Autumn[[k]]$Mean.aspeed<=18]="firebrick1"
  Autumn[[k]]$Colour[Autumn[[k]]$Mean.aspeed>18 & Autumn[[k]]$Mean.aspeed<=27]="forestgreen"
  Autumn[[k]]$Colour[Autumn[[k]]$Mean.aspeed>27]="grey0"
  # Autumn[[k]]$Colour[Autumn[[k]]$Mean.aspeed=NA]="grey"
  Autumn[[k]]$Colour1[Autumn[[k]]$Mean.wspeed>=0 & Autumn[[k]]$Mean.wspeed<=5]="cadetblue1"
  Autumn[[k]]$Colour1[Autumn[[k]]$Mean.wspeed>5 & Autumn[[k]]$Mean.wspeed<=10]="cornflowerblue"
  Autumn[[k]]$Colour1[Autumn[[k]]$Mean.wspeed>10 & Autumn[[k]]$Mean.wspeed<=15]="steelblue"
  Autumn[[k]]$Colour1[Autumn[[k]]$Mean.wspeed>15 & Autumn[[k]]$Mean.wspeed<=20]="deepskyblue4"
  Autumn[[k]]$Colour1[Autumn[[k]]$Mean.wspeed>20 & Autumn[[k]]$Mean.wspeed<=25]="dodgerblue3"
  Autumn[[k]]$Colour1[Autumn[[k]]$Mean.wspeed>25 & Autumn[[k]]$Mean.wspeed<=30]="mediumblue"
  Autumn[[k]]$Colour1[Autumn[[k]]$Mean.wspeed>30 & Autumn[[k]]$Mean.wspeed<=45]="blue"
  Autumn[[k]]$Colour1[Autumn[[k]]$Mean.wspeed>35 & Autumn[[k]]$Mean.wspeed<=40]="navy"
  Autumn[[k]]$Colour1[Autumn[[k]]$Mean.wspeed>40]="yellow"
}


###feather.plot2 function is in another script called changed_featherf

for (k in 1:length(Autumn)){
  s <- as.data.frame(Autumn[[k]])
  datetime <- paste0(format(s[1,6], format="%Y-%m-%d"))
  png(paste0("C:/Users/Maja/Documents/plots_OWEZ/feather/Autumn_heading",datetime,".png"),height=4,width=8,unit="in",res=500)
  feather.plot2(Autumn[[k]]$Mean.aspeed,Autumn[[k]]$heading.rad, colour = Autumn[[k]]$Colour,fp.type="m",xlabels= Autumn[[k]]$Timestamp)
  mtext("Heading", adj=0,side=3, line=0.5, cex=1, las=1)
  dev.off()
  png(paste0("C:/Users/Maja/Documents/plots_OWEZ/feather/Autumn_wind",datetime,".png"),height=4,width=8,unit="in",res=500)
  feather.plot2(Autumn[[k]]$Mean.wspeed,Autumn[[k]]$winddir.rad ,colour= Autumn[[k]]$Colour1,fp.type="m",xlabels= Autumn[[k]]$Timestamp)
  mtext("Wind direction", adj=0,side=3, line=0.5, cex=1, las=1)
  dev.off()
  png(paste0("C:/Users/Maja/Documents/plots_OWEZ/feather/Autumn_tr.dir",datetime,".png"),height=4,width=8,unit="in",res=500)
  feather.plot2(Autumn[[k]]$Mean.aspeed,Autumn[[k]]$tr.dir.rad ,colour=Autumn[[k]]$Colour,fp.type="m",xlabels= Autumn[[k]]$Timestamp)
  mtext("Track direction", adj=0,side=3, line=0.5, cex=1, las=1)
  dev.off()
}
