#categories according to air speed
for(k in 1:length(Birds)){
  
  Birds[[k]]$Aspeed<- cut(Birds[[k]]$airspeedms,breaks=c(0,12,15,18,27,45), 
                               labels = c("0-12","12-15","15-18", "18-30"))
  
}
#categories wind speed
for(k in 1:length(Birds)){
  
  Birds[[k]]$Wspeed<- cut(Birds[[k]]$windspeedms,breaks=c(0,10,15,20,30,40,50), 
                               labels = c("0-10","10-15","15-20","20-30", "30-40", ">40"))
  
}

for (k in 1:length(Birds)){
  Birds[[k]]$Aspeed <- factor(Birds[[k]]$Aspeed, levels = rev(levels(Birds[[k]]$Aspeed)))
  
}

require(ggplot2)
##wind direction and speed
ggplot(Birds[[4]], aes(x=new.winddir)) + 
  geom_histogram(aes(fill=Birds[[4]]$Wspeed, colour=Birds[[4]]$Wspeed), breaks=c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,235,240,255,270,285,300,315,330,345,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black","black","black","black","black"), name="Wind speed (m/s)", drop=F)+
  scale_fill_manual(values = c("cadetblue1","cornflowerblue","steelblue", "deepskyblue4","dodgerblue3", "mediumblue", "blue", "navy", "yellow","grey"), name="Wind speed (m/s)", drop=F)+
  ggtitle("27 March 2008") + 
  ylab("Number of tracks")+ ylim(0,7500)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
##track direction
ggplot(Birds[[4]], aes(x=trackheading)) + 
  geom_histogram(aes(fill=Birds[[4]]$Aspeed, colour=Birds[[4]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("darkolivegreen","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("27 March 2008") + 
  ylab("Number of tracks")+ ylim(0,10000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
##heading
ggplot(Birds[[4]], aes(x=b.heading)) + 
  geom_histogram(aes(fill=Birds[[4]]$Aspeed, colour=Birds[[4]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("darkolivegreen","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("27 March 2008") + 
  ylab("Number of tracks")+ ylim(0,10000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
