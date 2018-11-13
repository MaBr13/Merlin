#####################################################################################
########Make the daily subsets and look into differences between different hours#####
#####################################################################################

for (k in 1:length(Allyears)){
  Allyears[[k]]$Aspeed <- factor(Allyears[[k]]$Aspeed, levels = rev(levels(Allyears[[k]]$Aspeed)))
  
}


####OCTOBER 2007

hm1a <- subset(Allyears[[1]], date=="2007-10-10", select = id:Wspeed)
hm2a <- subset(Allyears[[1]], date=="2007-10-13", select = id:Wspeed)
hm3a <- subset(Allyears[[1]], date=="2007-10-18", select = id:Wspeed)
hm4a <- subset(Allyears[[1]], date=="2007-10-19", select = id:Wspeed)
hm5a <- subset(Allyears[[1]], date=="2007-10-20", select = id:Wspeed)

##number and composition of migrants
ggplot(hm1a,aes(light))+
  geom_histogram(aes(colour=hm1a$Aspeed, fill=hm1a$Aspeed),bins=2,show.legend = T) +
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  scale_colour_manual(values  = c("black", "black", "black",  "black", "black",  "black","black"), name="Air speed (m/s)", drop=F)+
  ylim(0,10000)+
  ggtitle("10 Oct 2007") +
  ylab("Number of tracks") + 
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(size = 18))+
  scale_x_continuous(breaks = c(0,1),labels = paste0(c("Night", "Day")))

##wind direction and speed
ggplot(hm5a, aes(x=Rlight)) + 
  geom_histogram(aes(fill=hm5a$Wspeed, colour=hm5a$Wspeed), breaks=c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,235,240,255,270,285,300,315,330,345,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black","black"), name="Wind speed (m/s)", drop=F)+
  scale_fill_manual(values = c("cadetblue1","cornflowerblue","steelblue", "blue", "navy", "yellow","grey"), name="Wind speed (m/s)", drop=F)+
  ggtitle("20 Oct 2007") + 
  ylab("Number of tracks")+ ylim(0,5000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
##track direction
ggplot(hm5a, aes(x=trackheading)) + 
  geom_histogram(aes(fill=hm5a$Aspeed, colour=hm5a$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("20 Oct 2007") + 
  ylab("Number of tracks")+ ylim(0,8000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
##heading
ggplot(hm5a, aes(x=b.heading)) + 
  geom_histogram(aes(fill=hm5a$Aspeed, colour=hm5a$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("20 Oct 2009") + 
  ylab("Number of tracks")+ ylim(0,8000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

####MARCH 2008

hm2a <- subset(Allyears[[2]], date=="2008-03-28", select = id:Wspeed)

##number and composition of migrants
ggplot(hm2a,aes(light))+
  geom_histogram(aes(colour=hm2a$Aspeed, fill=hm2a$Aspeed), bins=2, show.legend = T) +
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  scale_colour_manual(values  = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  ylim(0,15000)+
  ggtitle("28 March 2008") +
  ylab("Number of tracks") + 
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(size = 18))+
  scale_x_continuous(breaks = c(0,1),labels = paste0(c("Night", "Day")))

##wind direction and speed
ggplot(hm2a, aes(x=new.winddir)) + 
  geom_histogram(aes(fill=hm2a$Wspeed, colour=hm2a$Wspeed), breaks=c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,235,240,255,270,285,300,315,330,345,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black","black"), name="Wind speed (m/s)", drop=F)+
  scale_fill_manual(values = c("cadetblue1","cornflowerblue","steelblue", "blue", "navy", "yellow","grey"), name="Wind speed (m/s)", drop=F)+
  ggtitle("28 March 2008") + 
  ylab("Number of tracks")+ ylim(0,7000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
##track direction
ggplot(hm2a, aes(x=trackheading)) + 
  geom_histogram(aes(fill=hm2a$Aspeed, colour=hm2a$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("28 March 2008") + 
  ylab("Number of tracks")+ ylim(0,10000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
##heading
ggplot(hm2a, aes(x=b.heading)) + 
  geom_histogram(aes(fill=hm2a$Aspeed, colour=hm2a$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("28 March 2008") + 
  ylab("Number of tracks")+ ylim(0,10000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
####OCTOBER 2008

hm2a <- subset(Allyears[[2]], date=="2008-10-29", select = id:Wspeed)
hm3a <- subset(Allyears[[2]], date=="2008-10-30", select = id:Wspeed)
hm4a <- subset(Allyears[[2]], date=="2008-10-31", select = id:Wspeed)


##number and composition of migrants
ggplot(hm4a,aes(light))+
  geom_histogram(aes(colour=hm4a$Aspeed, fill=hm4a$Aspeed), bins=2, show.legend = T) +
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  scale_colour_manual(values  = c("black", "black", "black",  "black", "black",  "black","black"), name="Air speed (m/s)", drop=F)+
  ylim(0,30000)+
  ggtitle("31 Oct 2008") +
  ylab("Number of tracks") + 
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(size = 18))+
  scale_x_continuous(breaks = c(0,1),labels = paste0(c("Night", "Day")))

##wind direction and speed
ggplot(hm4a, aes(x=new.winddir)) + 
  geom_histogram(aes(fill=hm4a$Wspeed, colour=hm4a$Wspeed), breaks=c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,235,240,255,270,285,300,315,330,345,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black","black"), name="Wind speed (m/s)", drop=F)+
  scale_fill_manual(values = c("cadetblue1","cornflowerblue","steelblue", "blue", "navy", "yellow","grey"), name="Wind speed (m/s)", drop=F)+
  ggtitle("31 Oct 2008") + 
  ylab("Number of tracks")+ ylim(0,20000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
##track direction
ggplot(hm3a, aes(x=trackheading)) + 
  geom_histogram(aes(fill=hm3a$Aspeed, colour=hm3a$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("31 Oct 2008") + 
  ylab("Number of tracks")+ ylim(0,22000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
##heading
ggplot(hm4a, aes(x=b.heading)) + 
  geom_histogram(aes(fill=hm4a$Aspeed, colour=hm4a$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("31 Oct 2008") + 
  ylab("Number of tracks")+ ylim(0,15000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

####MARCH 2009

hm1a <- subset(Allyears[[3]], date=="2009-03-13", select = id:Wspeed)
hm2a <- subset(Allyears[[3]], date=="2009-03-16", select = id:Wspeed)

##number and composition of migrants
ggplot(hm2a,aes(light))+
  geom_bar(aes(colour=hm2a$Aspeed, fill=hm2a$Aspeed), bins=2, show.legend = T) +
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ylim(0,30000)+
  ggtitle("16 March 2009") +
  ylab("Number of tracks") + 
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(size = 18))+
  scale_x_continuous(breaks = c(0,1),labels = paste0(c("Night", "Day")))

##wind direction and speed
ggplot(hm1a, aes(x=new.winddir)) + 
  geom_histogram(aes(fill=hm1a$Wspeed, colour=hm1a$Wspeed), breaks=c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,235,240,255,270,285,300,315,330,345,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black","black"), name="Wind speed (m/s)", drop=F)+
  scale_fill_manual(values = c("cadetblue1","cornflowerblue","steelblue", "blue", "navy", "yellow","grey"), name="Wind speed (m/s)", drop=F)+
  ggtitle("13 March 2009") + 
  ylab("Number of tracks")+ ylim(0,10000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
##track direction
ggplot(hm2a, aes(x=trackheading)) + 
  geom_histogram(aes(fill=hm2a$Aspeed, colour=hm2a$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("13 March 2009") + 
  ylab("Number of tracks")+ ylim(0,12000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
##heading
ggplot(hm2a, aes(x=b.heading)) + 
  geom_histogram(aes(fill=hm2a$Aspeed, colour=hm2a$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("13 March 2009") + 
  ylab("Number of tracks")+ ylim(0,10000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))


####MARCH 2010

hm1a <- subset(Allyears[[4]], date=="2010-03-21", select = id:Wspeed)
hm2a <- subset(Allyears[[4]], date=="2010-03-16", select = id:Wspeed)
hm3a <- subset(Allyears[[4]], date=="2010-03-22", select = id:Wspeed)

##number and composition of migrants
ggplot(hm1a,aes(light))+
  geom_bar(aes(colour=hm1a$Aspeed, fill=hm1a$Aspeed), show.legend = T) +
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ylim(0,15000)+
  ggtitle("21 March 2010") +
  ylab("Number of tracks") + 
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(size = 18))+
  scale_x_continuous(breaks = c(0,1),labels = paste0(c("Night", "Day")))

##wind direction and speed
ggplot(hm3a, aes(x=new.winddir)) + 
  geom_histogram(aes(fill=hm3a$Wspeed, colour=hm3a$Wspeed), breaks=c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,235,240,255,270,285,300,315,330,345,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black","black"), name="Wind speed (m/s)", drop=F)+
  scale_fill_manual(values = c("cadetblue1","cornflowerblue","steelblue", "blue", "navy", "yellow","grey"), name="Wind speed (m/s)", drop=F)+
  ggtitle("16 March 2010") + 
  ylab("Number of tracks")+ ylim(0,9000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
##track direction
ggplot(hm1a, aes(x=trackheading)) + 
  geom_histogram(aes(fill=hm1a$Aspeed, colour=hm1a$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("21 March 2010") + 
  ylab("Number of tracks")+ ylim(0,8000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
##heading
ggplot(hm1a, aes(x=b.heading)) + 
  geom_histogram(aes(fill=hm1a$Aspeed, colour=hm1a$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("21 March 2010") + 
  ylab("Number of tracks")+ ylim(0,6000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
