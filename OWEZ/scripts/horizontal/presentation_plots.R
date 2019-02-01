############################################################
##############PRESENTATION PLOTS OF WIND ROSES##############
############################################################


####PLOT A WIND ROSE OF WIND DIRECTION AND SPEED IN SPRING
ggplot(SpringAll[[3]], aes(x=new.winddir)) + 
  geom_histogram(aes(fill=SpringAll[[3]]$Wspeed, colour=SpringAll[[3]]$Wspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black","black"), name="Wind speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey", "yellow", "navy", "blue","steelblue","cornflowerblue","cadetblue1"), name="Wind speed (m/s)", drop=F)+
  ggtitle(2009) +
  ylab("Number of tracks")+ylim(0,105000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
####PLOT A WIND ROSE OF WIND DIRECTION AND SPEED IN AUTUMN
ggplot(AutumnAll[[2]], aes(x=new.winddir)) + 
  geom_histogram(aes(fill=AutumnAll[[2]]$Wspeed, colour=AutumnAll[[2]]$Wspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black","black"), name="Wind speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey", "yellow", "navy", "blue","steelblue","cornflowerblue","cadetblue1"), name="Wind speed (m/s)", drop=F)+
  ggtitle(2008) + 
  ylab("Number of tracks")+ ylim(0,132000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
####PLOT A WIND ROSE OF TRACK DIRECTIONS AND AIR SPEED IN SPRING
 ggplot(SpringAll[[3]], aes(x=trackheading)) + 
  geom_histogram(aes(fill=SpringAll[[3]]$Aspeed, colour=SpringAll[[3]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle(2009) + 
  ylab("Number of tracks")+ ylim(0,105000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
 ####PLOT A WIND ROSE OF TRACK DIRECTIONS AND AIR SPEED IN AUTUMN
ggplot(AutumnAll[[2]], aes(x=trackheading)) + 
  geom_histogram(aes(fill=AutumnAll[[2]]$Aspeed, colour=AutumnAll[[2]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle(2008) + 
  ylab("Number of tracks")+ ylim(0,132000)+
  #theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
   #     legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
    #    axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
     #   axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
#### PLOT A WIND ROSE OF BIRDS HEADINGS AND AIRSPEEDS IN SPRING
ggplot(SpringAll[[3]], aes(x=b.heading)) + 
  geom_histogram(aes(fill=SpringAll[[3]]$Aspeed, colour=SpringAll[[3]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle(2009) + 
  ylab("Number of tracks")+ ylim(0,105000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

#### PLOT A WIND ROSE OF BIRDS HEADINGS AND AIRSPEEDS IN AUTUMN
ggplot(AutumnAll[[2]], aes(x=b.heading)) + 
  geom_histogram(aes(fill=AutumnAll[[2]]$Aspeed, colour=AutumnAll[[2]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle(2008) + 
  ylab("Number of tracks")+ ylim(0,140000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
