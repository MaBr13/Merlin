library(ggmap)
library(ggpubr)
library(cowplot)
library(ggpubr)
library(grid)

register_google(key = "AIzaSyC-bw1FEcHmDesL-zAPgW9RaWwolld2gRw")
world <- map_data("world")
Departures_Autumn_int <- read.csv("Departures_Autumn_int.csv",sep = ",")
Departures_Spring_int <- read.csv("Departures_Spring_int.csv",sep = ",")

blob <- subset(Departures_Spring_int,Long_start>=2.5 & Long_start<=4 & Lat_start>=52.3 & Lat_start<=52.8)#to subset the locations close to the radar
world <- get_map("world")
#plot them on a map to see if we are correct
ggplot(data=blob,aes(x=blob$Long_start,y=blob$Lat_start))+
  geom_polygon(data = world, aes(x=long, y = lat, group = group), colour="black",fill="gray84")+
  geom_point(cex=0.1,alpha=0.3)+
  coord_map(xlim = c(-11, 20), ylim = c(49, 62))
#make a histogram of hours from the sunset
windows(4,3)
hrs <- ggplot(blob,aes(x=step))+
  geom_bar(fill="cyan4",col="black")+
  scale_x_continuous(breaks=seq(2,13,1),labels = seq(1,6.5,0.5))+
  theme_bw()+
  theme(panel.grid=element_blank(),axis.text=element_text(size=12),
        axis.title = element_text(size=14),panel.border = element_blank(),
        axis.line = element_line())+
  xlab("Hours from the sunset") + ylab("Nr. of tracks")
#make a histogram of airspeeds in the blob
asp <- ggplot(blob,aes(x=airspeed))+
  geom_histogram(fill="steelblue",col="black",breaks=seq(5,30,1))+
  theme_bw()+
  scale_x_continuous(breaks=seq(5,30,3))+
  theme(panel.grid=element_blank(),axis.text=element_text(size=12),
        axis.title = element_text(size=14),panel.border = element_blank(),
        axis.line = element_line())+
  xlab("Airspeed (m/s)") + ylab("Nr. of tracks")

windows(8,3)
blob <- ggarrange(hrs,asp,labels = c("(a)","(b)"),font.label = list(size = 22),ncol=2,nrow = 1,
          widths=c(1,1,0.8,0.8),common.legend = TRUE,legend ="bottom",label.x=c(0.8,.8))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), legend.text = element_text(size = 4))

ggsave(filename=paste0("C:/Users/mbradar/surfdrive/Documents/Maja_PhD/first_paper_ver1/Paper figures/blob.png"),blob,dpi=500)

#make beautiful plots of separate days
set.seed(533)

fstday <- subset(Departures_Spring_int,date=="2008-04-22")
fstday <- fstday[sample(nrow(fstday),nrow(fstday)/2),]

#plots for the first day
windows(4,4)
#main plot with the map
pfst <- ggplot(data=fstday,aes(x=fstday$Long_start,y=fstday$Lat_start))+
  geom_polygon(data = world, aes(x=long, y = lat, group = group), colour="black",fill="gray84")+
  coord_map(xlim = c(-11, 20), ylim = c(49, 62))+
  geom_segment(aes(x=4.38,y=52.6,xend = fstday$Long_start, yend = fstday$Lat_start),colour = "grey90",alpha=0.3)+
  geom_point(cex=0.1,alpha=0.1,col="darkcyan",show.legend = TRUE)+
  geom_point(aes(x=4.38,y=52.6),col="red",cex=2,show.legend = TRUE)+
  borders("world")+
  theme_bw()+
  theme(panel.grid=element_blank(),legend.position = "bottom")+
  xlab("Longitude")+ ylab("Latitude")
dev.off()
#plot with proportion
windows(1.5,1.5)
pfsti1 <- ggplot(fstday,aes(x=date, fill=country))+
  geom_bar(position = "fill",colour="black",width = 0.3)+
  # coord_flip()+
  ylab("Proportion")+  
  scale_fill_brewer(palette="PuBuGn",name="")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_blank(),
        axis.title.x = element_blank(),panel.grid = element_blank(),axis.ticks.x = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"))+
  scale_x_discrete(expand = expand_scale(mult = c(0, .1)))
dev.off()
windows(1.5,1.5)
#plot with headings
pfsti2 <- ggplot(fstday,aes(x=heading,fill=country))+
  geom_histogram(colour="black",breaks = seq(0,360,10))+
  ylab("Number") + 
  xlab("Heading (degrees)")+
  scale_colour_manual(values = rep("black",7))+
  scale_fill_brewer(palette="PuBuGn",name="")+
  coord_polar(start=0)+
  theme_minimal()+
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_text(size=7,colour="black"),
        axis.title.x = element_blank(),axis.ticks = element_blank())+
  scale_x_continuous(breaks = seq(0,360,60))
dev.off()
#joint inset plot  
windows(4,4)
plot1 <-  ggdraw() +
  draw_plot(pfst) +
  draw_plot(pfsti1, x = 0.06, y = 0.49, width = .3, height = .4)+
  draw_plot(pfsti2, x = 0.13, y = 0.49, width = .6, height = .4)
dev.off()
  
#plots of the second day
sday <- subset(Departures_Spring_int,date=="2009-03-15")
sday <- sday[sample(nrow(sday),nrow(sday)/2),]
#main plot with the map
windows(4,4)
ps <- ggplot(data=sday,aes(x=sday$Long_start,y=sday$Lat_start))+
  geom_polygon(data = world, aes(x=long, y = lat, group = group), colour="black",fill="gray84")+
  coord_map(xlim = c(-11, 20), ylim = c(49, 62))+
  geom_segment(aes(x=4.38,y=52.6,xend = sday$Long_start, yend = sday$Lat_start),colour = "grey90",alpha=0.3)+
  geom_point(cex=0.1,alpha=0.1,col="darkcyan")+
  geom_point(aes(x=4.38,y=52.6),col="red",cex=2)+
  borders("world")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  xlab("Longitude")+ ylab("Latitude")
dev.off()
#plot with proportion
windows(1.5,1.5)
psi1 <- ggplot(sday,aes(x=date, fill=country))+
  geom_bar(position = "fill",colour="black",width = 0.3)+
  # coord_flip()+
  ylab("Proportion")+  
  scale_fill_brewer(palette="PuBuGn",name="")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_blank(),
        axis.title.x = element_blank(),panel.grid = element_blank(),axis.ticks.x = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"))+
  scale_x_discrete(expand = expand_scale(mult = c(0, .1)))
dev.off()
windows(1.5,1.5)
#plot with headings
psi2 <- ggplot(sday,aes(x=heading,fill=country))+
  geom_histogram(colour="black",breaks = seq(0,360,10))+
  ylab("Number") + 
  xlab("Heading (degrees)")+
  scale_colour_manual(values = rep("black",7))+
  scale_fill_brewer(palette="PuBuGn",name="")+
  coord_polar(start=0)+
  theme_minimal()+
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_text(size=7,colour="black"),
        axis.title.x = element_blank(),axis.ticks = element_blank())+
  scale_x_continuous(breaks = seq(0,360,60))
dev.off()
#joint inset plot  
windows(4,4)
plot2 <-  ggdraw() +
  draw_plot(ps) +
  draw_plot(psi1, x = 0.06, y = 0.49, width = .3, height = .4)+
  draw_plot(psi2, x = 0.13, y = 0.49, width = .6, height = .4)
  dev.off()  
#plots of the third day
tday <- subset(Departures_Autumn_int,date=="2007-10-13")
tday <- tday[sample(nrow(tday),nrow(tday)/2),]

#main plot with the map
windows(4,4)
pt <- ggplot(data=tday,aes(x=tday$Long_start,y=tday$Lat_start))+
  geom_polygon(data = world, aes(x=long, y = lat, group = group), colour="black",fill="gray84")+
  coord_map(xlim = c(-11, 20), ylim = c(49, 62))+
  geom_segment(aes(x=4.38,y=52.6,xend = tday$Long_start, yend = tday$Lat_start),colour = "grey90",alpha=0.3)+
  geom_point(cex=0.1,alpha=0.1,col="darkcyan")+
  geom_point(aes(x=4.38,y=52.6),col="red",cex=2)+
  borders("world")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  xlab("Longitude")+ ylab("Latitude")
dev.off()
#plot with proportion
windows(1.5,1.5)
pti1 <- ggplot(tday,aes(x=date, fill=country))+
  geom_bar(position = "fill",colour="black",width = 0.3)+
  # coord_flip()+
  ylab("Proportion")+  
  scale_fill_brewer(palette="PuBuGn",name="")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_blank(),
        axis.title.x = element_blank(),panel.grid = element_blank(),axis.ticks.x = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"))+
  scale_x_discrete(expand = expand_scale(mult = c(0, .1)))
dev.off()
windows(1.5,1.5)
#plot with headings
pti2 <- ggplot(tday,aes(x=heading,fill=country))+
  geom_histogram(colour="black",breaks = seq(0,360,10))+
  ylab("Number") + 
  xlab("Heading (degrees)")+
  scale_colour_manual(values = rep("black",7))+
  scale_fill_brewer(palette="PuBuGn",name="")+
  coord_polar(start=0)+
  theme_minimal()+
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_text(size=7,colour="black"),
        axis.title.x = element_blank(),axis.ticks = element_blank())+
  scale_x_continuous(breaks = seq(0,360,60))
dev.off()
#joint inset plot  
windows(4,4)
plot3 <-  ggdraw() +
  draw_plot(pt) +
  draw_plot(pti1, x = 0.06, y = 0.49, width = .3, height = .4)+
  draw_plot(pti2, x = 0.13, y = 0.49, width = .6, height = .4)
  dev.off()
#plots of the fourth day
fday <- subset(Departures_Autumn_int, date=="2008-10-30")
fday <- fday[sample(nrow(fday),nrow(fday)/2),]
#main plot with the map
windows(4,4)
pf <- ggplot(data=fday,aes(x=fday$Long_start,y=fday$Lat_start))+
  geom_polygon(data = world, aes(x=long, y = lat, group = group), colour="black",fill="gray84")+
  coord_map(xlim = c(-11, 20), ylim = c(49, 62))+
  geom_segment(aes(x=4.38,y=52.6,xend = fday$Long_start, yend = fday$Lat_start),colour = "grey90",alpha=0.3)+
  geom_point(cex=0.1,alpha=0.1,col="darkcyan")+
  geom_point(aes(x=4.38,y=52.6),col="red",cex=2)+
  borders("world")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  xlab("Longitude")+ ylab("Latitude")
dev.off()
#plot with proportion
windows(1.5,1.5)
pfi1 <- ggplot(fday,aes(x=date, fill=country))+
  geom_bar(position = "fill",colour="black",width = 0.3)+
  # coord_flip()+
  ylab("Proportion")+  
  scale_fill_brewer(palette="PuBuGn",name="")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_blank(),
        axis.title.x = element_blank(),panel.grid = element_blank(),axis.ticks.x = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"))+
  scale_x_discrete(expand = expand_scale(mult = c(0, .1)))
dev.off()
windows(1.5,1.5)
#plot with headings
pfi2 <- ggplot(fday,aes(x=heading,fill=country))+
  geom_histogram(colour="black",breaks = seq(0,360,10))+
  ylab("Number") + 
  xlab("Heading (degrees)")+
  scale_colour_manual(values = rep("black",7))+
  scale_fill_brewer(palette="PuBuGn",name="")+
  coord_polar(start=0)+
  theme_minimal()+
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_text(size=7,colour="black"),
        axis.title.x = element_blank(),axis.ticks = element_blank())+
  scale_x_continuous(breaks = seq(0,360,60))
dev.off()
#joint inset plot  
windows(4,4)
plot4 <-  ggdraw() +
  draw_plot(pf) +
  draw_plot(pfi1, x = 0.06, y = 0.49, width = .3, height = .4)+
  draw_plot(pfi2, x = 0.13, y = 0.49, width = .6, height = .4)
  dev.off()

fg <-  pfi1 <- ggplot(fday,aes(x=date, fill=country))+
    geom_bar(position = "fill",colour="black",width = 0.3)+
    # coord_flip()+
    ylab("Proportion")+  
    scale_fill_brewer(palette="PuBuGn",name="")+
    theme_minimal()+
    theme(legend.position = "bottom",
          axis.title.y = element_text(size=8,colour = "black"),
          axis.text.y=element_blank(), axis.text.x=element_blank(),
          axis.title.x = element_blank(),panel.grid = element_blank(),axis.ticks.x = element_blank(),
          plot.margin = unit(c(.5,1,.5,1), "cm"))+
    scale_x_discrete(expand = expand_scale(mult = c(0, .1))) 

leg <- get_legend(fg)

windows(8,8)
fplot <- ggarrange(plot1,plot2,plot4,plot3,labels = c("(a)","(b)","(c)","(d)"),font.label = list(size = 16),ncol=2,nrow = 2,
          widths=c(1,1,0.8,0.8),common.legend = TRUE,legend ="bottom",label.x=c(0.86,.86,.86,.86),label.y = c(.94,.94,.94,.94))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), legend.text = element_text(size = 4))+
  draw_plot(leg)
ggsave(filename=paste0("C:/Users/mbradar/surfdrive/Documents/Maja_PhD/first_paper_ver1/Paper figures/multiple_days.png"),fplot,dpi=500)
