##############################################################
#####GRIDS AND INDIVIDUAL PLOTS OF MIGRANTS###################
##############################################################
require(ggplot2)

#YEARLY NUMBERS OF MIGRANTS


#####change the levels on which categories occur on plots (air speed and wind speed)

#Air speed

for (k in 1:length(means)){
  means[[k]]$Aspeed <- factor(means[[k]]$Aspeed, levels = rev(levels(means[[k]]$Aspeed)))
  
}

for (k in 1:length(Spring)){
  Spring[[k]]$Aspeed <- factor(Spring[[k]]$Aspeed, levels = rev(levels(Spring[[k]]$Aspeed)))
  
}

for (k in 1:length(Autumn)){
 Autumn[[k]]$Aspeed <- factor(Autumn[[k]]$Aspeed, levels = rev(levels(Autumn[[k]]$Aspeed)))
  
}

for (k in 1:length(SpringAll)){
  SpringAll[[k]]$Aspeed <- factor(SpringAll[[k]]$Aspeed, levels = rev(levels(SpringAll[[k]]$Aspeed)))
  
}

for (k in 1:length(AutumnAll)){
  AutumnAll[[k]]$Aspeed <- factor(AutumnAll[[k]]$Aspeed, levels = rev(levels(AutumnAll[[k]]$Aspeed)))
  
}

#Wind speed

for (k in 1:length(SpringAll)){
  SpringAll[[k]]$Wspeed <- factor(SpringAll[[k]]$Wspeed, levels = rev(levels(SpringAll[[k]]$Wspeed)))
  
}

for (k in 1:length(AutumnAll)){
  AutumnAll[[k]]$Wspeed <- factor(AutumnAll[[k]]$Wspeed, levels = rev(levels(AutumnAll[[k]]$Wspeed)))
  
}


####PLOT A GRID OF NUMBERS OF MIGRANTS AND MIGRANT COMPOSITION IN ALL YEARS

  p1 <-  ggplot(means[[1]],aes(Rlight, Nr.tracks))+
    geom_bar(stat="identity",aes(colour=means[[1]]$Aspeed, fill=means[[1]]$Aspeed), show.legend = T) +
    scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
    scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
    ylim(0,1250000)+
    ggtitle("2007") +
    ylab("Number of tracks") + 
    theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
          legend.title=element_text(size=16, face="bold"),
          axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
          axis.title.x = element_blank(), plot.title = element_text(size = 18))+
    scale_x_discrete(breaks = c(0,1),labels = paste0(c("Night", "Day")))
  
   p2 <- ggplot(means[[2]])+aes(Rlight, Nr.tracks)+
     geom_bar(stat="identity",aes(colour=means[[2]]$Aspeed, fill=means[[2]]$Aspeed), show.legend = T) +
     scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
     scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
     ylim(0,1250000)+
     ggtitle("2008") +
     theme(axis.text.y = element_blank(), 
           axis.ticks.y = element_blank(), 
           axis.title.y = element_blank(),
           axis.title.x = element_blank(),
           axis.text.x=element_text(size=14),
           plot.title = element_text(size = 16)) +
     scale_x_discrete(breaks = c(0,1),labels = paste0(c("Night", "Day")))
   
  p3 <-  ggplot(means[[3]])+aes(Rlight, Nr.tracks)+
     geom_bar(stat="identity",aes(colour=means[[3]]$Aspeed, fill=means[[3]]$Aspeed), show.legend = T) +
     scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
     scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
     ylim(0,1250000)+
     ggtitle("2009") +
     theme(axis.text.y = element_blank(), 
           axis.ticks.y = element_blank(), 
           axis.title.y = element_blank(),
           axis.title.x = element_blank(),
           axis.text.x=element_text(size=14),
           plot.title = element_text(size = 16)) +
     scale_x_discrete(breaks = c(0,1),labels = paste0(c("Night", "Day")))
   
   p4 <- ggplot(means[[4]])+aes(Rlight, Nr.tracks)+
     geom_bar(stat="identity",aes(colour=means[[4]]$Aspeed, fill=means[[4]]$Aspeed), show.legend = T) +
     scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
     scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
     ylim(0,1250000)+
     ggtitle("2010") +
     theme(axis.text.y = element_blank(), 
           axis.ticks.y = element_blank(), 
           axis.title.y = element_blank(),
           axis.title.x = element_blank(),
           axis.text.x=element_text(size=14),
           plot.title = element_text(size = 16)) +
     scale_x_discrete(breaks = c(0,1),labels = paste0(c("Night", "Day")))
   
  
library(ggpubr)
library(grid)
migrants <- ggarrange(p1, p2,p3, p4,  ncol=4, nrow=1, common.legend = TRUE, legend="bottom",widths=c(1.42,1,1,1)) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
annotate_figure(migrants,top = textGrob("Diurnal and nocturnal migrants",gp=gpar(fontsize=24,fontface="bold")))


####PLOT A GRID OF NUMBERS OF MIGRANTS AND MIGRANT COMPOSITION IN SPRING IN ALL YEARS

pS1 <-  ggplot(Spring[[1]],aes(Rlight, Nr.tracks))+
  geom_bar(stat="identity",aes(colour=Spring[[1]]$Aspeed, fill=Spring[[1]]$Aspeed), show.legend = T) +
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ylim(0,1250000)+
  ggtitle("2007") +
  ylab("Number of tracks") + 
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(size = 18))+
  scale_x_discrete(breaks = c(0,1),labels = paste0(c("Night", "Day")))

pS2 <- ggplot(Spring[[2]])+aes(Rlight, Nr.tracks)+
  geom_bar(stat="identity",aes(colour=Spring[[2]]$Aspeed, fill=Spring[[2]]$Aspeed), show.legend = T) +
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ylim(0,1250000)+
  ggtitle("2008") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=14),
        plot.title = element_text(size = 16)) +
  scale_x_discrete(breaks = c(0,1),labels = paste0(c("Night", "Day")))

pS3 <-  ggplot(Spring[[3]])+aes(Rlight, Nr.tracks)+
  geom_bar(stat="identity",aes(colour=Spring[[3]]$Aspeed, fill=Spring[[3]]$Aspeed), show.legend = T) +
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ylim(0,1250000)+
  ggtitle("2009") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=14),
        plot.title = element_text(size = 16)) +
  scale_x_discrete(breaks = c(0,1),labels = paste0(c("Night", "Day")))

pS4 <- ggplot(Spring[[4]])+aes(Rlight, Nr.tracks)+
  geom_bar(stat="identity",aes(colour=Spring[[4]]$Aspeed, fill=Spring[[4]]$Aspeed), show.legend = T) +
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ylim(0,1250000)+
  ggtitle("2010") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=14),
        plot.title = element_text(size = 16)) +
  scale_x_discrete(breaks = c(0,1),labels = paste0(c("Night", "Day")))


library(ggpubr)

migrantsS <- ggarrange(pS1, pS2,pS3, pS4,  ncol=4, nrow=1, common.legend = TRUE, legend="bottom",widths=c(1.42,1,1,1)) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
annotate_figure(migrantsS,top = textGrob("Diurnal and nocturnal migrants Spring",gp=gpar(fontsize=24,fontface="bold")))

####PLOT A GRID OF NUMBERS OF MIGRANTS AND MIGRANT COMPOSITION IN AUTUMN IN ALL YEARS

pA1 <-  ggplot(Autumn[[1]],aes(Rlight, Nr.tracks))+
  geom_bar(stat="identity",aes(colour=Autumn[[1]]$Aspeed, fill=Autumn[[1]]$Aspeed), show.legend = T) +
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ylim(0,1250000)+
  ggtitle("2007") +
  ylab("Number of tracks") + 
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(size = 18))+
  scale_x_discrete(breaks = c(0,1),labels = paste0(c("Night", "Day")))

pA2 <- ggplot(Autumn[[2]])+aes(Rlight, Nr.tracks)+
  geom_bar(stat="identity",aes(colour=Autumn[[2]]$Aspeed, fill=Autumn[[2]]$Aspeed), show.legend = T) +
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  scale_colour_manual(values  = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ylim(0,1250000)+
  ggtitle("2008") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=14),
        plot.title = element_text(size = 16)) +
  scale_x_discrete(breaks = c(0,1),labels = paste0(c("Night", "Day")))




library(ggpubr)
migrantsA <- ggarrange(pA1, pA2,  ncol=2, nrow=1, common.legend = TRUE, legend="bottom",widths=c(1.32,1)) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
annotate_figure(migrantsA,top = textGrob("Diurnal and nocturnal migrants Autumn",gp=gpar(fontsize=24,fontface="bold")))
   
####PLOT A WIND ROSES OF WIND DIRECTION AND SPEED IN SPRING

      spwS1 <- ggplot(SpringAll[[1]], aes(x=winddir)) + 
      geom_histogram(aes(fill=SpringAll[[1]]$Wspeed, colour=SpringAll[[1]]$Wspeed), breaks=c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,235,240,255,270,285,300,315,330,345,360)) +
      scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black"), name="Wind speed (m/s)", drop=F)+
      scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Wind speed (m/s)", drop=F)+
      ggtitle(2007) + 
      ylab("Number of tracks")+ ylim(0,75000)+
      theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
      coord_polar(start = 0) +
      scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
    
     spwS2 <- ggplot(SpringAll[[2]], aes(x=winddir)) + 
      geom_histogram(aes(fill=SpringAll[[2]]$Wspeed, colour=SpringAll[[2]]$Wspeed), breaks=c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,235,240,255,270,285,300,315,330,345,360)) +
      scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black"), name="Wind speed (m/s)", drop=F)+
      scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Wind speed (m/s)", drop=F)+
      ggtitle(2008) +
      ylab("Number of tracks")+ ylim(0,75000)+
      theme(axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            plot.title = element_text(size = 16)) +
      coord_polar(start = 0) +
      scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
    
    #spwS3 <- 
      ggplot(SpringAll[[3]], aes(x=new.winddir)) + 
      geom_histogram(aes(fill=SpringAll[[3]]$Wspeed, colour=SpringAll[[3]]$Wspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
      scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black","black"), name="Wind speed (m/s)", drop=F)+
      scale_fill_manual(values = c("grey", "yellow", "navy", "blue","steelblue","cornflowerblue","cadetblue1"), name="Wind speed (m/s)", drop=F)+
      ggtitle("Wind rose") +
      ylab("Number of tracks")+ylim(0,105000)+
      theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
            legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
            axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
            axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
      coord_polar(start = 0) +
      scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
    
    spwS4 <- ggplot(SpringAll[[4]], aes(x=winddir)) + 
      geom_histogram(aes(fill=SpringAll[[4]]$Wspeed, colour=SpringAll[[4]]$Wspeed), breaks=c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,235,240,255,270,285,300,315,330,345,360)) +
      scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black"), name="Wind speed (m/s)", drop=F)+
      scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Wind speed (m/s)", drop=F)+
      ggtitle(2010) +
      ylab("Number of tracks")+ylim(0,75000)+
      theme(axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            plot.title = element_text(size = 16)) +
      coord_polar(start = 0) +
      scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))
    
    library(ggpubr)
    library(gridExtra)
    library(grid)
    swindS <- ggarrange(spwS1, spwS2,spwS3, spwS4,  ncol=4, nrow=1, common.legend = TRUE, legend="bottom",widths=c(1.31,1,1,1)) +
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
    annotate_figure(swindS,top = textGrob("Wind direction and speed Spring",gp=gpar(fontsize=20,fontface="bold")))
    
####PLOT A WIND ROSE OF WIND DIRECTION AND SPEED IN AUTUMN

spwA1 <- ggplot(AutumnAll[[1]], aes(x=winddir)) + 
  geom_histogram(aes(fill=AutumnAll[[1]]$Wspeed, colour=AutumnAll[[1]]$Wspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black"), name="Wind speed (m/s)", drop=F)+
  scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Wind speed (m/s)", drop=F)+
  ggtitle(2007) + 
  ylab("Number of tracks")+ ylim(0,105000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

#spwA2 <- 
ggplot(AutumnAll[[2]], aes(x=new.winddir)) + 
  geom_histogram(aes(fill=AutumnAll[[2]]$Wspeed, colour=AutumnAll[[2]]$Wspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black","black"), name="Wind speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey", "yellow", "navy", "blue","steelblue","cornflowerblue","cadetblue1"), name="Wind speed (m/s)", drop=F)+
  ggtitle("Wind rose") + 
  ylab("Number of tracks")+ ylim(0,132000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

library(ggpubr)
library(gridExtra)
library(grid)
swindA <- ggarrange(spwA1, spwA2,  ncol=2, nrow=1, common.legend = TRUE, legend="bottom",widths=c(1.32,1)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
annotate_figure(swindA,top = textGrob("Wind direction and speed Autumn",gp=gpar(fontsize=20,fontface="bold")))


####PLOT A WIND ROSE OF TRACK DIRECTIONS AND AIR SPEED IN SPRING


sphS1 <- ggplot(SpringAll[[1]], aes(x=trackheading)) + 
  geom_histogram(aes(fill=SpringAll[[1]]$Aspeed, colour=SpringAll[[1]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (m/s)", drop=F)+
  ggtitle(2007) + 
  ylab("Number of tracks")+ ylim(0,150000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

sphS2 <- ggplot(SpringAll[[2]], aes(x=trackheading)) + 
  geom_histogram(aes(fill=SpringAll[[2]]$Aspeed, colour=SpringAll[[2]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (m/s)", drop=F)+
  ggtitle(2008) + 
  ylab("Number of tracks")+ ylim(0,150000)+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16)) +
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

#sphS3 <- 

ggplot(SpringAll[[3]], aes(x=trackheading)) + 
  geom_histogram(aes(fill=SpringAll[[3]]$Aspeed, colour=SpringAll[[3]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("Flight direction") + 
  ylab("Number of tracks")+ ylim(0,105000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

sphS4 <- ggplot(SpringAll[[4]], aes(x=trackheading)) + 
  geom_histogram(aes(fill=SpringAll[[4]]$Aspeed, colour=SpringAll[[4]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (m/s)", drop=F)+
  ggtitle(2010) + 
  ylab("Number of tracks")+ ylim(0,150000)+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16)) +
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

library(ggpubr)
library(gridExtra)
library(grid)
sheadS <- ggarrange(sphS1, sphS2,sphS3, sphS4,  ncol=4, nrow=1, common.legend = TRUE, legend="bottom",widths=c(1.31,1,1,1)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
annotate_figure(sheadS,top = textGrob("Headings and air speeds Spring",gp=gpar(fontsize=20,fontface="bold")))

####PLOT A WIND ROSE OF TRACK DIRECTIONS AND AIR SPEED IN AUTUMN



sphA1 <- ggplot(AutumnAll[[1]], aes(x=trackheading)) + 
  geom_histogram(aes(fill=AutumnAll[[1]]$Aspeed, colour=AutumnAll[[1]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (m/s)", drop=F)+
  ggtitle(2007) + 
  ylab("Number of tracks")+ ylim(0,140000)+
  theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

#sphA2 <- 
ggplot(AutumnAll[[2]], aes(x=trackheading)) + 
  geom_histogram(aes(fill=AutumnAll[[2]]$Aspeed, colour=AutumnAll[[2]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("Flight direction") + 
  ylab("Number of tracks")+ ylim(0,132000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

library(ggpubr)
library(gridExtra)
library(grid)
sheadA <- ggarrange(sphA1, sphA2,  ncol=2, nrow=1, common.legend = TRUE, legend="bottom",widths=c(1.32,1)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
annotate_figure(sheadA,top = textGrob("Headings and air speeds Autumn",gp=gpar(fontsize=20,fontface="bold")))



#### PLOT A WIND ROSE OF BIRDS HEADINGS AND AIRSPEEDS IN SPRING

ggplot(SpringAll[[3]], aes(x=b.heading)) + 
  geom_histogram(aes(fill=SpringAll[[3]]$Aspeed, colour=SpringAll[[3]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("Heading") + 
  ylab("Number of tracks")+ ylim(0,105000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

#### PLOT A WIND ROSE OF BIRDS HEADINGS AND AIRSPEEDS IN SPRING

 ggplot(AutumnAll[[2]], aes(x=b.heading)) + 
  geom_histogram(aes(fill=AutumnAll[[2]]$Aspeed, colour=AutumnAll[[2]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("grey","grey0","forestgreen","firebrick1","coral3", "maroon"), name="Air speed (m/s)", drop=F)+
  ggtitle("Heading") + 
  ylab("Number of tracks")+ ylim(0,132000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

 
 
 
