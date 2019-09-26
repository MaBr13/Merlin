####Script to analyze departure locations of the birds + heading airspeed data from the radar location (which is also
#the same for departure locations as they were kept constant)


#prepare the data with the script "statistical analysis"

#make bounding boxes for different countries (the way to group departure locations)
Departures_int <- rbind(Departures_Spring_int,Departures_Autumn_int)
Departures_int$country <- with(Departures_int,
                               ifelse(Long_start>=4.74 & Long_start<=7.06 & 
                                        Lat_start>=50.69 & Lat_start<=57.62,"The Netherlands",
                                      ifelse(Long_start>=-12.24 & Long_start<=1.64 & 
                                               Lat_start>=48.75 & Lat_start<=58.98,"UK & Ireland",
                                             ifelse(Long_start>=4.64 & Long_start<11.07 & 
                                                      Lat_start>=57.68 & Lat_start<=71.16,"Norway",
                                                    ifelse(Long_start>=11.07 & Long_start<=18.86 & 
                                                             Lat_start>=54.94 & Lat_start<=68.47,"Sweden",
                                                           ifelse(Long_start>=8.17 & Long_start<=12.32 & 
                                                                    Lat_start>=54.78 & Lat_start<=57.65,"Denmark",
                                                                  ifelse(Long_start>=7.27 & Long_start<=14.00 & 
                                                                           Lat_start>=47.34 & Lat_start<=54.69,"Germany",
                                                                         ifelse(Long_start>=2.52 & Long_start<=6.47 & 
                                                                                  Lat_start>=49.40 & Lat_start<=51.19,"Belgium",
                                                                                ifelse(Long_start>=-1.87 & Long_start<=6.65 & 
                                                                                         Lat_start>=42.13 & Lat_start<=48.4,"France","Other")))))))))

Departures_Spring_int$country <- with(Departures_Spring_int,
                                                 ifelse(Long_start>=4.74 & Long_start<=7.06 & 
                                                          Lat_start>=50.69 & Lat_start<=57.62,"The Netherlands",
                                                        ifelse(Long_start>=-12.24 & Long_start<=1.64 & 
                                                                 Lat_start>=48.75 & Lat_start<=58.98,"UK & Ireland",
                                                               ifelse(Long_start>=4.64 & Long_start<11.07 & 
                                                                        Lat_start>=57.68 & Lat_start<=71.16,"Norway",
                                                                      ifelse(Long_start>=11.07 & Long_start<=18.86 & 
                                                                               Lat_start>=54.94 & Lat_start<=68.47,"Sweden",
                                                                             ifelse(Long_start>=8.17 & Long_start<=12.32 & 
                                                                                      Lat_start>=54.78 & Lat_start<=57.65,"Denmark",
                                                                                    ifelse(Long_start>=7.27 & Long_start<=14.00 & 
                                                                                             Lat_start>=47.34 & Lat_start<=54.69,"Germany",
                                                                                           ifelse(Long_start>=2.52 & Long_start<=6.47 & 
                                                                                                    Lat_start>=49.40 & Lat_start<=51.19,"Belgium",
                                                                                                  ifelse(Long_start>=-1.87 & Long_start<=6.65 & 
                                                                                                           Lat_start>=42.13 & Lat_start<=48.4,"France","Other")))))))))


Departures_Autumn_int$country <- with(Departures_Autumn_int,
                               ifelse(Long_start>=4.74 & Long_start<=7.06 & 
                                        Lat_start>=50.69 & Lat_start<=57.62,"The Netherlands",
                                      ifelse(Long_start>=-12.24 & Long_start<=1.64 & 
                                               Lat_start>=48.75 & Lat_start<=58.98,"UK & Ireland",
                                             ifelse(Long_start>=4.64 & Long_start<11.07 & 
                                                      Lat_start>=57.68 & Lat_start<=71.16,"Norway",
                                                    ifelse(Long_start>=11.07 & Long_start<=18.86 & 
                                                             Lat_start>=54.94 & Lat_start<=68.47,"Sweden",
                                                           ifelse(Long_start>=8.17 & Long_start<=12.32 & 
                                                                    Lat_start>=54.78 & Lat_start<=57.65,"Denmark",
                                                                  ifelse(Long_start>=7.27 & Long_start<=14.00 & 
                                                                           Lat_start>=47.34 & Lat_start<=54.69,"Germany",
                                                                         ifelse(Long_start>=2.52 & Long_start<=6.47 & 
                                                                                  Lat_start>=49.40 & Lat_start<=51.19,"Belgium",
                                                                                ifelse(Long_start>=-1.87 & Long_start<=6.65 & 
                                                                                         Lat_start>=42.13 & Lat_start<=48.4,"France","Other")))))))))


library(lubridate)
Departures_int$Season <- ifelse(month(Departures_int$date)>=8 & month(Departures_int$date)<12,"Autumn","Spring") 
#change the order of the countries according to latitude
Departures_int$country <- factor(Departures_int$country, levels = c("Norway", "Sweden","Denmark","Germany","The Netherlands", "Belgium",
                                                                    "UK & Ireland","France","Other"),
                                 labels  = c("Norway", "Sweden","Denmark","Germany","The Netherlands", "Belgium",
                                                    "UK & Ireland","France","Other"))
library(ggplot2)
#daily proportions of departure locations per season
ggplot(subset(Departures_int,month(date)>=2 & month(date)<=5),aes(x = factor(date), fill = country)) + 
  geom_bar(position = "fill")+
  ylab("Proportion") + 
  xlab("Date")+
  scale_colour_manual(values = rep("black",7))+
  scale_fill_brewer(palette="PuBuGn",name="Origin")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
#seasonal proportions of departure locations
colourCount = length(unique(Departures_int$country))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

  ggplot(Departures_int,aes(x = Season, fill = country)) + 
  geom_bar(position = "fill")+
  ylab("Proportion")+  
  scale_fill_brewer(palette="PuBuGn",name="Origin")+
  theme_minimal()
  
#figures per day
  
  p1 <- ggplot(subset(Departures_int,date=="2008-10-30"),aes(x=date, fill=country))+
    geom_bar(position = "fill",colour="black")+
    # coord_flip()+
    ylab("Proportion")+  
    scale_fill_brewer(palette="PuBuGn",name="")+
    theme_minimal()+
    theme(legend.position = "none",
          axis.title.y = element_text(size=18),
          axis.text.y=element_text(size=14), axis.text.x=element_blank(),
          axis.title.x = element_blank())
  
  
  p2 <- ggplot(subset(Departures_int,date=="2008-10-30"),aes(x=heading,fill=country))+
    geom_histogram(colour="black",breaks = seq(0,360,10))+
    ylab("Number") + 
    xlab("Heading (degrees)")+
    scale_colour_manual(values = rep("black",7))+
    scale_fill_brewer(palette="PuBuGn",name="")+
    theme_minimal()+
    theme(legend.position="none",
          axis.title.y = element_text(size=18),
          axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
          axis.title.x = element_text(size=18))+
    scale_x_continuous(breaks = seq(0,360,60))
  
  p3 <- ggplot(subset(Departures_int,date=="2008-04-22"),aes(x=winddir_start,fill=country))+
    geom_histogram(colour="black",breaks = seq(0,360,10))+
    ylab("Number") + 
    xlab("Wind direction (degrees)")+
    scale_colour_manual(values = rep("black",7))+
    scale_fill_brewer(palette="PuBuGn",name="")+
    theme_minimal()+
    theme(legend.position="none",
          axis.title.y = element_text(size=12),
          axis.text.y=element_text(size=12), axis.text.x=element_text(size=12),
          axis.title.x = element_text(size=12))+
    scale_x_continuous(breaks = seq(0,360,30))
  
  library(ggpubr)
  library(grid)
  
  ggarrange(p1,p2,labels = c("A","B"),ncol = 2,
            widths=c(0.3,1,1,1))+
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  #annotate_figure(migrants,top = textGrob("Diurnal and nocturnal migrants",gp=gpar(fontsize=24,fontface="bold")))

  
Other_S = nrow(Departures_Spring_int[Departures_Spring_int$country=="Other",]) / nrow(Departures_Spring_int)*100
Belgium_S = nrow(Departures_Spring_int[Departures_Spring_int$country=="Belgium",]) / nrow(Departures_Spring_int)*100
Denmark_S = nrow(Departures_Spring_int[Departures_Spring_int$country=="Denmark",]) / nrow(Departures_Spring_int)*100
France_S = nrow(Departures_Spring_int[Departures_Spring_int$country=="France",]) / nrow(Departures_Spring_int)*100
Germany_S = nrow(Departures_Spring_int[Departures_Spring_int$country=="Germany",]) / nrow(Departures_Spring_int)*100
Norway_S = nrow(Departures_Spring_int[Departures_Spring_int$country=="Norway",]) / nrow(Departures_Spring_int)*100
Sweden_S = nrow(Departures_Spring_int[Departures_Spring_int$country=="Sweden",]) / nrow(Departures_Spring_int)*100
Netherlands_S = nrow(Departures_Spring_int[Departures_Spring_int$country=="The Netherlands",]) / nrow(Departures_Spring_int)*100
UK_S = nrow(Departures_Spring_int[Departures_Spring_int$country=="UK & Ireland",]) / nrow(Departures_Spring_int)*100

Other_A = nrow(Departures_Autumn_int[Departures_Autumn_int$country=="Other",]) / nrow(Departures_Autumn_int)*100
Belgium_A = nrow(Departures_Autumn_int[Departures_Autumn_int$country=="Belgium",]) / nrow(Departures_Autumn_int)*100
Denmark_A = nrow(Departures_Autumn_int[Departures_Autumn_int$country=="Denmark",]) / nrow(Departures_Autumn_int)*100
France_A = nrow(Departures_Autumn_int[Departures_Autumn_int$country=="France",]) / nrow(Departures_Autumn_int)*100
Germany_A = nrow(Departures_Autumn_int[Departures_Autumn_int$country=="Germany",]) / nrow(Departures_Autumn_int)*100
Norway_A = nrow(Departures_Autumn_int[Departures_Autumn_int$country=="Norway",]) / nrow(Departures_Autumn_int)*100
Sweden_A = nrow(Departures_Autumn_int[Departures_Autumn_int$country=="Sweden",]) / nrow(Departures_Autumn_int)*100
Netherlands_A = nrow(Departures_Autumn_int[Departures_Autumn_int$country=="The Netherlands",]) / nrow(Departures_Autumn_int)*100
UK_A = nrow(Departures_Autumn_int[Departures_Autumn_int$country=="UK & Ireland",]) / nrow(Departures_Autumn_int)*100

#make histograms of distributions of headings and airspeeds
##NOTE: when saving plots, make them 700x700 and change fontsize to 14
##if saving in a grid, make the final picutre 2500x2000, and change fontsize to 28
s1 <- ggplot(subset(Departures_int,month(date)>=8 & month(date)<=11),aes(x = airspeed, fill = country)) + 
  geom_histogram(colour="black",breaks = seq(0,30,1))+
  theme_minimal()+
  ylab("Number") + 
  xlab("Airspeed (m/s)")+
  scale_colour_manual(values = rep("black",7))+
  scale_fill_brewer(palette="PuBuGn",name="")+
  theme(legend.position=c(0.85, 0.75),legend.text = element_text(size=14),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_text(size=12),
        axis.title.x = element_text(size=14), panel.grid = element_blank())+
  scale_x_continuous(breaks = seq(0,30,5))

s2 <- ggplot(subset(Departures_int,month(date)>=2 & month(date)<=5),aes(x = airspeed, fill = country)) + 
  geom_histogram(colour="black",breaks = seq(0,30,1))+
  ylab("Number") + 
  xlab("Airspeed (m/s)")+
  scale_colour_manual(values = rep("black",7))+
  scale_fill_brewer(palette="PuBuGn",name="")+
  theme_minimal()+
  theme(legend.position=c(0.85, 0.75),legend.text = element_text(size=14),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_text(size=12),
        axis.title.x = element_text(size=14),panel.grid = element_blank())+
  scale_x_continuous(breaks = seq(0,30,5))

s3 <- ggplot(subset(Departures_int,month(date)>=8 & month(date)<=11),aes(x = heading, fill = country)) + 
  geom_histogram(colour="black",breaks = seq(0,360,10))+
  ylab("Number") + 
  xlab("Heading (degrees)")+
  coord_polar()+
  scale_colour_manual(values = rep("black",7))+
  scale_fill_brewer(palette="PuBuGn",name="")+
  theme_minimal()+
  theme(legend.position=c(0.85, 0.75),legend.text = element_text(size=14),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_text(size=12),
        axis.title.x = element_text(size=14))+
  scale_x_continuous(breaks = seq(0,360,45))

s4 <- ggplot(subset(Departures_int,month(date)>=2 & month(date)<=5),aes(x = heading, fill = country)) + 
  geom_histogram(colour="black",breaks = seq(0,360,10))+
  ylab("Number") + 
  xlab("Heading (degrees)")+
  coord_polar()+
  scale_colour_manual(values = rep("black",7))+
  scale_fill_brewer(palette="PuBuGn",name="")+
  theme_minimal()+
  theme(legend.position=c(0.85, 0.75),legend.text = element_text(size=14),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_text(size=12),
        axis.title.x = element_text(size=14))+
  scale_x_continuous(breaks = seq(0,360,45))

library(ggpubr)
library(grid)
windows(8,8)
sf<- ggarrange(s2,s1,s4,s3,labels = c("(a)","(b)","",""),font.label = list(size = 22),ncol=2,nrow = 2,
          widths=c(1,1,0.8,0.8),common.legend = TRUE,legend ="bottom")+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), legend.text = element_text(size = 4))
ggsave(filename=paste0("C:/Users/mbradar/surfdrive/Documents/Maja_PhD/first_paper_ver1/Paper figures/season_histograms.png"),sf,dpi=500)
