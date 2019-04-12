
####Data analysis of the vertical bird radar data (MERLIN)
###University of Amsterdam, IBED
## Maja Bradaric, June 2018



#loading spss files

library(foreign)
file.choose()
dataset<- read.spss( "C:\\Users\\mbradar\\Documents\\Merlin\\OWEZ\\data\\vertical\\OWEZ_X_tracks_2007-2010.sav", 
                      to.data.frame = T)
dataset1 <- read.spss("C:\\Users\\Maja\\Documents\\OWEZ data\\Xtracks_MM_MERLIN_2011_01_VerticalTTtotalnorain.sav", 
                      to.data.frame = T)
bla <- read.spss("C:\\Users\\mbradar\\Documents\\Radar projects\\Merlin\\OWEZ\\data\\vertical\\OWEZ_X_tracks_2014-2016.sav", 
                       to.data.frame = T)

#subsetting and adding timestep with library lubridate (sometimes it doesn't work, probably becuase of different
#time specifications on different computers)

Firstyear <- subset(dataset, Year==2009, select=Track_ID:light)
Firstyear <- subset(Firstyear,Month>2, select=Track_ID:light)
Firstyear <- subset(Firstyear,Month<6, select=Track_ID:light)
Firstyear <- subset(Firstyear, Day==29, select=Track_ID:light)

library(lubridate)
Sys.setenv(TZ="UTC")
Sys.setlocale(category = "LC_ALL", locale = "English_United Kingdom.1252")#set the time on your computer to match

dataset$date <- with(dataset, ymd(paste(Year,Month,Day, sep=' ')))
dataset$timestep <- with(dataset, ymd_h(paste(Year,Month,Day, Hour, sep= ' ')))

#aggregating by timestamp and according to different function that you specify in brackets (the list of functions
#to be used can be find in a help page) and merging the data frames

library(xts)
counts <- aggregate(Firstyear$Track_ID, by = list(Firstyear$date), FUN="length")
mean.alt <- aggregate(Firstyear$Altitudem_mean, by = list(Firstyear$date), FUN="mean")
date <- aggregate(Firstyear$date, by=list(Firstyear$date), FUN="mean")
g <- merge(counts, mean.alt, by="Group.1", sort = TRUE) #TABLE FOR ANALYSIS
means <- merge(g,date,by="Group.1", sort=TRUE)
names(means)[c(1,2,3,4)]<-paste(c("Timestamp","Nr.tracks", "Mean.alt", "Date"))


#plotting
par(mar = c(5,5,2,5))
with(means, plot(Date, Mean.alt, type="h", col="aquamarine", 
             ylab="Altitude", xlab = "Month", xlim=c(as.Date("2008-08-01"), as.Date("2008-12-31")),
             ylim=c(0,1500),  main="Number of tracks and altitude per hour 2015")) 
par(new = T)
with(means, plot(Date, Nr.tracks, type="p", pch=5, axes=F, xlab=NA, ylab=NA, cex=0.5, col="aquamarine4", 
                 xlim=c(as.Date("2015-01-01"), as.Date("2016-01-01")), ylim=c(0,4000)))
axis(side = 4)
mtext(side = 4, line = 3, 'Density')
legend("topleft",
       legend=c("Mean altitude per hour", "Number of tracks per hour"),
       lty=c(1,1), pch=c(NA, NA), col=c("aquamarine", "aquamarine4"))

#ggplotting
require(ggplot2)
            
ggplot(means, aes(Date,Nr.tracks)) +
  annotate("rect",xmin=means$Date[82],xmax=means$Date[157],ymin=0,ymax=Inf,fill="coral",alpha=0.4)+
  annotate("rect",xmin=means$Date[227],xmax=means$Date[311],ymin=0,ymax=Inf,fill="forestgreen",alpha=0.4)+
  annotate("rect",xmin=means$Date[396],xmax=means$Date[470],ymin=0,ymax=Inf,fill="coral",alpha=0.4)+
  annotate("rect",xmin=means$Date[553],xmax=means$Date[638],ymin=0,ymax=Inf,fill="forestgreen",alpha=0.4)+
  annotate("rect",xmin=means$Date[706],xmax=means$Date[785],ymin=0,ymax=Inf,fill="coral",alpha=0.4)+
  annotate("rect",xmin=means$Date[872],xmax=means$Date[959],ymin=0,ymax=Inf,fill="forestgreen",alpha=0.4)+
  geom_bar(stat="identity",fill="black") +
  ggtitle("Number of tracks per day 2007-2010") +
  #geom_vline(xintercept = as.numeric(means$Date[c(82,157,227,311,396,470,553,638,706,785,872,959)]),linetype=2,colour=c("black"),size=1.5)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Year") + ylab("Number of tracks per day") + ylim(0,20000) +
  scale_x_date(date_breaks="years", date_labels="%Y", limits=c(as.Date("2007-01-01"), as.Date("2010-06-01")))

#zoom in
ggplot(means, aes(Date,Nr.tracks)) +
  annotate("rect",xmin=means$Date[82],xmax=means$Date[157],ymin=0,ymax=Inf,fill="coral",alpha=0.4)+
  annotate("rect",xmin=means$Date[227],xmax=means$Date[311],ymin=0,ymax=Inf,fill="forestgreen",alpha=0.4)+
  annotate("rect",xmin=means$Date[396],xmax=means$Date[470],ymin=0,ymax=Inf,fill="coral",alpha=0.4)+
  annotate("rect",xmin=means$Date[553],xmax=means$Date[638],ymin=0,ymax=Inf,fill="forestgreen",alpha=0.4)+
  annotate("rect",xmin=means$Date[706],xmax=means$Date[785],ymin=0,ymax=Inf,fill="coral",alpha=0.4)+
  annotate("rect",xmin=means$Date[872],xmax=means$Date[959],ymin=0,ymax=Inf,fill="forestgreen",alpha=0.4)+
  geom_bar(stat="identity",fill="black") +
  ggtitle("Number of tracks per day October 2008") +
  coord_cartesian(xlim=c(as.Date("2009-10-01"), as.Date("2009-10-31")))+
  #geom_vline(xintercept = as.numeric(means$Date[c(82,157,227,311,396,470,553,638,706,785,872,959)]),linetype=2,colour=c("black"),size=1.5)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Year") + ylab("Number of tracks per day") + ylim(0,20000) +
  scale_x_date(date_breaks="days", date_labels="%d", limits=c(as.Date("2009-10-01"), as.Date("2009-10-31")))

#altitude 1

ggplot(Firstyear, aes(Altitudem_mean)) +
  geom_histogram(fill="steelblue4", colour="black",
                 breaks=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400)) +
  ggtitle("Flight altitudes Spring 2008") +
  coord_flip()+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=14), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=12), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Altitudes") + ylab("Number of tracks") + ylim(0,240000)+
scale_x_continuous("",limits=c(0,1400), breaks = c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400))

#altitude 2
ggplot(means, aes(Date,Mean.alt)) +
  geom_bar(stat="identity",fill="steelblue4", colour="black") +
  ggtitle("Flight altitudes per day 2010") +
  #coord_flip()+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=14), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=12), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Months") + ylab("Number of tracks") +
  scale_x_date(date_breaks="months", date_labels="%m", limits=c(as.Date("2010-01-01"), as.Date("2010-12-31")))



#ploting and ggploting for days

par(mar = c(5,5,2,5))
with(means, plot(Timestamp, Mean.alt, type="h", lwd=5, col="aquamarine", 
                 ylab="Altitude", xlab = "Hour", ylim=c(0,1500),  main="Number of tracks per hour 29th of Oct 2014")) 
par(new = T)
with(means, plot(Timestamp, Nr.tracks, type="p", pch=5, axes=F, xlab=NA, ylab=NA, cex=0.5, col="aquamarine4", 
                 ylim=c(0,4000)))
axis(side = 4)
mtext(side = 4, line = 3, 'Density')
legend("topleft",
       legend=c("Mean altitude per hour", "Number of tracks per hour"),
       lty=c(1,1), pch=c(NA, NA), col=c("aquamarine", "aquamarine4"))

ggplot(means, aes(Timestamp,Nr.tracks)) +
  geom_point(alpha=1, size=6) +
  scale_colour_gradientn(colours = terrain.colors(20), limits=c(0,1400), guide = guide_colorbar(title=expression(paste('Altitude (m)', sep=""))))+
  ggtitle("Number of tracks and altitude per hour 29th of Oct 2008") +
  theme(axis.title.y = element_text(size=18), axis.title.x = element_blank(),legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        plot.title = element_text(size = 18, face = "bold"))+
  xlab("Hour") + ylab("Number of tracks per hour") + ylim(0,4000)


#plotting number of migrants during different times of the day

require(ggplot2)
  
ggplot(Firstyear, aes(x = Firstyear$light))+
  geom_bar(fill=c("midnightblue","lightskyblue"))+
  ggtitle("Number of diurnal and nocturnal migrants Oct 2008") +
  xlab("Time od the day") + ylab("Number of tracks per hour") + 
  scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))
 #"Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

one <- subset(dataset, Year==2007, select=Track_ID:light)
two <- subset(dataset, Year==2008, select=Track_ID:light)
three <- subset(dataset, Year==2009, select=Track_ID:light)
four <- subset(dataset, Year==2010, select=Track_ID:light)
five <-  subset(bla, Year==2014, select=Track_ID:light)
six <- subset(bla, Year==2015, select=Track_ID:light)
seven <- subset(bla, Year==2016, select=Track_ID:light)

plot1 <- ggplot(one, aes(x = one$light))+
  geom_bar(fill=c("midnightblue","lightskyblue"))+
  ggtitle("2007") +
  ylab("Number of tracks per hour") +
  ylim(0,320000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(size = 18))+
  scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))
 
plot2 <- ggplot(two, aes(x = two$light)) +
  geom_bar(fill=c("midnightblue","lightskyblue"))+
  ggtitle("2008") +
  ylim(0,320000)+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=14),
        plot.title = element_text(size = 16)) +
 scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))

plot3 <- ggplot(three, aes(x = three$light)) +
  geom_bar(fill=c("midnightblue","lightskyblue"))+
  ggtitle("2009") +
  ylim(0,320000)+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=14),
        plot.title = element_text(size = 16)) +
  scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))

plot4 <- ggplot(four, aes(x = four$light)) +
  geom_bar(fill=c("midnightblue","lightskyblue"))+
  ggtitle("2010") +
  ylim(0,320000)+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=14),
        plot.title = element_text(size = 16)) +
  scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))


library(gridExtra)
grid.arrange(plot1,plot2,plot3,plot4,ncol=4,top = textGrob("Number of diurnal and nocturnal migrants",gp=gpar(fontsize=20,fontface="bold")),
              bottom=textGrob("Time of day",gp=gpar(fontsize=14)), widths=c(1.2,1,1,1))

octone <- subset(one,Month==10, select=Track_ID:light)
octtwo <- subset(two,Month==10, select=Track_ID:light)
octthr <- subset(three,Month==10, select=Track_ID:light)


p1 <- ggplot(octone, aes(x = octone$light))+
  geom_bar(fill=c("midnightblue","lightskyblue"))+
  ggtitle("October 2007") +
  ylab("Number of tracks per hour") +
  ylim(0,125000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(size = 18))+
  scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))

p2 <- ggplot(octtwo, aes(x = octtwo$light)) +
  geom_bar(fill=c("midnightblue","lightskyblue"))+
  ggtitle("October 2008") +
  ylim(0,125000)+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=14),
        plot.title = element_text(size = 16)) +
  scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))

p3 <- ggplot(octthr, aes(x = octthr$light)) +
  geom_bar(fill=c("midnightblue","lightskyblue"))+
  ggtitle("October 2009") +
  ylim(0,125000)+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=14),
        plot.title = element_text(size = 16)) +
  scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))


library(gridExtra)
grid.arrange(p1,p2,p3, ncol=3, top = textGrob("Number of diurnal and nocturnal migrants",gp=gpar(fontsize=20,fontface="bold")),
             bottom=textGrob("Time of day",gp=gpar(fontsize=14)), widths=c(1.2,1,1))

#spring
octone <- subset(one,Month==3, select=Track_ID:light)
octtwo <- subset(two,Month==3, select=Track_ID:light)
octthr <- subset(three,Month==3, select=Track_ID:light)
octfour <- subset(four,Month==3, select=Track_ID:light)

p1 <- ggplot(octtwo, aes(x = octtwo$light))+
  geom_bar(fill=c("midnightblue","lightskyblue"))+
  ggtitle("March 2008") +
  ylab("Number of tracks per hour") +
  ylim(0,60000)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(size = 18))+
  scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))

p2 <- ggplot(octfour, aes(x = octfour$light)) +
  geom_bar(fill=c("midnightblue","lightskyblue"))+
  ggtitle("March 2010") +
  ylim(0,60000)+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=14),
        plot.title = element_text(size = 16)) +
  scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))

p3 <- ggplot(octthr, aes(x = octthr$light)) +
  geom_bar(fill=c("midnightblue","lightskyblue"))+
  ggtitle("March 2009") +
  ylim(0,60000)+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=14),
        plot.title = element_text(size = 16)) +
  scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))



library(gridExtra)
grid.arrange(p1,p3,p2 ,ncol=3, top = textGrob("Number of diurnal and nocturnal migrants",gp=gpar(fontsize=20,fontface="bold")),
             bottom=textGrob("Time of day",gp=gpar(fontsize=14)), widths=c(1.2,1,1))
