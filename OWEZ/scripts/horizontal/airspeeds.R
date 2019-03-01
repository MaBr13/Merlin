#start with Allyears table (load data pack)

library(dplyr)
library(tidyr)
Speed<- list()
Speed <- lapply(1:4, function(n){
  Allyears[[n]] %>%
    group_by(id) %>%
    select(id=id,season=season,airspeed=airspeedms,
           date=date, timestamp=timestep,n.date=n.date, migr.day=migr.day)})

speed <- do.call(rbind,Speed)  

speedS <- subset(speed,season==2)
speedA <- subset(speed,season==4)

speedS <- speedS %>% drop_na()
speedA <- speedA %>% drop_na()

test <- filter(speedS,  !is.na(speedS$airspeed))

NASS <- subset(speedS,is.na(speedS$airspeed)) 
NASAt <-subset(speedA,is.na(speedA$airspeed)) 

NASS <- NASS %>%
    group_by(timestamp) %>%
    summarize(Nr=length(id),date=median(date),n.date=median(n.date), 
           migr.day=median(migr.day))

require(ggplot2)

breaks <- seq(0,150,5)
par(mar=c(3,3,3,3))
ggplot(speedA,aes(airspeed))+
  geom_histogram(fill="mediumpurple4",col="black",breaks=breaks)+
  ggtitle("Distribution of airspeeds Autumn")+xlab("airspeed (m/s)")+ ylab("nr. tracks") + 
  theme(axis.title.y = element_text(size=12),
        axis.text.y=element_text(size=9), axis.text.x=element_text(size=9), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size=12), plot.title = element_text(size = 14, face = "bold"))+
  scale_x_continuous(limits=c(0,150), breaks = breaks)
  

  

ggplot(NASS, aes(date,Nr))+
  geom_bar(stat="identity")+
  scale_x_date(date_breaks="days", date_labels="%d",limits = c(as.Date("2010-03-01"),as.Date("2010-05-31")))
