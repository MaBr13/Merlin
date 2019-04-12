library(foreign)
dataset<- read.spss( "C:\\Users\\mbradar\\Documents\\Merlin\\OWEZ\\data\\vertical\\OWEZ_X_tracks_2007-2010.sav", 
                     to.data.frame = T)

library(lubridate)
Sys.setenv(TZ="UTC")
Sys.setlocale(category = "LC_ALL", locale = "English_United Kingdom.1252")#set the time on your computer to match

dataset$date <- with(dataset, ymd(paste(Year,Month,Day, sep=' ')))
dataset$timestep <- with(dataset, ymd_h(paste(Year,Month,Day, Hour, sep= ' ')))

fstyear <- subset(dataset,Year==2007)
syear <- subset(dataset,Year==2008)
tyear <- subset(dataset,Year==2009)
fyear <- subset(dataset,Year==2010)

Allyears <- list(fstyear,syear,tyear,fyear)

rm(list = c("fstyear","syear","tyear", "fyear"))#remove all the lists you don't need

library(tidyverse)

sun <- list()
library(suncalc)
for (k in 1:length(Allyears)){
  sun[[k]]<- getSunlightTimes(unique(Allyears[[k]]$date),52.60636,4.389639,keep = c("sunrise","sunset"),tz="UTC")
  sun[[k]]$new.date <- as.Date(sun[[k]]$date)
  colnames(sun[[k]])[colnames(sun[[k]])==c("date","new.date")] <- c("timestep","date")
  sun[[k]]$sunrise <- sun[[k]]$sunrise-3600
}
Join <- list()

Join <- lapply(1:4, function(n){
  Allyears[[n]] %>% left_join(sun[[n]], by=c("date"))
})
##turn days into migration days
for (k in 1:length(Join)){
  s <- Join[[k]]
  date <- paste0(format(s[1,11], format="%Y-%m-%d"))
  time <- "16:00:00"
  start.date <- ymd(date) + hms(time)
  breaks = seq(start.date - 366*3600*24, start.date + 366*3600*24, "1 days")
  Join[[k]]$change = cut(Join[[k]]$timestep.x, breaks=breaks)
  Join[[k]]$n.year <- year(Join[[k]]$change)
  Join[[k]]$n.month <- month(Join[[k]]$change)
  Join[[k]]$n.day <- day(Join[[k]]$change)
  Join[[k]]$n.date <- with(Join[[k]],ymd(paste(n.year,n.month,n.day,sep = ' ')))
  Join[[k]]$migr.day <- with(Join[[k]],ymd_h(paste(n.year,n.month,n.day,Hour,sep = ' ')))
  Join[[k]] <- Join[[k]] %>% arrange(timestep.x)
}


for(k in 1:length(Join)){
  Join[[k]] <- subset(Join[[k]],Month>=2 & Month<=5 | Month>=8 & Month<12)
}

Birds <- list()

for (k in 1:length(Join)){
    Birds[[k]] <- subset(Join[[k]],light==0)
}
  

#dataset that contains numbers of birds between sunset and sunrise as recorded by the vertical radar
Allbirds <- do.call("rbind",Birds)
#calculate number of tracks per date
library(lubridate)


library(xts)
counts <- aggregate(Allbirds$Track_ID, by = list(Allbirds$n.date), FUN="length")
colnames(counts)[colnames(counts)==c("Group.1","x")] <- c("Date","Nr.tracks")

library(ggplot2)
ggplot(counts, aes(Date,Nr.tracks)) +
  annotate("rect",xmin=as.Date("2007-02-15"),xmax=as.Date("2007-05-31"),ymin=0,ymax=Inf,fill="forestgreen",alpha=0.4)+
  annotate("rect",xmin=as.Date("2007-08-01"),xmax=as.Date("2007-11-30"),ymin=0,ymax=Inf,fill="coral",alpha=0.4)+
  annotate("rect",xmin=as.Date("2008-02-15"),xmax=as.Date("2008-05-31"),ymin=0,ymax=Inf,fill="forestgreen",alpha=0.4)+
  annotate("rect",xmin=as.Date("2008-08-01"),xmax=as.Date("2008-11-30"),ymin=0,ymax=Inf,fill="coral",alpha=0.4)+
  annotate("rect",xmin=as.Date("2009-02-15"),xmax=as.Date("2009-05-31"),ymin=0,ymax=Inf,fill="forestgreen",alpha=0.4)+
  annotate("rect",xmin=as.Date("2009-08-01"),xmax=as.Date("2009-11-30"),ymin=0,ymax=Inf,fill="coral",alpha=0.4)+
  annotate("rect",xmin=as.Date("2010-02-15"),xmax=as.Date("2010-05-31"),ymin=0,ymax=Inf,fill="forestgreen",alpha=0.4)+
  geom_bar(stat="identity",fill="black") +
  geom_hline(yintercept=2000, color = "red")+
  #ggtitle("Number of tracks per day 2007-2010") +
  coord_cartesian(xlim=c(as.Date("2007-06-01"), as.Date("2010-06-01")))+
  #geom_vline(xintercept = as.numeric(means$Date[c(82,157,227,311,396,470,553,638,706,785,872,959)]),linetype=2,colour=c("black"),size=1.5)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Year") + ylab("Number of tracks per day") + ylim(0,20000) +
  scale_x_date(date_breaks="years", date_labels="%Y", limits=c(as.Date("2007-06-01"), as.Date("2010-06-01")))
