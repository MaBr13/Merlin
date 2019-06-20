####SCRIPT FOR PICKING OUT THE NIGHTS WITH THE MOST INTENSE MIGRATION BASED ON 95 PERCENTILE



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
a <- nights$Date
check <- Allbirds[which(Allbirds$n.date %in% a),]
fsv <- check[,c(1:2,10,22:23)]
write.csv(fsv, file="intense_migration_v.csv",row.names = FALSE)
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
  geom_col(fill="black") +
  geom_hline(yintercept=quantile(counts$Nr.tracks,c(.95),type=8), color = "red")+
  #ggtitle("Number of tracks per day 2007-2010") +
  coord_cartesian(xlim=c(as.Date("2007-06-01"), as.Date("2010-06-01")))+
  #geom_vline(xintercept = as.numeric(means$Date[c(82,157,227,311,396,470,553,638,706,785,872,959)]),linetype=2,colour=c("black"),size=1.5)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Year") + ylab("Number of tracks per day") + ylim(0,20400) +
  scale_x_date(date_breaks="years", date_labels="%Y", limits=c(as.Date("2007-06-01"), as.Date("2010-06-01")))


nights1=subset(counts,Nr.tracks>=quantile(counts$Nr.tracks,c(.95),type=8))
nights2=subset(counts,Nr.tracks>=quantile(counts$Nr.tracks,c(.75),type=8))
nights3=subset(counts,Nr.tracks>=quantile(counts$Nr.tracks,c(.5),type=8))
nights4=subset(counts,Nr.tracks>=quantile(counts$Nr.tracks,c(.25),type=8))
nights5=subset(counts,Nr.tracks>=quantile(counts$Nr.tracks,c(.0),type=8))
nights <- nights[["Date"]]

numbers <- c(nrow(nights1),nrow(nights2),nrow(nights3),nrow(nights4),nrow(nights5))
quantiles <- c(.95,.75,.50,.25,.0)

qnts <- as.data.frame(cbind(numbers,quantiles))

ggplot(qnts, aes(quantiles,numbers)) +
  geom_col() +
  #geom_vline(xintercept = as.numeric(means$Date[c(82,157,227,311,396,470,553,638,706,785,872,959)]),linetype=2,colour=c("black"),size=1.5)+
  theme(axis.title.y = element_text(size=14), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=11), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Quantiles") + ylab("Number of nights") + 
  scale_y_continuous(breaks=seq(0,613,20)) + 
  scale_x_continuous(breaks=c(.0,.25,.5,.75,.95),labels=c("0%","25%","50%","75%","95%"))

