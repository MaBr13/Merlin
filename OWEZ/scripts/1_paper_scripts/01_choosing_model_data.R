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

#if you want to include only summer and autumn seasons in your graph
#SKIP IF YOU WANT ALL DATA TO SHOW IN YOUR GRAPH!!!
for(k in 1:length(Join)){
  Join[[k]] <- subset(Join[[k]],Month>=2 & Month<=5 | Month>=8 & Month<12)
}

Birds <- list()

for (k in 1:length(Join)){
    Birds[[k]]<- subset(Join[[k]],light==0)
  }
    
#dataset that contains numbers of birds between sunset and sunrise as recorded by the vertical radar
Allbirds <- do.call("rbind",Birds)
#calculate number of tracks per date
library(xts)
counts <- aggregate(Allbirds$Track_ID, by = list(Allbirds$n.date), FUN="length")
colnames(counts)[colnames(counts)==c("Group.1","x")] <- c("Date","Nr.tracks")

#divide in spring and summer for quantile calculation for different seasons
spring <- subset(Allbirds, Month>=2 & Month<=5)
autumn <- subset(Allbirds, Month>=8 & Month<=11)
#calculate number of tracks per date per season
countsS <- aggregate(spring$Track_ID, by = list(spring$n.date), FUN="length")
colnames(countsS)[colnames(countsS)==c("Group.1","x")] <- c("Date","Nr.tracks")
countsA <- aggregate(autumn$Track_ID, by = list(autumn$n.date), FUN="length")
colnames(countsA)[colnames(countsA)==c("Group.1","x")] <- c("Date","Nr.tracks")

#visualize the full dataset with lines that represent 95% quantiles for spring and autumn season (coloured differently)
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
  geom_hline(yintercept=quantile(countsA$Nr.tracks,c(.95),type=8), color = "red")+
  geom_hline(yintercept=quantile(countsS$Nr.tracks,c(.95),type=8), color = "blue")+
  #ggtitle("Number of tracks per day 2007-2010") +
  coord_cartesian(xlim=c(as.Date("2007-06-01"), as.Date("2010-06-01")))+
  #geom_vline(xintercept = as.numeric(means$Date[c(82,157,227,311,396,470,553,638,706,785,872,959)]),linetype=2,colour=c("black"),size=1.5)+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Year") + ylab("Number of tracks per day") + ylim(0,20400) +
  scale_x_date(date_breaks="years", date_labels="%Y", limits=c(as.Date("2007-06-01"), as.Date("2010-06-01")))

#calculate and visualize all the quantiles (for all seasons)
nights1S=subset(countsS,Nr.tracks>=quantile(countsS$Nr.tracks,c(.95),type=8))
nights2S=subset(countsS,Nr.tracks>=quantile(countsS$Nr.tracks,c(.75),type=8))
nights3S=subset(countsS,Nr.tracks>=quantile(countsS$Nr.tracks,c(.5),type=8))
nights4S=subset(countsS,Nr.tracks>=quantile(countsS$Nr.tracks,c(.25),type=8))
nights5S=subset(countsS,Nr.tracks>=quantile(countsS$Nr.tracks,c(.0),type=8))
nightsS <- nights1S[["Date"]]

numbersS <- c(nrow(nights1S),nrow(nights2S),nrow(nights3S),nrow(nights4S),nrow(nights5S))
quantilesS <- c(.95,.75,.50,.25,.0)

qntsS <- as.data.frame(cbind(numbersS,quantilesS))

nights1A=subset(countsA,Nr.tracks>=quantile(countsA$Nr.tracks,c(.95),type=8))
nights2A=subset(countsA,Nr.tracks>=quantile(countsA$Nr.tracks,c(.75),type=8))
nights3A=subset(countsA,Nr.tracks>=quantile(countsA$Nr.tracks,c(.5),type=8))
nights4A=subset(countsA,Nr.tracks>=quantile(countsA$Nr.tracks,c(.25),type=8))
nights5A=subset(countsA,Nr.tracks>=quantile(countsA$Nr.tracks,c(.0),type=8))
nightsA <- nights1A[["Date"]]

numbersA <- c(nrow(nights1A),nrow(nights2A),nrow(nights3A),nrow(nights4A),nrow(nights5A))
quantilesA <- c(.95,.75,.50,.25,.0)

qntsA <- as.data.frame(cbind(numbersA,quantilesA))

grob <- grobTree(textGrob(c("q=6430", x=0,  y=0.95, hjust=0,
                          gp=gpar(col="white", fontsize=13, fontface="italic"))))
ggplot(qntsS, aes(quantilesS,numbersS)) +
  geom_col() +
 annotation_custom(grob)+
  #geom_vline(xintercept = as.numeric(means$Date[c(82,157,227,311,396,470,553,638,706,785,872,959)]),linetype=2,colour=c("black"),size=1.5)+
  theme(axis.title.y = element_text(size=14), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=11), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Quantiles (%)") + ylab("Number of nights") + 
  scale_y_continuous(breaks=seq(0,613,20)) + 
  scale_x_continuous(breaks=c(.0,.25,.5,.75,.95),labels=c("0","25","50","75","95"))+
  theme_light()

#plot nights (ranked by intensity) for spring and autumn and draw a line for 95%

windows(6.5,2)

main.plot1 <- ggplot(countsS,aes(x=reorder(Date, -Nr.tracks), Nr.tracks))+
  geom_col(col="black",fill="cyan4",width = 1)+
  geom_vline(xintercept = which.min(abs(sort(countsS$Nr.tracks,decreasing=TRUE) - 
                                          quantile(countsS$Nr.tracks,0.95,type=8)))+0.5,col="red")+
  xlab("Night") + ylab("Number of tracks") +
  theme_minimal()+
  theme(axis.title.y = element_blank(), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=11), axis.text.x=element_blank(),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"),
        axis.ticks.x = element_blank(),panel.grid = element_blank())+
  scale_y_continuous(expand = c(0,0)) #force y-axis to start from 0
 

main.plot2 <- ggplot(countsA,aes(x=reorder(Date, -Nr.tracks), Nr.tracks))+
  geom_col(col="black",fill="cyan4",width = 1)+
  geom_vline(xintercept = which.min(abs(sort(countsA$Nr.tracks,decreasing=TRUE) - 
                                          quantile(countsA$Nr.tracks,0.95,type=8)))+0.5,col="red")+
  xlab("Night") + ylab("Number of tracks") +
  theme_minimal()+
  theme(axis.title.y = element_blank(), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=11), axis.text.x=element_blank(),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"),
        axis.ticks.x = element_blank(),panel.grid = element_blank())+
  scale_y_continuous(expand = c(0,0)) #force y-axis to start from 0

inset.plot <- ggplot(countsA,aes(x=reorder(Date, -Nr.tracks), Nr.tracks))+
  geom_col(col="black",fill="cyan4",width = 1)+
  scale_y_continuous(expand = c(0,0)) +#force y-axis to start from 0+
  scale_x_discrete(expand=c(-.032,0),breaks=as.character(nightsA))+
  coord_cartesian(xlim =c(0,which.min(abs(sort(countsA$Nr.tracks,decreasing=TRUE) - 
                                            quantile(countsA$Nr.tracks,0.95,type = 8)))+1),clip = "on")+
  theme_bw()+
  xlab("Night") + ylab("Number of tracks") +
  theme(axis.title.y = element_text(size=14), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=11,face="bold",margin =unit(c(0,0,0,0),"cm")),
        axis.text.x=element_text(size=11,angle = 90,face = "bold"), 
        axis.title.x = element_text(size=14), 
        plot.title = element_text(size = 18, face = "bold"),panel.grid = element_blank(),axis.ticks.y = element_blank())
 

png(filename=paste0("C:/Users/mbradar/Documents/check.png"),width=6.5,height = 4,units = "in",res=500)
library(ggpubr)
library(grid)

p=ggarrange(main.plot1,main.plot2,labels = c("(a)","(b)"),font.label = list(size = 14),ncol=2,nrow = 1,
          widths=c(1,1,1,1),common.legend = TRUE,legend ="right",label.x=c(0.8,.8))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
p=annotate_figure(p,bottom = text_grob("Night",size=14),left = text_grob("Number of tracks",size=14,rot=90))
ggsave(filename=paste0("C:/Users/mbradar/Documents/check.png"),p,dpi=500)

windows(4,2)
  plot1 <-   ggdraw() +
    draw_plot(main.plot) +
    draw_label("(a)",x=.22,y=.93,size=18,fontface = "bold")+
    draw_plot(inset.plot, x = 0.29, y = .2, width = .7, height = .8)+
    draw_label("(b)",x=.95,y=.93,size=18,fontface = "bold")
dev.off()

#plot proportions of tracks on nights of intense migration and full season
autumn$Autumn <- ifelse(autumn$n.date %in% nightsA,"IM","FS")
spring$Autumn <- ifelse(spring$n.date %in% nightsS,"IM","FS")
countsA$Autumn <- ifelse(countsA$Date %in% nightsA,"IM","FS")
countsS$Autumn <- ifelse(countsS$Date %in% nightsS,"IM","FS")

prA <- data.frame(Autumn=c("FS","IM"),
                  Nr.tracks=c(sum(countsA$Nr.tracks),sum(nights1A$Nr.tracks)),
                  Nr.nights=c(nrow(countsA),nrow(nights1A))) 
 
nrs <-   ggplot(NULL,mapping=aes(x=Autumn,y = (..count..)/sum(..count..))) +
  geom_bar(data=countsS,width=.4, position=position_nudge(x = -0.2),col="black", fill="darkcyan")+
  geom_text(data=countsS,aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
              stat = "count", vjust = -0.25,hjust=1) +
  geom_bar(data=countsA,width=.4, position=position_nudge(x = 0.2),col="black",fill="steelblue4") +
  geom_text(data=countsA,aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
              stat = "count", vjust = -0.25,hjust=-0.2) +
  ylim(0,1)+ylab("Proportion")+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=14),legend.key = element_blank(),
        axis.text.y = element_blank(),axis.ticks = element_blank())
 

trs <- ggplot(NULL,mapping=aes(x=Autumn,y = (..count..)/sum(..count..))) +
  geom_bar(data=spring,width=.4, position=position_nudge(x = -0.2),col="black", fill="darkcyan")+
  geom_text(data=spring,aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25,hjust=1) +
  geom_bar(data=autumn,width=.4, position=position_nudge(x = 0.2),col="black",fill="steelblue4") +
  geom_text(data=autumn,aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25,hjust=-0.2) +
  ylim(0,1)+ylab("Proportion")+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.key = element_blank(),
        axis.text.y = element_blank(),axis.ticks = element_blank(),axis.line.y = element_blank())


windows(6.5,4)
n=ggarrange(nrs,trs,labels = c("(a)","(b)"),font.label = list(size = 14),ncol=2,nrow = 1,
            widths=c(1,1,1,1),common.legend = TRUE,legend ="right",label.x=c(0.8,.8))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
ggsave(filename=paste0("C:/Users/mbradar/surfdrive/Documents/Maja_PhD/first_paper_ver1/Paper figures/percentage_night.png"),n,dpi=500)


#use dates calculated based on 95% to extract nights for the analysis from the horizontal radar

a <- c(nightsS$Date,nightsA$Date)
setwd('C:/Users/mbradar/Documents/Merlin/OWEZ/Model')
write.csv(a,file = 'nights.csv',row.names = FALSE )
check <- Allbirds[which(Allbirds$n.date %in% a),]
fsv <- check[,c(1:2,10,22:23)]