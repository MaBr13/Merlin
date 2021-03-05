# University of Amsterdam, IBED, 04/09/2019
#Maja Bradaric

#load the packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(circular)
library(xts)
library(tidyverse)
#load the data (wind conditions are from ECMWF 1000 hPa)
#setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Iris_paper/data")
#Autumn <- read.csv("Autumn.csv",sep=",")
#Autumn$n.date <- date(ISOdate(Autumn$n.year,Autumn$n.month,Autumn$n.day,tz="UTC"))
#Spring <- read.csv("Spring.csv",sep=",")
#Spring$n.date <- date(ISOdate(Spring$n.year,Spring$n.month,Spring$n.day,tz="UTC"))
#load the data from scratch to calculate headings with ERA5 data
path <- setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/data/horizontal/bigger tables")
listcsv <- dir(path,pattern = "*.csv",ignore.case = FALSE)
#getting yearly data files
Fstyear <- listcsv[1:9]
Syear <- listcsv[10:21]
Tyear <- listcsv[22:28]
Fyear <- listcsv[29:31]

#1st year
nc <- ncol(read.csv(Fstyear[1], sep=';',header = TRUE, nrows=1))#determine number of total columns
colClasses <- replace(rep("NULL", nc),c(1,8:12,14:15,17,20:22,26,29),NA)#determine which columns to use
#(NA for those that will be used)
listFstyear <- lapply(Fstyear,read.csv,sep=';',header= TRUE, colClasses=colClasses)#apply function to all 

#2nd year
nc <- ncol(read.csv(Syear[1], sep=';',header = TRUE, nrows=1))
colClasses <- replace(rep("NULL", nc),c(1,8:12,14:15,17,20:22,26,29),NA)
listSyear <- lapply(Syear,read.csv,sep=';',header= TRUE, colClasses=colClasses)
#3rd year
nc <- ncol(read.csv(Tyear[1], sep=';',header = TRUE, nrows=1))
colClasses <- replace(rep("NULL", nc),c(1,8:12,14:15,17,20:22,26,29),NA)
listTyear <- lapply(Tyear,read.csv,sep=';',header= TRUE, colClasses=colClasses)
#4th year
nc <- ncol(read.csv(Fyear[1], sep=';',header = TRUE, nrows=1))
colClasses <- replace(rep("NULL", nc),c(1,8:12,14:15,17,20:22,26,29),NA)
listFyear <- lapply(Fyear,read.csv,sep=';',header= TRUE, colClasses=colClasses)
#create timestamp and date in all the lists
library(lubridate)
Sys.setenv(TZ="UTC")
Sys.setlocale(category = "LC_ALL", locale = "English_United Kingdom.1252")#set the time on your computer to match

for(k in 1:length(listFstyear)){
  listFstyear[[k]]$date <- with(listFstyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listFstyear[[k]]$timestep <- with(listFstyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
}
for(k in 1:length(listSyear)){
  listSyear[[k]]$date <- with(listSyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listSyear[[k]]$timestep <- with(listSyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
}
for(k in 1:length(listTyear)){
  listTyear[[k]]$date <- with(listTyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listTyear[[k]]$timestep <- with(listTyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
}
for(k in 1:length(listFyear)){
  listFyear[[k]]$date <- with(listFyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listFyear[[k]]$timestep <- with(listFyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
}
#merge together all the months in a year
#y2007 <- do.call("rbind", listFstyear)
y2007 <- do.call("rbind", listFstyear)
y2008 <- do.call("rbind", listSyear)
y2009 <- do.call("rbind", listTyear)
y2010 <- do.call("rbind", listFyear)
#create values to use for plots on a yearly basis to visualize tracks and groundspeed
Allyears <- list(y2007,y2008,y2009,y2010)
rm(list = c("listFstyear","listFyear","listSyear", 
            "listTyear", "y2007", "y2008", "y2009","y2010","colClasses","Fstyear",
            "Syear","Tyear","Fyear","listcsv","nc"))#remove all the lists you don't need

#make migratory nights
for (k in 1:length(Allyears)){
  s <- Allyears[[k]]
  date <- paste0(format(s[1,15], format="%Y-%m-%d"))
  time <- "16:00:00"
  start.date <- ymd(date) + hms(time)
  breaks = seq(start.date - 366*3600*24, start.date + 366*3600*24, "1 days")
  Allyears[[k]]$change = cut(Allyears[[k]]$timestep, breaks=breaks)
  Allyears[[k]]$n.year <- year(Allyears[[k]]$change)
  Allyears[[k]]$n.month <- month(Allyears[[k]]$change)
  Allyears[[k]]$n.day <- day(Allyears[[k]]$change)
  Allyears[[k]]$n.date <- with(Allyears[[k]],ymd(paste(n.year,n.month,n.day,sep = ' ')))
  Allyears[[k]]$migr.day <- with(Allyears[[k]],ymd_h(paste(n.year,n.month,n.day,uur,sep = ' ')))
  Allyears[[k]] <- Allyears[[k]] %>% arrange(timestep)
}
#nights of intense and low migration
intense_S <- as.Date(c("2008-03-27","2008-04-22","2009-03-12","2009-03-16","2010-03-14","2010-03-16","2010-03-21",
                       "2010-03-26"))
low_S <- as.Date(c("2008-03-24","2008-03-29","2008-04-18","2009-03-10","2009-03-17","2009-03-19","2010-03-19"))
intense_A <- as.Date(c("2007-10-02","2007-10-05","2007-10-10","2007-10-13","2007-10-19",
                       "2008-10-29","2009-10-13","2009-10-30","2009-11-08"))
low_A <- as.Date(c("2007-10-03","2007-10-11","2007-10-15","2008-10-26","2009-10-11","2009-10-18","2009-10-22",
                   "2009-10-31"))
#make a dataframe out of the list and divide into groups based on seasonal low and intense migration nights
Allyears <- do.call("rbind",Allyears)
Spring_int <- Allyears[which(Allyears$n.date %in% intense_S),]
Spring_low <- Allyears[which(Allyears$n.date %in% low_S),]
Autumn_int <- Allyears[which(Allyears$n.date %in% intense_A),]
Autumn_low <- Allyears[which(Allyears$n.date %in% low_A),]

rm(list = c("s","start.date","breaks","date","time"))#remove all the lists you don't need

#load the wind data from the radar location
setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Iris_paper/data")
weatherR <- readLines("U-V_52N-4E_2007-2010.txt")
weatherR <- weatherR[-(1:7)]
weatherR <- read.table(text=weatherR, header=F)
weatherR <- weatherR[,5:7]
colnames(weatherR)[colnames(weatherR)==c("V5","V6","V7")] <- c("date","u","v")
#turn date into a datetime object
weatherR$timestamp <- as.POSIXct(as.character(weatherR$date),tz="UTC",format="%d-%b-%Y %H")
weatherR$date <- as.POSIXct(as.character(weatherR$date),tz="UTC",format="%d-%b-%Y")
#calculate wind direction and wind speed from u and v components
weatherR$windspeedms <-  sqrt((weatherR$u * weatherR$u) + (weatherR$v * weatherR$v))
#wind direction in radians
winddirR <- atan2(weatherR$u, weatherR$v)
#wind direction in deg
weatherR$winddir <- winddirR*(180/pi)
#360 modulo
weatherR$winddir <- ifelse(weatherR$winddir<0, 360+weatherR$winddir, weatherR$winddir)
colnames(weatherR)[colnames(weatherR)=="timestamp"] <- "timestep"

spring_int <- Spring_int %>% left_join(weatherR, by=c("timestep"))
spring_low <- Spring_low %>% left_join(weatherR, by=c("timestep"))
autumn_int <- Autumn_int %>% left_join(weatherR, by=c("timestep"))
autumn_low <- Autumn_low %>% left_join(weatherR, by=c("timestep"))

nights <- list(spring_int,spring_low,autumn_int,autumn_low)
for (k in 1:length(nights)){
  
  nights[[k]] <- nights[[k]]%>% drop_na
  
}

for (k in 1:length(nights)){
  nights[[k]]$groundspeedms <- nights[[k]]$groundspeedkph/3.6
  trackheadingR <- nights[[k]]$trackheading*(pi/180)#formula for conversion to radians
  winddirR <- atan2(nights[[k]]$u, nights[[k]]$v)
  strack<- sin(trackheadingR)#calculate sinus and cosinus of track and wind direction
  ctrack<- cos(trackheadingR)
  swind <- sin(winddirR)
  cwind <- cos(winddirR)
  
  xa <- (nights[[k]]$groundspeedms*strack)-(nights[[k]]$windspeedms*swind)
  ya <- (nights[[k]]$groundspeedms*ctrack)-(nights[[k]]$windspeedms*cwind)
  
  heading<- atan2(xa,ya)
  nights[[k]]$airspeedms<-sqrt((xa^2)+(ya^2)) 
  heading <- heading*(180/pi)#formula for conversion back to angles
  nights[[k]]$heading <- ifelse(heading<0, 360+heading, heading)
}
Spring_int <- nights[[1]]
Spring_low <- nights[[2]]
Autumn_int <- nights[[3]]
Autumn_low <- nights[[4]]

ggplot(Autumn_int,aes(x=winddir.y))+
  geom_histogram(breaks=seq(0,360,15))+
  coord_polar(start=0)+
  scale_x_continuous(" ",limits = c(0,360),breaks = seq(0,360,15))

# summarise the data based on date: avg values of heading, airspeed, track direction and groundspeed
mean_spring_int <- 
  Spring_int%>%
  group_by(n.date) %>%
  summarise(heading.m=mean(circular(heading,units = "degrees", modulo="2pi",template = 'geographic')),
            airspeed=mean(airspeedms), trackdir=mean(circular(trackheading,units = "degrees", modulo="2pi",
            template = 'geographic')),groundspeed=mean(groundspeedms),winddir=mean(circular(winddir.y,units = 
            "degrees", modulo="2pi",template = 'geographic')), windspeedms=mean(windspeedms))
mean_spring_low <- 
  Spring_low%>%
  group_by(n.date) %>%
  summarise(heading.m=mean(circular(heading,units = "degrees", modulo="2pi",template = 'geographic')),
            airspeed=mean(airspeedms), trackdir=mean(circular(trackheading,units = "degrees", modulo="2pi",
            template = 'geographic')),groundspeed=mean(groundspeedms), winddir=mean(circular(winddir.y,units = 
            "degrees", modulo="2pi",template = 'geographic')), windspeedms=mean(windspeedms))
mean_autumn_int <- 
  Autumn_int%>%
  group_by(n.date) %>%
  summarise(heading.m=mean(circular(heading,units = "degrees", modulo="2pi",template = 'geographic')),
            airspeed=mean(airspeedms), trackdir=mean(circular(trackheading,units = "degrees", modulo="2pi",
            template = 'geographic')),groundspeed=mean(groundspeedms),winddir=mean(circular(winddir.y,units = 
            "degrees", modulo="2pi",template = 'geographic')), windspeedms=mean(windspeedms))
mean_autumn_low <- 
  Autumn_low%>%
  group_by(n.date) %>%
  summarise(heading.m=mean(circular(heading,units = "degrees", modulo="2pi",template = 'geographic')),
            airspeed=mean(airspeedms), trackdir=mean(circular(trackheading,units = "degrees", modulo="2pi",
            template = 'geographic')),groundspeed=mean(groundspeedms),winddir=mean(circular(winddir.y,units = 
            "degrees", modulo="2pi",template = 'geographic')), windspeedms=mean(windspeedms))
#load the wind data from ERA5 (hourly values averaged per night and location over the North Sea)
weather1 <- readLines("ERA5_950hPa_U-V_EU_avgnight18-6UTC_07-10.txt")
weather1 <- readLines("ERA5_U-V_EU_avgnight18-6UTC_07-10.txt")
# remove unnecessary lines
weather1 <- weather1[-seq(1, length(weather1), 70)]
# make a table
weather1 <- read.table(text=weather1, header=F)
weather1 <- weather1[,4:8]
colnames(weather1)[colnames(weather1)==c("V4","V5","V6","V7","V8")] <- c("long","lat","date","u","v")
#turn date into a datetime object
weather1$timestamp <- as.POSIXct(as.character(weather1$date),tz="UTC",format="%d-%b-%Y %H")
weather1$date <- as.POSIXct(as.character(weather1$date),tz="UTC",format="%d-%b-%Y")
#calculate wind direction and wind speed from u and v components
weather1$windspeedms <-  sqrt((weather1$u * weather1$u) + (weather1$v * weather1$v))
#wind direction in radians
winddirR <- atan2(weather1$u, weather1$v)
#wind direction in deg
weather1$winddir <- winddirR*(180/pi)
#360 modulo
weather1$winddir <- ifelse(weather1$winddir<0, 360+weather1$winddir, weather1$winddir)
#calculate mean values per region per day
mean_weather <- 
  weather1%>%
  group_by(date) %>%
  summarise(winddir=mean(circular(winddir,units = "degrees", modulo="2pi",template = 'geographic')),
            windspeedms=mean(windspeedms))

colnames(mean_weather)[colnames(mean_weather)=="date"] <- "n.date"
mean_weather$n.date <- as.Date(mean_weather$n.date)
mean_spring_int <- mean_spring_int %>% left_join(mean_weather, by=c("n.date"))
mean_spring_low <- mean_spring_low %>% left_join(mean_weather, by=c("n.date"))
mean_autumn_int <- mean_autumn_int %>% left_join(mean_weather, by=c("n.date"))
mean_autumn_low <- mean_autumn_low %>% left_join(mean_weather, by=c("n.date"))

cHSint <- circular(mean_spring_int$heading.m,units = "degrees", modulo="2pi",
                       template = 'geographic')
cTDSint <- circular(mean_spring_int$trackdir,units = "degrees", modulo="2pi",
                       template = 'geographic')
cWDSint <- circular(mean_spring_int$winddir.x,units = "degrees", modulo="2pi",
                       template = 'geographic')
cHSlow <- circular(mean_spring_low$heading.m,units = "degrees", modulo="2pi",
                       template = 'geographic')
cTDSlow <- circular(mean_spring_low$trackdir,units = "degrees", modulo="2pi",
                       template = 'geographic')
cWDSlow <- circular(mean_spring_low$winddir.x,units = "degrees", modulo="2pi",
                       template = 'geographic')
cHAint <- circular(mean_autumn_int$heading.m,units = "degrees", modulo="2pi",
                       template = 'geographic')
cTDAint <- circular(mean_autumn_int$trackdir,units = "degrees", modulo="2pi",
                       template = 'geographic')
cWDAint <- circuar(mean_autumn_int$winddir.x,units = "degrees", modulo="2pi",
                       template = 'geographic')
cHAlow <- circular(mean_autumn_low$heading.m,units = "degrees", modulo="2pi",
                       template = 'geographic')
cTDAlow <- circular(mean_autumn_low$trackdir,units = "degrees", modulo="2pi",
                       template = 'geographic')
cWSAlow <- circular(mean_autumn_low$winddir.x,units = "degrees", modulo="2pi",
                       template = 'geographic')

all <- list(cHSint,cTDSint,cWDSint,cHSlow,cTDSlow,cWDSlow,cHAint,cTDAint,cWDAint,cHAlow,
            cTDAlow, cWSAlow)

means <- lapply(all,mean.circular)
ads <- lapply(all,angular.deviation)
rs <- lapply(all, rho.circular)
ray <- lapply(all,rayleigh.test)

table <- rbind.fill(as.data.frame(unlist(means)),as.data.frame(unlist(ads)),
                    as.data.frame(unlist(rs)),as.data.frame(unlist(ray)))

ggplot(mean_autumn_int,aes(heading.m))+
  geom_segment(aes(x= mean(circular(mean_autumn_int$heading.m,units = "degrees", modulo="2pi",
                                    template = 'geographic')),y=0,xend=mean(circular(mean_autumn_int$heading.m,units = "degrees", modulo="2pi",
                                                                                     template = 'geographic')),
                   yend=0.5),arrow = arrow(length = unit(0.3, "inches")),col="red")+
  geom_density(aes(y= ..scaled..),linetype=1)+
  geom_density(aes(mean_autumn_int$winddir,y= ..scaled..), col="grey",fill="grey")+
  geom_segment(aes(x= mean(circular(mean_autumn_int$winddir,units = "degrees", modulo="2pi",
                                    template = 'geographic')),y=0,xend=mean(circular(mean_autumn_int$winddir,units = "degrees", modulo="2pi",
                                                                                     template = 'geographic')),
                   yend=0.5),arrow = arrow(length = unit(0.3, "inches")),col="blue")+
  geom_density(aes(mean_autumn_int$trackdir,y= ..scaled..),linetype=2)+
  geom_segment(aes(x= mean(circular(mean_autumn_int$trackdir,units = "degrees", modulo="2pi",
                                    template = 'geographic')),y=0,xend=mean(circular(mean_autumn_int$trackdir,units = "degrees", modulo="2pi",
                                                                                     template = 'geographic')),
                   yend=0.5),arrow = arrow(length = unit(0.3, "inches")),col="green")+
  coord_polar(start = 0)+
  theme_bw()+
  theme(axis.title.y = element_blank(), axis.text.y=element_blank(),axis.ticks.y = element_blank())+
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

windows(7,7)

par(mfrow = c(2,2), mar= c(3, 4, 1, 1) + 0.1)

par(mfrow=c(2,2),oma=c(0,0,0,0))
par(mar = c(0.1,1,0.1,0.1))
plot(density.circular(circular(mean_spring_int$heading.m,units = "degrees", modulo="2pi",
                               template = 'geographic'),bw=75),shrink=1.9,ylab=NA,main=NA,xlab=NA,lwd=2)
lines(density.circular(circular(mean_spring_int$winddir.x,units = "degrees", modulo="2pi",
                                template = 'geographic'),bw=75),shrink=1.9,ylab=NA,main=NA,xlab=NA,lty=3,lwd=2)
lines(density.circular(circular(mean_spring_int$trackdir,units = "degrees", modulo="2pi",
                                template = 'geographic'),bw=75),shrink=1.9,ylab=NA,main=NA,xlab=NA,lty=2,lwd=2)
arrows.circular(mean(circular(mean_spring_int$heading.m,units = "degrees", modulo="2pi",
                              template = 'geographic')),col="red",lwd=3)
arrows.circular(mean(circular(mean_spring_int$winddir.x,units = "degrees", modulo="2pi",
                              template = 'geographic')),col="blue",lwd=3)
arrows.circular(mean(circular(mean_spring_int$trackdir,units = "degrees", modulo="2pi",
                              template = 'geographic')),col="forestgreen",lwd=3)
text(x=1.9,y=1.5,labels="(a)",cex=2)
par(mar = c(0.1,0.1,0.1,1))
plot(density.circular(circular(mean_spring_low$heading.m,units = "degrees", modulo="2pi",
                               template = 'geographic'),bw=75),shrink=1.9,ylab=NA,main=NA,xlab=NA,lwd=2)
lines(density.circular(circular(mean_spring_low$winddir.x,units = "degrees", modulo="2pi",
                                template = 'geographic'),bw=75),shrink=1.9,ylab=NA,main=NA,xlab=NA,lty=3,lwd=2)
lines(density.circular(circular(mean_spring_low$trackdir,units = "degrees", modulo="2pi",
                                template = 'geographic'),bw=75),shrink=1.9,ylab=NA,main=NA,xlab=NA,lty=2,lwd=2)
arrows.circular(mean(circular(mean_spring_low$heading.m,units = "degrees", modulo="2pi",
                              template = 'geographic')),col="red",lwd=3)
arrows.circular(mean(circular(mean_spring_low$winddir.x,units = "degrees", modulo="2pi",
                              template = 'geographic')),col="blue",lwd=3)
arrows.circular(mean(circular(mean_spring_low$trackdir,units = "degrees", modulo="2pi",
                              template = 'geographic')),col="forestgreen",lwd=3)
text(x=1.5,y=1.5,labels="(b)",cex = 2)
dev.off()

windows(7,4)

par(mfrow=c(1,2),oma=c(0,0,0,0))
par( mar = c(1,0.1,0.1,0.1))
plot(density.circular(circular(mean_autumn_int$heading.m,units = "degrees", modulo="2pi",
                               template = 'geographic'),bw=75),shrink=1.9,ylab=NA,main=NA,xlab=NA,lwd=2)
lines(density.circular(circular(mean_autumn_int$winddir.x,units = "degrees", modulo="2pi",
                                template = 'geographic'),bw=75),shrink=1.9,ylab=NA,main=NA,xlab=NA,lty=3,lwd=2)
lines(density.circular(circular(mean_autumn_int$trackdir,units = "degrees", modulo="2pi",
                                template = 'geographic'),bw=75),shrink=1.9,ylab=NA,main=NA,xlab=NA,lty=2,lwd=2)
arrows.circular(mean(circular(mean_autumn_int$heading.m,units = "degrees", modulo="2pi",
                              template = 'geographic')),col="red",lwd=3)
arrows.circular(mean(circular(mean_autumn_int$winddir.x,units = "degrees", modulo="2pi",
                              template = 'geographic')),col="blue",lwd=3)
arrows.circular(mean(circular(mean_autumn_int$trackdir,units = "degrees", modulo="2pi",
                              template = 'geographic')),col="forestgreen",lwd=3)
text(x=1.9,y=1.5,labels="(a)",cex=2)
par(mar = c(1,0.1,0,0))
plot(density.circular(circular(mean_autumn_low$heading.m,units = "degrees", modulo="2pi",
                               template = 'geographic'),bw=75),shrink=1.9,ylab=NA,main=NA,xlab=NA,lwd=2)
lines(density.circular(circular(mean_autumn_low$winddir.x,units = "degrees", modulo="2pi",
                                template = 'geographic'),bw=75),shrink=1.9,ylab=NA,main=NA,xlab=NA,lty=3,lwd=2)
lines(density.circular(circular(mean_autumn_low$trackdir,units = "degrees", modulo="2pi",
                                template = 'geographic'),bw=75),shrink=1.9,ylab=NA,main=NA,xlab=NA,lty=2,lwd=2)
arrows.circular(mean(circular(mean_autumn_low$heading.m,units = "degrees", modulo="2pi",
                              template = 'geographic')),col="red",lwd=3)
arrows.circular(mean(circular(mean_autumn_low$winddir.x,units = "degrees", modulo="2pi",
                              template = 'geographic')),col="blue",lwd=3)
arrows.circular(mean(circular(mean_autumn_low$trackdir,units = "degrees", modulo="2pi",
                              template = 'geographic')),col="forestgreen",lwd=3)
text(x=1.5,y=1.5,labels="(b)",cex = 2)
dev.off()
