######################################################################################################################
##############################CHOOSE NIGHTS WITH INTENSE MIGRATION FOR THE MODEL######################################
######################################################################################################################

#* data is collected by vertical radar only

library(foreign)
library(lubridate)
library(tidyverse)
library(suncalc)
library(xts)
library(dplyr)
library(imputeTS)
#read in the spss sheet with vertical radar data for all period
dataset<- read.spss( "C:\\Users\\mbradar\\Documents\\Merlin\\OWEZ\\data\\vertical\\OWEZ_X_tracks_2007-2010.sav", 
                     to.data.frame = T)
#set the time
Sys.setenv(TZ="UTC")
Sys.setlocale(category = "LC_ALL", locale = "English_United Kingdom.1252")#set the time on your computer to match
#add data and time columns to the dataset
dataset$date <- with(dataset, ymd(paste(Year,Month,Day, sep=' ')))
dataset$timestep <- with(dataset, ymd_h(paste(Year,Month,Day, Hour, sep= ' ')))
#subset per year and make a list for easier manipulation
fstyear <- subset(dataset,Year==2007)
syear <- subset(dataset,Year==2008)
tyear <- subset(dataset,Year==2009)
fyear <- subset(dataset,Year==2010)
Allyears <- list(fstyear,syear,tyear,fyear)
rm(list = c("fstyear","syear","tyear", "fyear"))#remove all objects you don't need
#calculate sunset and sunrise for all the tracks
sun <- list()

for (k in 1:length(Allyears)){
  sun[[k]]<- getSunlightTimes(unique(Allyears[[k]]$date),52.60636,4.389639,keep = c("sunrise","sunset"),tz="UTC")
  sun[[k]]$new.date <- as.Date(sun[[k]]$date)
  colnames(sun[[k]])[colnames(sun[[k]])==c("date","new.date")] <- c("timestep","date")
  sun[[k]]$sunrise <- sun[[k]]$sunrise-3600
}
#merge sunset and sunrise with the rest of the data
Join <- list()

Join <- lapply(1:4, function(n){
  Allyears[[n]] %>% left_join(sun[[n]], by=c("date"))
})
##turn calendar days into migration days (data from sunset of one day until the sunrise of a next day) 
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
#chose only nighttime migration (light==0)
Birds <- list()

for (k in 1:length(Join)){
  Birds[[k]]<- subset(Join[[k]],light==0)
}
#modify the list into a dataset that contains numbers of birds between sunset and sunrise 
#as recorded by the vertical radar
Allbirds <- do.call("rbind",Birds)
#calculate number of tracks per date
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
#find dates which fall within 95 quantile
nights1S=subset(countsS,Nr.tracks>=quantile(countsS$Nr.tracks,c(.95),type=8))
nights1A=subset(countsA,Nr.tracks>=quantile(countsA$Nr.tracks,c(.95),type=8))
#use dates calculated based on 95% to extract nights for the analysis from the horizontal radar
a <- c(nightsS$Date,nightsA$Date)
setwd('C:/Users/mbradar/Documents/Merlin/OWEZ/Model')
write.csv(a,file = 'nights.csv',row.names = FALSE )
#visualize number of tracks per night (ranked by intensity) for spring and autumn and draw a line for 95%
windows(6.5,2)
main.plot1 <- ggplot(countsS,aes(x=reorder(Date, -Nr.tracks), Nr.tracks))+
  geom_col(col="black",fill="cyan4",width = 1)+
  geom_vline(xintercept = which.min(abs(sort(countsS$Nr.tracks,decreasing=TRUE) - 
                                          quantile(countsS$Nr.tracks,0.95,type=8)))+0.5,col="red")+
  xlab("Nights") + ylab("Number of tracks") +
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
  xlab("Nights") + ylab("Number of tracks") +
  theme_minimal()+
  theme(axis.title.y = element_blank(), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=11), axis.text.x=element_blank(),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"),
        axis.ticks.x = element_blank(),panel.grid = element_blank())+
  scale_y_continuous(expand = c(0,0)) #force y-axis to start from 0
p=ggarrange(main.plot1,main.plot2,labels = c("(a)","(b)"),font.label = list(size = 14),ncol=2,nrow = 1,
            widths=c(1,1,1,1),common.legend = TRUE,legend ="right",label.x=c(0.8,.8))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
p=annotate_figure(p,bottom = text_grob("Nights",size=14),left = text_grob("Number of tracks",size=14,rot=90))
ggsave(filename=paste0("C:/Users/mbradar/Documents/check.png"),p,dpi=500)

######################################################################################################################
####################PREPARING HORIZONTAL RADAR DATA FOR BACK-TRAJECTORY MODEL#########################################
######################################################################################################################

#load the data
path <- setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/data/horizontal/bigger tables")
listcsv <- dir(path,pattern = "*.csv",ignore.case = FALSE)
#organize files per year for easier analysis and quicker load
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
#create timestamp and date in all the lists (in this case we have extra timestamp
#that includes minutes to decrease the ring effect in the model, but we also keep 
#the one with just hours to be able to merge the weather data)
Sys.setenv(TZ="UTC")
Sys.setlocale(category = "LC_ALL", locale = "English_United Kingdom.1252")#set the time on your computer to match
for(k in 1:length(listFstyear)){
  listFstyear[[k]]$date <- with(listFstyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listFstyear[[k]]$timestep.w <- with(listFstyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
  listFstyear[[k]]$timestep <- with(listFstyear[[k]], ymd_hm(paste(jaar,maand,dag, uur,minuut, sep= ' ')))
}
for(k in 1:length(listSyear)){
  listSyear[[k]]$date <- with(listSyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listSyear[[k]]$timestep.w <- with(listSyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
  listSyear[[k]]$timestep <- with(listSyear[[k]], ymd_hm(paste(jaar,maand,dag, uur,minuut, sep= ' ')))
}
for(k in 1:length(listTyear)){
  listTyear[[k]]$date <- with(listTyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listTyear[[k]]$timestep.w <- with(listTyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
  listTyear[[k]]$timestep <- with(listTyear[[k]], ymd_hm(paste(jaar,maand,dag, uur,minuut, sep= ' ')))
}
for(k in 1:length(listFyear)){
  listFyear[[k]]$date <- with(listFyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listFyear[[k]]$timestep.w <- with(listFyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
  listFyear[[k]]$timestep <- with(listFyear[[k]], ymd_hm(paste(jaar,maand,dag, uur,minuut, sep= ' ')))
}
#merge together all the months in a year
y2007 <- do.call("rbind", listFstyear)
y2008 <- do.call("rbind", listSyear)
y2009 <- do.call("rbind", listTyear)
y2010 <- do.call("rbind", listFyear)
#create a list for easier manipulation
Allyears <- list(y2007,y2008,y2009,y2010)
#clean up
rm(list = c("listFstyear","listFyear","listSyear", 
            "listTyear", "y2007", "y2008", "y2009","y2010"))#remove all the lists you don't need
#load weather data (from 1000 hPa pressure level to calculate headings and airspeeds of birds recorded by horiz. radar)
path <- setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/data/horizontal/ecmwf_weather")
Fstweather <- read.csv('Winds_radar_2007_pl1000_3h.csv',sep=',')
Sweather <- read.csv('Winds_radar_2008_pl1000_3h.csv',sep = ',')
Tweather <- read.csv('Winds_radar_2009_pl1000_3h.csv',sep = ',')
Fweather <- read.csv('Winds_radar_2010_pl1000_3h.csv',sep = ',')
#make a list for easier manipulation
weather <- list(Fstweather,Sweather,Tweather,Fweather)
#clean up
rm(list = c("Fstweather","Sweather","Tweather", "Fweather"))#remove all the lists you don't need
#change the name of the merging column, so it is the same in both data frames
for(k in 1:length(weather)){
  weather[[k]]$timestamp <- as.POSIXct(weather[[k]]$timestamp)
  colnames(weather[[k]])[colnames(weather[[k]])=="timestamp"] <- "timestep.w"
}
#merge bird data with corresponding weather data (rows that do not have a match will have missing values)
Allyears_ <- lapply(1:4, function(n){
  Allyears[[n]] %>% left_join(weather[[n]], by=c("timestep.w"))
})
#interpolate missing wind components in the data
for (k in 1:length(Allyears_)){
  Allyears_[[k]]$u_new<- na.interpolation(Allyears_[[k]]$u, option ="linear")
  Allyears_[[k]]$v_new<- na.interpolation(Allyears_[[k]]$v, option ="linear")
}
rm(list = c("weather","Allyears"))
##calculate heading and airspeed from track direction, groundspeed, wind direction and wind speed based on
#vector summation form Shamoun-Baranes et al.2007
for (k in 1:length(Allyears_)){
  Allyears_[[k]]$groundspeedms <- Allyears_[[k]]$groundspeedkph/3.6
  Allyears_[[k]]$windspeedms <-  sqrt((Allyears_[[k]]$u_new * Allyears_[[k]]$u_new) + (Allyears_[[k]]$v_new * Allyears_[[k]]$v_new))
  trackheadingR <- Allyears_[[k]]$trackheading*(pi/180)#formula for conversion to radians
  winddirR <- atan2(Allyears_[[k]]$u_new, Allyears_[[k]]$v_new)
  strack<- sin(trackheadingR)#calculate sinus and cosinus of track and wind direction
  ctrack <- cos(trackheadingR)
  swind <- sin(winddirR)
  cwind <- cos(winddirR)
  
  xa <- (Allyears_[[k]]$groundspeedms*strack)-(Allyears_[[k]]$windspeedms*swind)
  ya <- (Allyears_[[k]]$groundspeedms*ctrack)-(Allyears_[[k]]$windspeedms*cwind)
  
  heading<- atan2(xa,ya)
  Allyears_[[k]]$airspeedms<-sqrt((xa^2)+(ya^2)) 
  Allyears_[[k]]$r.heading <- heading*(180/pi)#formula for conversion back to angles
  Allyears_[[k]]$new.winddir <- winddirR*(180/pi)
  Allyears_[[k]]$new.winddir <- ifelse(Allyears_[[k]]$new.winddir<0, 360+Allyears_[[k]]$new.winddir, Allyears_[[k]]$new.winddir)
  
}
#make heading values into a 360 modulo
for(k in 1:length(Allyears_)){
  Allyears_[[k]]$b.heading <-  ifelse(Allyears_[[k]]$r.heading<0, 360+Allyears_[[k]]$r.heading, Allyears_[[k]]$r.heading)
}
#change calendar days into migration days 
for (k in 1:length(Allyears_)){
  s <- Allyears_[[k]]
  date <- paste0(format(s[1,15], format="%Y-%m-%d"))
  time <- "16:00:00"
  start.date <- ymd(date) + hms(time)
  breaks = seq(start.date - 366*3600*24, start.date + 366*3600*24, "1 days")
  Allyears_[[k]]$change = cut(Allyears_[[k]]$timestep, breaks=breaks)
  Allyears_[[k]]$n.year <- year(Allyears_[[k]]$change)
  Allyears_[[k]]$n.month <- month(Allyears_[[k]]$change)
  Allyears_[[k]]$n.day <- day(Allyears_[[k]]$change)
  Allyears_[[k]]$n.date <- with(Allyears_[[k]],ymd(paste(n.year,n.month,n.day,sep = ' ')))
  Allyears_[[k]]$migr.day <- with(Allyears_[[k]],ymd_hm(paste(n.year,n.month,n.day,uur,minuut,sep = ' ')))
  Allyears_[[k]] <- Allyears_[[k]] %>% arrange(timestep)
}

#filter based on light(to get only data between sunset and sunrise) and airspeed (5 m/s as a lower threshold to remove
#slow-moving targets, such as insects), and select only columns needed for further analysis
AllBirds <- list()

for (k in 1:length(Allyears_)){
  AllBirds[[k]]<- subset(Allyears_[[k]],light==0 & airspeedms>=5 & airspeedms<=30,
                         select=c(date,timestep,timestep.w,trackheading, groundspeedms, n.date,n.year,n.month,n.day,
                                  windspeedms,airspeedms,new.winddir,b.heading))
}
#subset data per season
#SPRING
spr <- list()
for(k in 1:length(AllBirds)){
  spr[[k]] <- subset(AllBirds[[k]],n.month>=2 & n.month<=5)
  spr[[k]] <- subset(spr[[k]],n.month>=2 & n.day>=15)
}
Spring <- do.call("rbind", spr)
#AUTUMN
aut <- list()
for(k in 1:length(AllBirds)){
  aut[[k]] <- subset(AllBirds[[k]],n.month>=8 & n.month<12)
}
Autumn <- do.call("rbind", aut)
#write out csvs for future analysis
setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/statistical_analysis/data")
write.csv(Spring, file = "Spring.csv")
write.csv(Autumn,file="Autumn.csv")
#clean up
rm(list=c("spr","Spring","s","aut","Autumn"))
#load the csv file that contains dates of the nights that should be analysed and choose only those nights from the
#horizontal radar data to include in the back-trajectory model
setwd('C:/Users/mbradar/Documents/Merlin/OWEZ/Model')
nights <- read.csv('nights.csv')
dates <- as.Date(nights$x)
AllBirds_1 <- do.call("rbind",AllBirds) 
analysis <- AllBirds_1[which(AllBirds_1$n.date %in% dates),]
#divide nights of intense migration per season
Spring_int <- subset(analysis,n.month>=2 & n.month<=5)
Autumn_int <- subset(analysis,n.month>=8 & n.month<12)
#save these subsets for further analysis
setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/statistical_analysis/data")
write.csv(Spring_int, file = "Spring_int.csv")
write.csv(Autumn_int,file="Autumn_int.csv")
#clean up
rm(list=c("AllBirds_1","Spring_int","Autumn_int"))
#make a list and filter based on sunrise and sunset times to use them in running a simulation (we previously used
#column light to roughly filter for this, but some tracks around sunset and sunrise ended up in this dataset)
Alldays <- split(analysis,analysis$n.date)
#remove rows with missing values
for (k in 1:length(Alldays)){
  Alldays[[k]] <- Alldays[[k]]%>% drop_na
}
#arrange data per timestamp
for (k in 1:length(Alldays)){
  Alldays[[k]] <- Alldays[[k]] %>% arrange(timestep.w)
}
#change the class of timestamp column
for (k in 1:length(Alldays)){
  Alldays[[k]]$timestep<- as.character(Alldays[[k]]$timestep)
  Alldays[[k]]$timestep.w<- as.character(Alldays[[k]]$timestep.w)
}
#calculate sunset and sunrise
sun <- list()
for (k in 1:length(Alldays)){
  sun[[k]]<- getSunlightTimes(unique(Alldays[[k]]$date),52.60636,4.389639,keep = c("sunrise","sunset"),tz="UTC")
  sun[[k]]$new.date <- as.Date(sun[[k]]$date)
  colnames(sun[[k]])[colnames(sun[[k]])==c("date","new.date")] <- c("timestep","date")
  sun[[k]]$sunrise <- sun[[k]]$sunrise-3600
}
#change the class of date column
for (k in 1:length(sun)){
  sun[[k]]$date <- as.character(sun[[k]]$date)
  Alldays[[k]]$date<- as.character(Alldays[[k]]$date)
}
#join bird data with sunset and sunrise data
Join <- lapply(1:22, function(n){
  Alldays[[n]] %>% left_join(sun[[n]], by=c("date"))
})
#filter out all the tracks that don't fall into time period between sunset and sunrise
Birds <- list()
for (k in 1:length(Join)){
  if (nrow(sun[[k]])==1){
    Birds[[k]] <- subset(Join[[k]],timestep.x>=sun[[k]][1,5]-86400 & timestep.x<=sun[[k]][1,4] 
                         & airspeedms>=5 & airspeedms<=30,select=c(date,timestep.x,trackheading, groundspeedms, n.year,n.month,n.day,
                                                                   windspeedms,airspeedms,new.winddir,b.heading))
  }else {
    Birds[[k]] <- subset(Join[[k]],timestep.x>=sun[[k]][1,5] & timestep.x<=sun[[k]][2,4] 
                         & airspeedms>=5 & airspeedms<=30,select=c(date,timestep.x,trackheading,
                                                                   groundspeedms, n.year,n.month,n.day,
                                                                   windspeedms,airspeedms,new.winddir,b.heading))
  }
  
}

#save the data
setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/radar_data/final_data")
#per day
library(lubridate)
for (k in 1:length(Birds)){
  s <- as.data.frame(Birds[[k]])
  datetime <- paste0(date(s[1,1]))
  write.csv(Birds[[k]], file = paste0("Birds_hr",datetime,".csv"))
}
#all data
fsv <- do.call(rbind.data.frame, Birds)
write.csv(fsv,file = "intense_migration_hr.csv", row.names = FALSE)

