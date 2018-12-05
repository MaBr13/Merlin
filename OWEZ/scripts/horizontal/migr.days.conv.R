##################################################
#####Data analysis horizontal bird radar, OWEZ#####
##################################################
###Bradaric Maja, UvA, 17.07.2018

path <- setwd("C:/Users/Maja/Documents/OWEZ data/horizontal")
listcsv <- dir(path,pattern = "*.csv",ignore.case = FALSE)
#getting yearly data files
Fstyear <- listcsv[1:7]
Syear <- listcsv[8:19]
Tyear <- listcsv[19:25]
Fyear <- listcsv[26:29]
##including only columns of interest##
#in one file
Monthdata <- read.csv("trackplusinfo200707.csv",sep=';')#[,c("trackid", "jaar","maand", "dag", "uur","minuut",
#   "season", "light","trackheading","groundspeedkph", 
#  "airspeedkph", "windspeedkph","winddir")]
#in multiple files
#1st year
nc <- ncol(read.csv(Fstyear[1], sep=';',header = TRUE, nrows=1))#determine number of total columns
colClasses <- replace(rep("NULL", nc),c(1,8:12,14:15,17,20:22,26,29),NA)#determine which columns to use
#(NA for those that will be used)
listFstyear <- lapply(Fstyear,read.csv,sep=';',header= TRUE, colClasses=colClasses)#apply function to all 
#datasets from
#a specific year
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
y2007 <- do.call("rbind", listFstyear)
y2008 <- do.call("rbind", listSyear)
y2009 <- do.call("rbind", listTyear)
y2010 <- do.call("rbind", listFyear)
#create values to use for plots on a yearly basis to visualize tracks and groundspeed
library(xts)
Allyears <- list(y2007,y2008,y2009,y2010)
rm(list = c("listFstyear","listFyear","listSyear", 
            "listTyear", "y2007", "y2008", "y2009","y2010"))#remove all the lists you don't need


############################################################
############manipulations with Allyears table###############

##calculating wind direction (direction where the wind blows to)
##winddir column represents wind direction wind blows from, and new.winddir wind direction where wind blows to

for (k in 1:length(Allyears)){
  
  
  Allyears[[k]]$new.winddir <- ifelse(Allyears[[k]]$winddir<180.0001,Allyears[[k]]$winddir+180,Allyears[[k]]$winddir-180)  
}

##calculating heading from track direction, groundspeed, wind direction and wind speed

for (k in 1:length(Allyears)){
  
  Allyears[[k]]$groundspeedms <- Allyears[[k]]$groundspeedkph/3.6
  Allyears[[k]]$windspeedms <- Allyears[[k]]$windspeedkph/3.6
  trackheadingR <- Allyears[[k]]$trackheading*(pi/180)#formula for conversion to radians
  winddirR <- Allyears[[k]]$new.winddir*(pi/180)
  strack<- sin(trackheadingR)#calculate sinus and cosinus of track and wind direction
  ctrack <- cos(trackheadingR)
  swind <- sin(winddirR)
  cwind <- cos(winddirR)
  
  xa <- (Allyears[[k]]$groundspeedms*strack)-(Allyears[[k]]$windspeedms*swind)
  ya <- (Allyears[[k]]$groundspeedms*ctrack)-(Allyears[[k]]$windspeedms*cwind)
  
  heading<- atan2(xa,ya)
  Allyears[[k]]$airspeedms<-sqrt((xa^2)+(ya^2)) 
  Allyears[[k]]$r.heading <- heading*(180/pi)#formula for conversion back to angles
  
}

#Getting rid of negative values in heading

for(k in 1:length(Allyears)){
  Allyears[[k]]$b.heading <-  ifelse(Allyears[[k]]$r.heading<0, 360+Allyears[[k]]$r.heading, Allyears[[k]]$r.heading)
}

#creating migration days
  
library(lubridate)

for (k in 1:length(Allyears)){
  s <- Allyears[[k]]
  date <- paste0(format(s[1,16], format="%Y-%m-%d"))
  time <- "16:00:00"
  start.date <- ymd(date) + hms(time)
  breaks = seq(start.date - 366*3600*24, start.date + 366*3600*24, "1 days")
  Allyears[[k]]$change = cut(Allyears[[k]]$timestep, breaks=breaks)
  Allyears[[k]]$n.year <- year(Allyears[[k]]$change)
  Allyears[[k]]$n.month <- month(Allyears[[k]]$change)
  Allyears[[k]]$n.day <- day(Allyears[[k]]$change)
  Allyears[[k]]$n.date <- with(Allyears[[k]],ymd(paste(n.year,n.month,n.day,sep = ' ')))
  Allyears[[k]]$migr.day <- with(Allyears[[k]],ymd_h(paste(n.year,n.month,n.day,uur,sep = ' '))) 
}


AllyearsN <- list()

for (k in 1:length(Allyears)){
  AllyearsN[[k]] <- subset(Allyears[[k]],light==1,select=id:Wspeed)
}

counts1 <- list()
mean.grspeed1 <- list()
mean.aspeed1 <- list()
mean.wspeed1 <- list()
mean.tr.dir1 <- list()
mean.heading1 <- list()
mean.winddir1 <- list()
date1 <- list()
season1 <- list()
light1 <- list()
dayP1 <- list()
g1 <- list()
s1 <- list()
p1 <- list()
a1 <- list()
d1 <- list()
l1 <- list()
b1 <- list()
e1 <- list()
f1 <- list()
r1 <- list()
means.migr <- list()
month1 <- list()

for(k in 1:length(AllyearsN)){
  counts1[[k]] <- aggregate(AllyearsN[[k]]$id,by = list(AllyearsN[[k]]$n.date), FUN="length")
  mean.grspeed1[[k]] <- aggregate(AllyearsN[[k]]$groundspeedms, by = list(AllyearsN[[k]]$n.date), FUN="mean")
  mean.aspeed1[[k]] <- aggregate(AllyearsN[[k]]$airspeedms, by=list(AllyearsN[[k]]$n.date), FUN="mean")
  mean.wspeed1[[k]] <- aggregate(AllyearsN[[k]]$windspeedms, by=list(AllyearsN[[k]]$n.date), FUN="mean")
  mean.tr.dir1[[k]] <- aggregate(AllyearsN[[k]]$trackheading, by=list(AllyearsN[[k]]$n.date), FUN="mean")
  mean.heading1[[k]] <- aggregate(AllyearsN[[k]]$b.heading, by=list(AllyearsN[[k]]$n.date), FUN="mean")
  mean.winddir1[[k]] <- aggregate(AllyearsN[[k]]$new.winddir, by=list(AllyearsN[[k]]$n.date), FUN="mean")
  date1[[k]]<- aggregate(AllyearsN[[k]]$n.date, by=list(AllyearsN[[k]]$n.date), FUN="median")
  month1[[k]] <- aggregate(AllyearsN[[k]]$n.month, by=list(AllyearsN[[k]]$n.date), FUN="median")
  names(month1[[k]])[c(1,2)] <- paste(c("Timestamp","Month"))
  season1[[k]] <- aggregate(AllyearsN[[k]]$season, by=list(AllyearsN[[k]]$n.date), FUN="median")
  names(season1[[k]])[c(1,2)] <- paste(c("Timestamp","Season"))
  light1[[k]] <- aggregate(AllyearsN[[k]]$light, by=list(AllyearsN[[k]]$n.date), FUN="median")
  names(light1[[k]])[c(1,2)] <- paste(c("Timestamp","Light"))
  dayP1[[k]] <- aggregate(AllyearsN[[k]]$dawndusk, by=list(AllyearsN[[k]]$n.date), FUN="median")
  names(dayP1[[k]])[c(1,2)] <- paste(c("Timestamp","DayP"))
  g1[[k]] <- merge(counts1[[k]], mean.grspeed1[[k]], by="Group.1", sort = TRUE) #TABLE FOR ANALYSIS
  names(g1[[k]])[c(1,2,3)] <- paste(c("Group.1","Nr.tracks", "Mean.speed"))
  s1[[k]] <- merge(mean.aspeed1[[k]],mean.wspeed1[[k]],by="Group.1", sort=TRUE)
  names(s1[[k]])[c(1,2,3)] <- paste(c("Timestamp","Mean.aspeed", "Mean.wspeed"))
  p1[[k]] <- merge(mean.heading1[[k]],mean.winddir1[[k]],by="Group.1", sort=TRUE)
  r1[[k]] <- merge(p1[[k]],mean.tr.dir1[[k]],by="Group.1",sort=TRUE)
  names(r1[[k]])[c(1,2,3,4)] <- paste(c("Timestamp","Mean.head", "Mean.wdir", "Mean.tr.dir"))
  a1[[k]]<- merge(g1[[k]], date1[[k]], by="Group.1", sort = TRUE)
  names(a1[[k]])[c(1,2,3,4)]<-paste(c("Timestamp","Nr.tracks", "Mean.speed", "Date"))
  d1[[k]] <- merge(s1[[k]],r1[[k]],by="Timestamp", sort = TRUE )
  l1 [[k]] <- merge(a1[[k]],d1[[k]],by="Timestamp", sort=TRUE)
  b1[[k]] <- merge(dayP1[[k]],month1[[k]], by="Timestamp", sort=TRUE)
  e1[[k]] <- merge(light1[[k]],season1[[k]],by="Timestamp", sort=TRUE)
  f1[[k]] <- merge(b1[[k]],l1[[k]], by="Timestamp", sort=TRUE)
  means.migr[[k]] <- merge(f1[[k]],e1[[k]], by="Timestamp", sort=TRUE)
}
rm(list =c("counts1","mean.grspeed1", "mean.aspeed1", "mean.wspeed1","mean.heading1" ,"mean.tr.dir1","mean.winddir1","date1", 
           "season1","light1","dayP1","g1", "s1" ,"p1" ,"a1" ,"d1","l1" ,"r1","b1" ,"e1" ,"f1" ,"month1")) 

#categories according to air speed
for(k in 1:length(means.migr)){
  
  means.migr[[k]]$Aspeed<- cut(means.migr[[k]]$Mean.aspeed,breaks=c(0,12,15,18,27,45), 
                          labels = c("0-12","12-15","15-18", "18-27", ">27"))
  
}
#categories wind speed
for(k in 1:length(means.migr)){
  
  means.migr[[k]]$Wspeed<- cut(means.migr[[k]]$Mean.wspeed,breaks=c(0,10,15,20,30,40,50), 
                          labels = c("0-10","10-15","15-20","20-30", "30-40", ">40"))
  
}



#mark NAs with a category
library(dplyr)   # gives mutate_if
library(forcats) # gives fct_explicit_na
library(magrittr) # for piping


for(k in 1:length(means.migr)){
  means.migr[[k]] %<>% mutate(Aspeed = fct_explicit_na(means.migr[[k]]$Aspeed, na_level = "Data n/a")) 
}

for(k in 1:length(means.migr)){
  means.migr[[k]] %<>% mutate(Wspeed = fct_explicit_na(means.migr[[k]]$Wspeed, na_level = "Data n/a")) 
}



##fix the values of light

for(k in 1:length(means.migr)){
  
  means.migr[[k]]$Rlight <- cut(means.migr[[k]]$Light,breaks=c(-1,0.6,1), 
                           labels = c("0","1"))}


##########################################################
#############manipulations with AllyearsN table###############

#categories according to air speed
for(k in 1:length(AllyearsN)){
  
  AllyearsN[[k]]$Aspeed<- cut(AllyearsN[[k]]$airspeedms,breaks=c(0,12,15,18,27,45), 
                             labels = c("0-12","12-15","15-18", "18-27", ">27"))
  
}
#categories wind speed
for(k in 1:length(AllyearsN)){
  
  AllyearsN[[k]]$Wspeed<- cut(AllyearsN[[k]]$windspeedms,breaks=c(0,5,10,15,20,25,30,35,40,50), 
                             labels = c("0-5","5-10","10-15","15-20","20-25","25-30","30-35", "35-40", ">40"))
  
}

#mark NAs with a category
library(dplyr)   # gives mutate_if
library(forcats) # gives fct_explicit_na
library(magrittr) # for piping

for(k in 1:length(AllyearsN)){
  AllyearsN[[k]] %<>% mutate(Aspeed = fct_explicit_na(AllyearsN[[k]]$Aspeed, na_level = "Data n/a"))
  AllyearsN[[k]] %<>% mutate(Wspeed = fct_explicit_na(AllyearsN[[k]]$Wspeed, na_level = "Data n/a"))
}

####dividing data by season
#mean values
Spring <- list()
for(k in 1:length(means.migr)){
  
  Spring[[k]] <- subset(means.migr[[k]], Season==2, select=Timestamp:Rlight)
}

Autumn <- list()

for(k in 1:length(means.migr)){
  
  Autumn[[k]] <- subset(means.migr[[k]], Season==4, select=Timestamp:Rlight)
}
#total values
SpringAll <- list()
for(k in 1:length(AllyearsN)){
  
  SpringAll[[k]] <- subset(AllyearsN[[k]], season==2, select=id:Wspeed)
}

AutumnAll <- list()

for(k in 1:length(AllyearsN)){
  
  AutumnAll[[k]] <- subset(AllyearsN[[k]], season==4, select=id:Wspeed)
}
