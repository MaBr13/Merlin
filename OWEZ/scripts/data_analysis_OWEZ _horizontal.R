##################################################
#####Data analysis horizontal bird radar, OWEZ#####
##################################################
###Bradaric Maja, UvA, 17.07.2018

path <- setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/data/horizontal/bigger tables")
listcsv <- dir(path,pattern = "*.csv",ignore.case = FALSE)
#getting yearly data files
Fstyear <- listcsv[1:9]
Syear <- listcsv[10:21]
Tyear <- listcsv[22:28]
Fyear <- listcsv[29:31]
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
 
  
  Allyears[[k]]$new.winddir <- Allyears[[k]]$winddir+180  
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

############################################################
##########making table with means per hour##################


                                                                                                   
counts <- list()
mean.grspeed <- list()
mean.aspeed <- list()
mean.wspeed <- list()
mean.heading <- list()
mean.winddir <- list()
date <- list()
season <- list()
light <- list()
dayP <- list()
g <- list()
s <- list()
p <- list()
a <- list()
d <- list()
l <- list()
b <- list()
e <- list()
f <- list()
means <- list()
month <- list()

for(k in 1:length(Allyears)){
  counts[[k]] <- aggregate(Allyears[[k]]$id,by = list(Allyears[[k]]$timestep), FUN="length")
  mean.grspeed[[k]] <- aggregate(Allyears[[k]]$groundspeedms, by = list(Allyears[[k]]$timestep), FUN="mean")
  mean.aspeed[[k]] <- aggregate(Allyears[[k]]$airspeedms, by=list(Allyears[[k]]$timestep), FUN="mean")
  mean.wspeed[[k]] <- aggregate(Allyears[[k]]$windspeedms, by=list(Allyears[[k]]$timestep), FUN="mean")
  mean.heading[[k]] <- aggregate(Allyears[[k]]$trackheading, by=list(Allyears[[k]]$timestep), FUN="mean")
  mean.winddir[[k]] <- aggregate(Allyears[[k]]$new.winddir, by=list(Allyears[[k]]$timestep), FUN="mean")
  date[[k]]<- aggregate(Allyears[[k]]$date, by=list(Allyears[[k]]$timestep), FUN="median")
  month[[k]] <- aggregate(Allyears[[k]]$maand, by=list(Allyears[[k]]$timestep), FUN="median")
  names(month[[k]])[c(1,2)] <- paste(c("Timestamp","Month"))
  season[[k]] <- aggregate(Allyears[[k]]$season, by=list(Allyears[[k]]$timestep), FUN="median")
  names(season[[k]])[c(1,2)] <- paste(c("Timestamp","Season"))
  light[[k]] <- aggregate(Allyears[[k]]$light, by=list(Allyears[[k]]$timestep), FUN="median")
  names(light[[k]])[c(1,2)] <- paste(c("Timestamp","Light"))
  dayP[[k]] <- aggregate(Allyears[[k]]$dawndusk, by=list(Allyears[[k]]$timestep), FUN="median")
  names(dayP[[k]])[c(1,2)] <- paste(c("Timestamp","DayP"))
  g[[k]] <- merge(counts[[k]], mean.grspeed[[k]], by="Group.1", sort = TRUE) #TABLE FOR ANALYSIS
  names(g[[k]])[c(1,2,3)] <- paste(c("Group.1","Nr.tracks", "Mean.speed"))
  s[[k]] <- merge(mean.aspeed[[k]],mean.wspeed[[k]],by="Group.1", sort=TRUE)
  names(s[[k]])[c(1,2,3)] <- paste(c("Timestamp","Mean.aspeed", "Mean.wspeed"))
  p[[k]] <- merge(mean.heading[[k]],mean.winddir[[k]],by="Group.1", sort=TRUE)
  names(p[[k]])[c(1,2,3)] <- paste(c("Timestamp","Mean.head", "Mean.wdir"))
  a[[k]]<- merge(g[[k]], date[[k]], by="Group.1", sort = TRUE)
  names(a[[k]])[c(1,2,3,4)]<-paste(c("Timestamp","Nr.tracks", "Mean.speed", "Date"))
  d[[k]] <- merge(s[[k]],p[[k]],by="Timestamp", sort = TRUE )
  l [[k]] <- merge(a[[k]],d[[k]],by="Timestamp", sort=TRUE)
  b[[k]] <- merge(dayP[[k]],month[[k]], by="Timestamp", sort=TRUE)
  e[[k]] <- merge(light[[k]],season[[k]],by="Timestamp", sort=TRUE)
  f[[k]] <- merge(b[[k]],l[[k]], by="Timestamp", sort=TRUE)
  means[[k]] <- merge(f[[k]],e[[k]], by="Timestamp", sort=TRUE)
}
rm(list =c("counts","mean.grspeed", "mean.aspeed", "mean.wspeed","mean.heading" ,"mean.winddir","date", 
           "season","light","dayP","g", "s" ,"p" ,"a" ,"d","l" ,"b" ,"e" ,"f" ,"month")) 

cut(capture.output(print(means),file="means.csv"))#if you want to save the table as csv
##########################################################
#############Manipulations with means table###############

#categories according to air speed
for(k in 1:length(means)){
  
  means[[k]]$Aspeed<- cut(means[[k]]$Mean.aspeed,breaks=c(0,12,15,18,27,45), 
                              labels = c("0-12","12-15","15-18", "18-27", ">27"))
  
}
#categories wind speed
for(k in 1:length(means)){
  
  means[[k]]$Wspeed<- cut(means[[k]]$Mean.wspeed,breaks=c(0,10,15,20,30,40,50), 
                          labels = c("0-10","10-15","15-20","20-30", "30-40", ">40"))
  
}

  

#mark NAs with a category
library(dplyr)   # gives mutate_if
library(forcats) # gives fct_explicit_na
library(magrittr) # for piping

for(k in 1:length(means)){
  means[[k]] %<>% mutate(Aspeed = fct_explicit_na(means[[k]]$Aspeed, na_level = "Data n/a")) 
}

for(k in 1:length(means)){
  means[[k]] %<>% mutate(Wspeed = fct_explicit_na(means[[k]]$Wspeed, na_level = "Data n/a")) 
}



##fix the values of light

for(k in 1:length(means)){
  
  means[[k]]$Rlight <- cut(means[[k]]$Light,breaks=c(-1,0.6,1), 
                               labels = c("0","1"))}


##########################################################
#############manipulations with Allyears table###############

#categories according to air speed
for(k in 1:length(Allyears)){
  
  Allyears[[k]]$Aspeed<- cut(Allyears[[k]]$airspeedms,breaks=c(0,12,15,18,27,45), 
                          labels = c("0-12","12-15","15-18", "18-27", ">27"))
  
}
#categories wind speed
for(k in 1:length(Allyears)){
  
  Allyears[[k]]$Wspeed<- cut(Allyears[[k]]$windspeedms,breaks=c(0,10,15,20,30,40,50), 
                          labels = c("0-10","10-15","15-20","20-30", "30-40", ">40"))
  
}

#mark NAs with a category
library(dplyr)   # gives mutate_if
library(forcats) # gives fct_explicit_na
library(magrittr) # for piping

for(k in 1:length(Allyears)){
  Allyears[[k]] %<>% mutate(Aspeed = fct_explicit_na(Allyears[[k]]$Aspeed, na_level = "Data n/a"))
  Allyears[[k]] %<>% mutate(Wspeed = fct_explicit_na(Allyears[[k]]$Wspeed, na_level = "Data n/a"))
}

####dividing data by season
#mean values
Spring <- list()
for(k in 1:length(means)){
  
  Spring[[k]] <- subset(means[[k]], Season==2, select=Timestamp:Rlight)
}

Autumn <- list()

for(k in 1:length(means)){
  
  Autumn[[k]] <- subset(means[[k]], Season==4, select=Timestamp:Rlight)
}
#total values
SpringAll <- list()
for(k in 1:length(Allyears)){
  
  SpringAll[[k]] <- subset(Allyears[[k]], Season==2, select=id:Wspeed)
}

AutumnAll <- list()

for(k in 1:length(Allyears)){
  
  AutumnAll[[k]] <- subset(Allyears[[k]], Season==4, select=id:Wspeed)
}




