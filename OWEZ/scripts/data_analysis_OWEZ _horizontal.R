##################################################
#####Data anlysis horizontal bird radar, OWEZ#####
##################################################
###Bradaric Maja, UvA, 17.07.2018

path <- setwd("C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/data/horizontal/bigger tables")
listcsv <- dir(path,pattern = "*.csv",ignore.case = FALSE)
#getting yearly data files
Fstyear <- listcsv[1:9]
Syear <- listcsv[10:21]
Tyear <- listcsv[22:28]
Fyear <- listcsv[29:31]
##including only columns of interest##
#in one file
Monthdata <- read.csv("trackplusinfo200707.csv",sep=';')[,c("trackid", "jaar","maand", "dag", "uur","minuut",
                                                            "season", "light","trackheading","groundspeedkph", 
                                                            "airspeedkph", "windspeedkph","winddir")]
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
  mean.grspeed[[k]] <- aggregate(Allyears[[k]]$groundspeedkph, by = list(Allyears[[k]]$timestep), FUN="mean")
  mean.aspeed[[k]] <- aggregate(Allyears[[k]]$airspeedkph, by=list(Allyears[[k]]$timestep), FUN="mean")
  mean.wspeed[[k]] <- aggregate(Allyears[[k]]$windspeedkph, by=list(Allyears[[k]]$timestep), FUN="mean")
  mean.heading[[k]] <- aggregate(Allyears[[k]]$trackheading, by=list(Allyears[[k]]$timestep), FUN="mean")
  mean.winddir[[k]] <- aggregate(Allyears[[k]]$winddir, by=list(Allyears[[k]]$timestep), FUN="mean")
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

cut(capture.output(print(means),file="means.csv"))
##########################################################
#############manipulations with means table###############

#categories according to air speed
for(k in 1:length(means)){
  
  means[[k]]$Aspeed<- cut(means[[k]]$Mean.aspeed,breaks=c(0,50,100,150), 
                              labels = c("0-50", "50-100", ">100"))
  
}
#categories wind speed
for(k in 1:length(means)){
  
  means[[k]]$Wspeed<- cut(means[[k]]$Mean.wspeed,breaks=c(0,50,100,150), 
                          labels = c("0-50", "50-100", ">100"))
  
}
#categories direction
#Autumn
for(k in 1:length(means)){
  
  means[[k]]$DirectionA <- cut(means[[k]]$Mean.head,breaks=c(0,200,250,290,360), 
                          labels = c("0-200", "200-250", "250-290","290-360"))
  
}
#spring
for(k in 1:length(means)){
  
  means[[k]]$DirectionS <- cut(means[[k]]$Mean.head,breaks=c(0,30,70,120,360), 
                               labels = c("0-30", "30-70", "70-120","120-360"))
  
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

for(k in 1:length(means)){
  means[[k]] %<>% mutate(DirectionA = fct_explicit_na(means[[k]]$DirectionA, na_level = "Data n/a")) 
}



##fix the values of light

for(k in 1:length(means)){
  
  means[[k]]$Rlight <- cut(means[[k]]$Light,breaks=c(-1,0.6,1), 
                               labels = c("0","1"))}


##########################################################
#############manipulations with Allyears table###############

#categories according to air speed
for(k in 1:length(Allyears)){
  
  Allyears[[k]]$Aspeed<- cut(Allyears[[k]]$airspeedkph,breaks=c(0,50,100,150), 
                          labels = c("0-50", "50-100", ">100"))
  
}
#categories wind speed
for(k in 1:length(Allyears)){
  
  Allyears[[k]]$Wspeed<- cut(Allyears[[k]]$windspeedkph,breaks=c(0,50,100,150), 
                          labels = c("0-50", "50-100", ">100"))
  
}
#categories direction
#Autumn
for(k in 1:length(Allyears)){
  
  Allyears[[k]]$DirectionA <- cut(Allyears[[k]]$trackheading,breaks=c(0,200,250,290,360), 
                               labels = c("0-200", "200-250", "250-290","290-360"))
  
}
#spring
for(k in 1:length(Allyears)){
  
  Allyears[[k]]$DirectionS <- cut(Allyears[[k]]$trackheading,breaks=c(0,30,70,120,360), 
                               labels = c("0-30", "30-70", "70-120","120-360"))
  
}
#mark NAs with a category
library(dplyr)   # gives mutate_if
library(forcats) # gives fct_explicit_na
library(magrittr) # for piping

for(k in 1:length(Allyears)){
  Allyears[[k]] %<>% mutate(Aspeed = fct_explicit_na(Allyears[[k]]$Aspeed, na_level = "Data n/a")) 
}

for(k in 1:length(Allyears)){
  Allyears[[k]] %<>% mutate(Wspeed = fct_explicit_na(Allyears[[k]]$Wspeed, na_level = "Data n/a")) 
}

for(k in 1:length(Allyears)){
  Allyears[[k]] %<>% mutate(DirectionA = fct_explicit_na(Allyears[[k]]$DirectionA, na_level = "Data n/a")) 
}

for(k in 1:length(Allyears)){
  Allyears[[k]] %<>% mutate(DirectionS = fct_explicit_na(Allyears[[k]]$DirectionS, na_level = "Data n/a")) 
}

