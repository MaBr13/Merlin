##################################################
#####Data analysis horizontal bird radar, OWEZ#####
##################################################
###Bradaric Maja, UvA, 17.07.2018

path <- setwd("C:/Users/mbradar/Documents/MERLIN/OWEZ/data/horizontal/bigger tables")
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

library(lubridate)
Sys.setenv(TZ="UTC")
Sys.setlocale(category = "LC_ALL", locale = "English_United Kingdom.1252")#set the time on your computer to match

library(dplyr)

for(k in 1:length(listFstyear)){
  listFstyear[[k]]$date <- with(listFstyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listFstyear[[k]]$timestep <- with(listFstyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
  listFstyear[[k]] <- listFstyear[[k]] %>% arrange(timestep)
}
for(k in 1:length(listSyear)){
  listSyear[[k]]$date <- with(listSyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listSyear[[k]]$timestep <- with(listSyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
  listSyear[[k]] <- listSyear[[k]] %>% arrange(timestep)
}
for(k in 1:length(listTyear)){
  listTyear[[k]]$date <- with(listTyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listTyear[[k]]$timestep <- with(listTyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
  listTyear[[k]] <- listTyear[[k]] %>% arrange(timestep)
}
for(k in 1:length(listFyear)){
  listFyear[[k]]$date <- with(listFyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listFyear[[k]]$timestep <- with(listFyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
  listFyear[[k]] <- listFyear[[k]] %>% arrange(timestep)
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
  Allyears[[k]] <- Allyears[[k]] %>% arrange(timestep)
}

##########################################################
#############manipulations with Allyears table###############

#categories according to air speed
for(k in 1:length(Allyears)){
  
  Allyears[[k]]$Aspeed<- cut(Allyears[[k]]$airspeedms,breaks=c(0,12,15,18,27,45), 
                             labels = c("0-12","12-15","15-18", "18-27", ">27"))
  
}
#categories wind speed
for(k in 1:length(Allyears)){
  
  Allyears[[k]]$Wspeed<- cut(Allyears[[k]]$windspeedms,breaks=c(0,5,10,15,20,25,30,35,40,50), 
                             labels = c("0-5","5-10","10-15","15-20","20-25","25-30","30-35", "35-40", ">40"))
  
}

#mark NAs with a category
library(dplyr)   # gives mutate_if
library(forcats) # gives fct_explicit_na
library(magrittr) # for piping

for(k in 1:length(Allyears)){
  Allyears[[k]] %<>% mutate(Aspeed = fct_explicit_na(Allyears[[k]]$Aspeed, na_level = "Data n/a"))
  Allyears[[k]] %<>% mutate(Wspeed = fct_explicit_na(Allyears[[k]]$Wspeed, na_level = "Data n/a"))
}

#total values
SpringAll <- list()
for(k in 1:length(Allyears)){
  
  SpringAll[[k]] <- subset(Allyears[[k]], season==2, select=id:Wspeed)
}

AutumnAll <- list()

for(k in 1:length(Allyears)){
  
  AutumnAll[[k]] <- subset(Allyears[[k]], season==4, select=id:Wspeed)
}

for (k in 1:length(SpringAll)){
  SpringAll[[k]]$Aspeed <- factor(SpringAll[[k]]$Aspeed, levels = rev(levels(SpringAll[[k]]$Aspeed)))
  
}

for (k in 1:length(AutumnAll)){
  AutumnAll[[k]]$Aspeed <- factor(AutumnAll[[k]]$Aspeed, levels = rev(levels(AutumnAll[[k]]$Aspeed)))
  
}

