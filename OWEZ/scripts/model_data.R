###GETTING THE DATA FOR THE MODEL


path <- setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/data/horizontal/bigger tables")
listcsv <- dir(path,pattern = "*.csv",ignore.case = FALSE)
#getting yearly data files
Syear <- listcsv[10:21]
Tyear <- listcsv[22:28]
Fyear <- listcsv[29:31]

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
y2008 <- do.call("rbind", listSyear)
y2009 <- do.call("rbind", listTyear)
y2010 <- do.call("rbind", listFyear)
#create values to use for plots on a yearly basis to visualize tracks and groundspeed
library(xts)
Allyears <- list(y2008,y2009,y2010)
rm(list = c("listFstyear","listFyear","listSyear", 
            "listTyear", "y2007", "y2008", "y2009","y2010"))#remove all the lists you don't need

#merge weather data and bird data


path <- setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/data/horizontal/ecmwf_weather")
Fweather <- read.csv('Winds_radar_2008.csv',sep = ',')
Sweather <- read.csv('Winds_radar_2009.csv',sep = ',')
Tweather <- read.csv('Winds_radar_2010.csv',sep = ',')

weather <- list(Fweather,Sweather,Tweather)

for(k in 1:length(weather)){
  weather[[k]]$timestamp <- as.POSIXct(weather[[k]]$timestamp)
  colnames(weather[[k]])[colnames(weather[[k]])=="timestamp"] <- "timestep"
}


library(dplyr)
Allyears_ <- lapply(1:3, function(n){
    Allyears[[n]] %>% left_join(weather[[n]], by=c("timestep"))
   })


library(imputeTS)
for (k in 1:length(Allyears_)){
  Allyears_[[k]]$u_new<- na.interpolation(Allyears_[[k]]$u, option ="linear")
  Allyears_[[k]]$v_new<- na.interpolation(Allyears_[[k]]$v, option ="linear")
}


############################################################
############manipulations with Allyears table###############


##calculating heading from track direction, groundspeed, wind direction and wind speed

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


#Getting rid of negative values in heading

for(k in 1:length(Allyears_)){
  Allyears_[[k]]$b.heading <-  ifelse(Allyears_[[k]]$r.heading<0, 360+Allyears_[[k]]$r.heading, Allyears_[[k]]$r.heading)
}


Sys.setenv(TZ="UTC")
Sys.setlocale(category = "LC_ALL", locale = "English_United Kingdom.1252")#set the time on your computer to match



Oct6a <- subset(Allyears_[[1]], timestep>="2008-10-28 16:00:00" & timestep<="2008-10-29 16:00:00", select = c(timestep,trackheading,windspeedms,b.heading,airspeedms,date,new.winddir))
Oct7a <- subset(Allyears_[[1]], timestep>="2008-10-29 16:00:00" & timestep<="2008-10-30 16:00:00", select = c(timestep,trackheading,windspeedms,b.heading,airspeedms,date,new.winddir))
Oct8a <- subset(Allyears_[[1]], timestep>="2008-10-30 16:00:00 UTC" & timestep<="2008-10-31 16:00:00 UTC", select = c(timestep,trackheading,windspeedms,b.heading,airspeedms,date,new.winddir))
Mar1a <- subset(Allyears_[[1]], timestep>="2008-03-27 16:00:00 UTC" & timestep<="2008-03-28 16:00:00 UTC", select = c(timestep,trackheading,windspeedms,b.heading,airspeedms,date,new.winddir))
Mar2a <- subset(Allyears_[[2]], timestep>="2009-03-13 16:00:00" & timestep<="2009-03-14 16:00:00", select = c(timestep,trackheading,windspeedms,b.heading,airspeedms,date,new.winddir))
Mar3a <- subset(Allyears_[[2]], timestep>="2009-03-16 16:00:00" & timestep<="2009-03-17 16:00:00", select = c(timestep,trackheading,windspeedms,b.heading,airspeedms,date,new.winddir))
Mar4a <- subset(Allyears_[[3]], timestep>="2010-03-16 16:00:00 UTC" & timestep<="2010-03-17 16:00:00 UTC", select = c(timestep,trackheading,windspeedms,b.heading,airspeedms,date,new.winddir))
Mar5a <- subset(Allyears_[[3]], timestep>="2010-03-21 16:00:00" & timestep<="2010-03-22 16:00:00" , select = c(timestep,trackheading,windspeedms,b.heading,airspeedms,date,new.winddir))
Mar6a<- subset(Allyears_[[3]], timestep>="2010-03-21 16:00:00" & timestep<="2010-03-22 16:00:00", select = c(timestep,trackheading,windspeedms,b.heading,airspeedms,date,new.winddir))

Alldays <- list(Oct6a,Oct7a,Oct8a,Mar1a,Mar2a,Mar3a,Mar4a,Mar5a,Mar6a)

library(tidyverse)

for (k in 1:length(Alldays)){
  
  Alldays[[k]] <- Alldays[[k]]%>% drop_na

}

for (k in 1:length(Alldays)){
  Alldays[[k]] <- Alldays[[k]] %>% arrange(timestep)
}

for (k in 1:length(Alldays)){
  Alldays[[k]]$timestep<- as.character(Alldays[[k]]$timestep)
}

sun <- list()
library(suncalc)
for (k in 1:length(Alldays)){
  sun[[k]]<- getSunlightTimes(unique(Alldays[[k]]$date),52.60636,4.389639,keep = c("sunrise","sunset"),tz="UTC")
  sun[[k]]$new.date <- as.Date(sun[[k]]$date)
 colnames(sun[[k]])[colnames(sun[[k]])==c("date","new.date")] <- c("timestep","date")
 sun[[k]]$sunrise <- sun[[k]]$sunrise-3600
}

Join <- lapply(1:9, function(n){
  Alldays[[n]] %>% left_join(sun[[n]], by=c("date"))
})

Birds <- list()
for (k in 1:length(Join)){
  if (nrow(sun[[k]])==1){
    Birds[[k]] <- subset(Join[[k]],timestep.x>=sun[[k]][1,5]-86400 & timestep.x<=sun[[k]][1,4] & airspeedms>=5 & airspeedms<=30,select=c(timestep.x:new.winddir))
  }else {
    Birds[[k]] <- subset(Join[[k]],timestep.x>=sun[[k]][1,5] & timestep.x<=sun[[k]][2,4] & airspeedms>=5 & airspeedms<=30,select=c(timestep.x:new.winddir))
  }
  
}





library(lubridate)
for (k in 1:length(Birds)){
  s <- as.data.frame(Birds[[k]])
  datetime <- paste0(date(s[1,1]))
  write.csv(Birds[[k]], file = paste0("Birds_sec",datetime,".csv"))
}
