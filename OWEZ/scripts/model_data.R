###GETTING THE DATA FOR THE MODEL


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
library(xts)
Allyears <- list(y2007,y2008,y2009,y2010)
rm(list = c("listFstyear","listFyear","listSyear", 
            "listTyear", "y2007", "y2008", "y2009","y2010"))#remove all the lists you don't need

#merge weather data and bird data


path <- setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/data/horizontal/ecmwf_weather")

Fstweather <- read.csv('Winds_radar_2007_pl1000_3h.csv',sep=',')
Sweather <- read.csv('Winds_radar_2008_pl1000_3h.csv',sep = ',')
Tweather <- read.csv('Winds_radar_2009_pl1000_3h.csv',sep = ',')
Fweather <- read.csv('Winds_radar_2010_pl1000_3h.csv',sep = ',')




weather <- list(Fstweather,Sweather,Tweather,Fweather)
rm(list = c("Fstweather","Sweather","Tweather", "Fweather"))#remove all the lists you don't need

for(k in 1:length(weather)){
  weather[[k]]$timestamp <- as.POSIXct(weather[[k]]$timestamp)
  colnames(weather[[k]])[colnames(weather[[k]])=="timestamp"] <- "timestep"
}


library(dplyr)
Allyears_ <- lapply(1:4, function(n){
    Allyears[[n]] %>% left_join(weather[[n]], by=c("timestep"))
   })



library(imputeTS)
for (k in 1:length(Allyears_)){
  Allyears_[[k]]$u_new<- na.interpolation(Allyears_[[k]]$u, option ="linear")
  Allyears_[[k]]$v_new<- na.interpolation(Allyears_[[k]]$v, option ="linear")
}

rm(list = c("weather","Allyears"))
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
  Allyears_[[k]]$migr.day <- with(Allyears_[[k]],ymd_h(paste(n.year,n.month,n.day,uur,sep = ' ')))
  Allyears_[[k]] <- Allyears_[[k]] %>% arrange(timestep)
}


#filter based on light(to get only data between sunrise and sunset) and airspeed
#select only columns needed for further analysis
AllBirds <- list()

for (k in 1:length(Allyears_)){
  AllBirds[[k]]<- subset(Allyears_[[k]],light==0 & airspeedms>=5 & airspeedms<=30,
                         select=c(date,timestep,trackheading, groundspeedms, n.date,n.year,n.month,n.day,
                                                  windspeedms,airspeedms,new.winddir,b.heading))
}

#divide Allbirds table in Spring and Autumn seasons
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

#write out csvs for further analysis
setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/statistical_analysis/data")
write.csv(Spring, file = "Spring.csv")
write.csv(Autumn,file="Autumn.csv")

rm(list=c("spr","Spring","s","aut","Autumn"))

#load the csv files that contains dates of the nights that should be analysed and choose only those nights from the
#horizontal data
setwd('C:/Users/mbradar/Documents/Merlin/OWEZ/Model')
nights <- read.csv('nights.csv')
dates <- as.Date(nights$x)


AllBirds_1 <- do.call("rbind",AllBirds) 
analysis <- AllBirds_1[which(AllBirds_1$n.date %in% dates),]

#divide nights of intense migration per season
Spring_int <- subset(analysis,n.month>=2 & n.month<=5)
Autumn_int <- subset(analysis,n.month>=8 & n.month<12)

setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/statistical_analysis/data")
write.csv(Spring_int, file = "Spring_int.csv")
write.csv(Autumn_int,file="Autumn_int.csv")

rm(list=c("AllBirds_1","analysis","Spring_int","Autumn_int"))
#divide intense nights and filer based on sunrise and sunset times to use them in running a simulation
Allyears_1 <- do.call("rbind",Allyears_) 
analysis <- Allyears_1[which(Allyears_1$n.date %in% dates),]

Alldays <- split(analysis,analysis$n.date)

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

Join <- lapply(1:22, function(n){
  Alldays[[n]] %>% left_join(sun[[n]], by=c("date"))
})


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

#saving the data
setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/radar_data")

library(lubridate)
for (k in 1:length(Birds)){
  s <- as.data.frame(Birds[[k]])
  datetime <- paste0(date(s[1,1]))
  write.csv(Birds[[k]], file = paste0("Birds_hr",datetime,".csv"))
}

fsv <- do.call(rbind.data.frame, Birds)
write.csv(fsv,file = "intense_migration_hr.csv", row.names = FALSE)

###################################################################################
########SKIP THIS PART IF YOU WANT ALL THE DIRECTIONS INCLUDED#####################
#filtering on timestamp, airspeed and directions that majority of birds is using
#first divide list in autumn and spring, because preferred directions are not the same


for (k in 1:8){
    Birds[[k]] <- subset(Birds[[k]],b.heading>=165 & b.heading<=280)
  }
  
for (k in 9:13){
  Birds[[k]] <- subset(Birds[[k]],b.heading>=30 & b.heading<=165)
}


for(k in 1:length(Birds)){
  
  Birds[[k]]$Aspeed<- cut(Birds[[k]]$airspeedms,breaks=c(5,10,15,20,25,30), 
                             labels = c("5-10","10-15","15-20", "20-25", "25-30"))
  
}

for (k in 1:length(Birds)){
  Birds[[k]]$Aspeed <- factor(Birds[[k]]$Aspeed, levels = rev(levels(Birds[[k]]$Aspeed)))
  
}
#visualizations
ggplot(Birds[[9]], aes(x=b.heading)) + 
  geom_histogram(aes(fill=Birds[[9]]$Aspeed, colour=Birds[[9]]$Aspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black","black"), name="Air speed (m/s)", drop=F)+
  scale_fill_manual(values = c("snow2","gray83","lightslategray","darkslategray4","darkslategray", "turquoise4"), name="Air speed (m/s)", drop=F)+
 # ggtitle('Heading of birds') + 
  ylab("Number of tracks")+ 
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

#categories wind speed

wind_a <- read.csv('winds_autumn2.csv',sep = ',')



  
  wind_a$Wspeed<- cut(wind_a$wind_sp,breaks=c(-1,5,10,15,20,25,50), 
                             labels = c("0-5","5-10","10-15","15-20","20-25",">25"))
  


  wind_a$Wspeed <- factor(wind_a$Wspeed, levels = rev(levels(wind_a$Wspeed)))
  


ggplot(wind_a, aes(x=winddir)) + 
  geom_histogram(aes(fill=wind_a$Wspeed, colour=wind_a$Wspeed), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values = c("black", "black", "black",  "black", "black",  "black","black","black","black"), name="Wind speed (m/s)", drop=F)+
  scale_fill_manual(values = c("snow2","gray83","lightslategray","darkslategray4","darkslategray", "turquoise4"), name="Wind speed (m/s)", drop=F)+
  #ggtitle("Winds") + 
  ylab("Number of tracks")+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))


