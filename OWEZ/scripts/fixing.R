######################################################################################################################
###################                  DATA PREPARATION FOR STATISTICAL ANALYSIS             ###########################
######################################################################################################################
library(lubridate)
library(plyr)
library(dplyr)
library(circular)
library(yaImpute)
library(rWind)
library(RNCEP)
library(xlsx)
library(raster)
#load the data
rm(list=ls())
memory.limit(size=20000)
setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/statistical_analysis/data")
#data from radar location for the full season (needed for speeds and headings)
Autumn_rad <- read.csv("Autumn925.csv",sep=",")
Spring_rad <- read.csv("Spring925.csv",sep=",")
#load the file that contains dates of intense migration nights
setwd('C:/Users/mbradar/Documents/Merlin/OWEZ/Model')#load csv that contains dates of intense migr. nights
nights <- read.csv('nights.csv')
dates <- as.Date(nights$x)
#remove nights with intense migration from the rest of the season
Autumn_rad <- Autumn_rad[which(!(Autumn_rad$n.date %in% nights$x)),]#using nights because n.date has class of factor
Spring_rad <- Spring_rad[which(!(Spring_rad$n.date %in% nights$x)),]#using nights because n.date has class of factor
#data from radar location for intense migration nights (needed for speeds and headings)
setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/statistical_analysis/data")
Autumn_int_rad <- read.csv("Autumn_int925.csv",sep=",")
Spring_int_rad<- read.csv("Spring_int925.csv",sep = ",")
#data from departure locations on intense nights
path <- setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/statistical_analysis/data/departures")
listcsv <- dir(path,pattern = "*925.csv",ignore.case = FALSE)#list all the csvs from the folder
#to extract date from the file name
file_date <- ymd()
for(k in 1:length(listcsv)){ 
  file_date[k] <- as.Date(sub( ".*?_(\\d{8})_.*", "\\1", listcsv[k]), "%Y%m%d" )#extract the date from file name
}
#read in all the files
dumdat <- lapply(listcsv,read.csv) #read in all the files as a list
for(k in 1:length(dumdat)){
  dumdat[[k]]$date <- file_date[k] #add the date column in all the list elements 
}
#calculate wind speed and direction from u and v component
for (k in 1:length(dumdat)){
  dumdat[[k]]$windspeedms_start <-  sqrt((dumdat[[k]]$wu_start * dumdat[[k]]$wu_start) + (dumdat[[k]]$wv_start * dumdat[[k]]$wv_start))
  winddirR_start <- atan2(dumdat[[k]]$wu_start, dumdat[[k]]$wv_start)
  dumdat[[k]]$winddir_start <- winddirR_start*(180/pi)
  dumdat[[k]]$winddir_start <- ifelse(dumdat[[k]]$winddir_start<0, 360+dumdat[[k]]$winddir_start, dumdat[[k]]$winddir_start)
  
  dumdat[[k]]$windspeedms_mean <-  sqrt((dumdat[[k]]$wu_mean * dumdat[[k]]$wu_mean) + (dumdat[[k]]$wv_mean * dumdat[[k]]$wv_mean))
  winddirR_mean <- atan2(dumdat[[k]]$wu_mean, dumdat[[k]]$wv_mean)
  dumdat[[k]]$winddir_mean <- winddirR_mean*(180/pi)
  dumdat[[k]]$winddir_mean <- ifelse(dumdat[[k]]$winddir_mean<0, 360+dumdat[[k]]$winddir_mean, dumdat[[k]]$winddir_mean)
  
}
#make a dataframe
Departures_int <- do.call("rbind",dumdat)
#clean up
rm(list=c("dumdat"))
#subset per season
Departures_Autumn_int <- subset(Departures_int, month(date)>=8 & month(date)<12)
Departures_Spring_int <- subset(Departures_int,month(date)>=2 & month(date)<=5)
#clean up
rm(list=c("Departures_int"))
#data from departure locations for the rest of the season (excluding intense migration nights)
path <- setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/ecmwf_weather_NSea/Forecast/925")
listcsv1 <- dir(path,pattern = "*.csv",ignore.case = FALSE)#list all the csvs from the folder
dum_Depart <- lapply(listcsv1, read.csv)
#extract max and min long and lat for creating bounding box
dep_loc_lat_a <- summary(Departures_Autumn_int$Lat_start)
dep_loc_long_a <- summary(Departures_Autumn_int$Long_start)
dep_loc_lat_s <- summary(Departures_Spring_int$Lat_start)
dep_loc_long_s <- summary(Departures_Spring_int$Long_start)
#calculate wind direction and speed from u and v component
for (k in 1:length(dum_Depart)){
  dum_Depart[[k]]$windspeedms_mean <-  sqrt((dum_Depart[[k]]$u * dum_Depart[[k]]$u) + (dum_Depart[[k]]$v * dum_Depart[[k]]$v))
  winddirR_mean <- atan2(dum_Depart[[k]]$u, dum_Depart[[k]]$v)
  dum_Depart[[k]]$winddir_mean <- winddirR_mean*(180/pi)
  dum_Depart[[k]]$winddir_mean <- ifelse(dum_Depart[[k]]$winddir_mean<0, 360+dum_Depart[[k]]$winddir_mean, dum_Depart[[k]]$winddir_mean)
}
#bind in a data frame
Departures <- do.call("rbind",dum_Depart)
#remove nights of intense migration from full season
Departures <- Departures[which(!(date(Departures$timestamp) %in% dates)),]
#clean up
rm(list=c("dum_Depart"))
#filter based on months and bounding boxes for latitude and longitude
Departures_Spring <- subset(Departures, month(timestamp)>=2 & month(timestamp)<=5 & lat>=dep_loc_lat_s[1]*100 & 
                              lat<= dep_loc_lat_s[6]*100 & long>= dep_loc_long_s[1]*100 & long<= dep_loc_long_s[6]*100)
Departures_Autumn <- subset(Departures, month(timestamp)>=8 & month(timestamp)<=11 & long>= dep_loc_long_a[1]*100 
                            & long<= dep_loc_long_a[6] * 100 & lat>=dep_loc_lat_a[1] * 100 & lat<=dep_loc_lat_a[6] *100 )
#clean up
rm(list=c("Departures", "dep_loc_lat_a", "dep_loc_lat_s","dep_loc_long_a","dep_loc_long_s","file_date",
          "k","listcsv","listcsv1","path","winddirR_mean","winddirR_start"))

#some of the tracks that didn't enter the model stayed in the data from the radar location, as we filtered based on
#the column light, instead of using the real sunrise and sunset times; therefore, we detect matching rows, and get rid
#of those that do not have a match with the data that entered the model

#choose columns for matching
checkS1 <- Spring_int_rad[,c(6,13,11)]
checkS2 <- Departures_Spring_int[,c(15,12,13)]
checkA1 <- Autumn_int_rad[,c(6,13,11)]
checkA2 <- Departures_Autumn_int[,c(15,12,13)]
#match names of the columns
names(checkS1)[names(checkS1) == c("n.date","b.heading","airspeedms")] <- c("date","heading","airspeed")
names(checkA1)[names(checkA1) == c("n.date","b.heading","airspeedms")] <- c("date","heading","airspeed")
#change date class
checkS2$date <- as.factor(checkS2$date)
checkA2$date <- as.factor(checkA2$date)
#find rows that don't have a match
rowsS <- unique(unlist(mapply(function(x, y) 
  sapply(setdiff(x, y), function(d) which(x==d)), checkS1, checkS2)))
rowsA <- unique(unlist(mapply(function(x, y) 
  sapply(setdiff(x, y), function(d) which(x==d)), checkA1, checkA2)))
#remove rows that don't have a match
Spring_int_rad <- Spring_int_rad[-rowsS,]
Autumn_int_rad <- Autumn_int_rad[-rowsA,]
#clean up
rm(list=c("checkA1","checkA2","checkS1","checkS2","rowsA","rowsS","dates"))
#calculate the amount of drift and preferred migration direction (McLaren et al 2012) and calculate the tailwind
#assistance using NCEP.Tailwind from RNCEP package, as suggested in Kemp et al.2010

Spring_rad$u <- ds2uv(Spring_rad$new.winddir,Spring_rad$windspeedms)[,1]
Spring_rad$v <- ds2uv(Spring_rad$new.winddir,Spring_rad$windspeedms)[,2]
Spring_rad$wind_ass <- NCEP.Tailwind(Spring_rad$u,Spring_rad$v,
                                     as.numeric(Spring_rad$b.heading),Spring_rad$airspeedms)[,1]


Spring_int_rad$u <- ds2uv(Spring_int_rad$new.winddir,Spring_int_rad$windspeedms)[,1]
Spring_int_rad$v <- ds2uv(Spring_int_rad$new.winddir,Spring_int_rad$windspeedms)[,2]
Spring_int_rad$wind_ass <- NCEP.Tailwind(Spring_int_rad$u,Spring_int_rad$v,
                                         as.numeric(Spring_int_rad$b.heading),Spring_int_rad$airspeedms)[,1]


Autumn_rad$u <- ds2uv(Autumn_rad$new.winddir,Autumn_rad$windspeedms)[,1]
Autumn_rad$v <- ds2uv(Autumn_rad$new.winddir,Autumn_rad$windspeedms)[,2]
Autumn_rad$wind_ass <- NCEP.Tailwind(Autumn_rad$u,Autumn_rad$v,
                                     as.numeric(Autumn_rad$b.heading),Autumn_rad$airspeedms)[,1]

Autumn_int_rad$u <- ds2uv(Autumn_int_rad$new.winddir,Autumn_int_rad$windspeedms)[,1]
Autumn_int_rad$v <- ds2uv(Autumn_int_rad$new.winddir,Autumn_int_rad$windspeedms)[,2]
Autumn_int_rad$wind_ass <- NCEP.Tailwind(Autumn_int_rad$u,Autumn_int_rad$v,
                                         as.numeric(Autumn_int_rad$b.heading),Autumn_int_rad$airspeedms)[,1]

Departures_Spring_int$wind_ass_start <-NCEP.Tailwind(Departures_Spring_int$wu_start,
                                                     Departures_Spring_int$wv_start,
                                                     as.numeric(Departures_Spring_int$heading), 
                                                     Departures_Spring_int$airspeed)[,1] 
Departures_Spring_int$wind_ass_mean <-NCEP.Tailwind(Departures_Spring_int$wu_mean,
                                                    Departures_Spring_int$wv_mean,
                                                    as.numeric(Departures_Spring_int$heading), 
                                                    Departures_Spring_int$airspeed)[,1]

Departures_Autumn_int$wind_ass_start <-NCEP.Tailwind(Departures_Autumn_int$wu_start,
                                                     Departures_Autumn_int$wv_start,
                                                     as.numeric(Departures_Autumn_int$heading), 
                                                     Departures_Autumn_int$airspeed)[,1] 
Departures_Autumn_int$wind_ass_mean <-NCEP.Tailwind(Departures_Autumn_int$wu_mean,
                                                    Departures_Autumn_int$wv_mean,
                                                    as.numeric(Departures_Autumn_int$heading), 
                                                    Departures_Autumn_int$airspeed)[,1]

#Since we've chosen the departure data for the rest of the season (excluding intese migration nights) based on
#bounding box of lats and longs, we wanted to match seasonal data with the actual locations from which the birds
#departed; in order to do so we needed to:
#match the names of columns
#remove all other timestamps except for 18:00:00
Departures_Spring <- subset(Departures_Spring,hour(timestamp)==18)
Departures_Autumn <- subset(Departures_Autumn,hour(timestamp)==18)
#make dates out of timestamps
Departures_Spring$date <- date(Departures_Spring$timestamp)
Departures_Autumn$date <- date(Departures_Autumn$timestamp)
#make lats and longs proper for the analysis (they were saved in this format in our data repository)
Departures_Spring$lat <- Departures_Spring$lat/100
Departures_Spring$long <- Departures_Spring$long/100
Departures_Autumn$lat <- Departures_Autumn$lat/100
Departures_Autumn$long <- Departures_Autumn$long/100
#Add raster to all dataframes with multiple locations
r1 <- raster(xmn=-18,xmx=30,ymn=45,ymx=65,resolution=c(0.25,0.25))
#give values to individual raster cells
vals <- 1:ncell(r1)
r1 <- setValues(r1,vals)
Departures_Spring$grid <- raster::extract(r1, Departures_Spring[,c('long', 'lat')])
Departures_Autumn$grid <- raster::extract(r1, Departures_Autumn[,c('long', 'lat')])
Departures_Spring_int$grid <- raster::extract(r1, Departures_Spring_int[,c('Long_start', 'Lat_start')])
Departures_Autumn_int$grid <- raster::extract(r1, Departures_Autumn_int[,c('Long_start', 'Lat_start')])

#extract only those days and locations that exist in the real radar data
Departures_Spring <- Departures_Spring[which((Departures_Spring$grid %in% Departures_Spring_int$grid)),]
Departures_Spring <- Departures_Spring[which(as.factor(Departures_Spring$date) %in% Spring_rad$n.date),]
Departures_Autumn <- Departures_Autumn[which((Departures_Autumn$grid %in% Departures_Autumn_int$grid)),]
Departures_Autumn <- Departures_Autumn[which(as.factor(Departures_Autumn$date) %in% Autumn_rad$n.date),]
#assign headings from the rest of the season to wind data at the departure to calculate pmd and wa
names(Departures_Spring)[names(Departures_Spring)==c("date")] <- c("n.date")
Departures_Spring$n.date <- as.factor(Departures_Spring$n.date)
names(Departures_Autumn)[names(Departures_Autumn)==c("date")] <- c("n.date")
Departures_Autumn$n.date <- as.factor(Departures_Autumn$n.date)


Smer <- Spring_rad[,c(6,11,13)] 
Amer <- Autumn_rad[,c(6,11,13)]

Departures_Spring <- Departures_Spring %>%
  group_by(n.date) %>%
  summarise(mean_windspeed=mean(windspeedms_mean),mean_winddir=mean(circular(winddir_mean, units = "degrees",modulo = "2pi", 
                                                            template ="geographic" )))

Departures_Autumn <- Departures_Autumn %>%
  group_by(n.date) %>%
  summarise(mean_windspeed=mean(windspeedms_mean),mean_winddir=mean(circular(winddir_mean, units = "degrees",modulo = "2pi", 
                                                                             template ="geographic" )))

Departures_Spring <- merge(Departures_Spring,Smer,by="n.date",all=TRUE)
Departures_Autumn <- merge(Departures_Autumn,Amer,by="n.date",all=TRUE)
Departures_Spring[,6:7] <- ds2uv(Departures_Spring$mean_winddir, Departures_Spring$mean_windspeed)
Departures_Autumn[,6:7] <- ds2uv(Departures_Autumn$mean_winddir, Departures_Autumn$mean_windspeed)
Departures_Spring <- Departures_Spring %>% rename (airspeed=airspeedms,heading=b.heading,u=V6,v=V7)
Departures_Autumn <- Departures_Autumn %>% rename (airspeed=airspeedms,heading=b.heading,u=V6,v=V7)



Departures_Spring$wind_ass <- NCEP.Tailwind(Departures_Spring$u,Departures_Spring$v,
                                            as.numeric(Departures_Spring$heading),Departures_Spring$airspeed)[,1]
Departures_Spring$td <- Departures_Spring$heading+Departures_Spring$mean_winddir
Departures_Spring$gsp <- Departures_Spring$airspeed+Departures_Spring$mean_windspeed


Departures_Autumn$wind_ass <- NCEP.Tailwind(Departures_Autumn$u,Departures_Autumn$v,
                                            as.numeric(Departures_Autumn$heading),Departures_Autumn$airspeed)[,1]
Departures_Autumn$td <- Departures_Autumn$heading+Departures_Autumn$mean_winddir
Departures_Autumn$gsp <- Departures_Autumn$airspeed+Departures_Autumn$mean_windspeed

Departures_Spring$td <- ifelse(Departures_Spring$td>360,0+(Departures_Spring$td-360),Departures_Spring$td)
Departures_Autumn$td <- ifelse(Departures_Autumn$td>360,0+(Departures_Autumn$td-360),Departures_Autumn$td)

Departures_Spring <- Departures_Spring %>%
  group_by(n.date) %>%
  summarise(mean_windspeed=mean(mean_windspeed),airspeed=mean(airspeed),gsp=mean(gsp),wind_ass=mean(wind_ass),
            mean_winddir=mean(circular(mean_winddir, units = "degrees",modulo = "2pi", 
                                       template ="geographic" )),
            heading=mean(circular(heading, units = "degrees",modulo = "2pi", 
                                  template ="geographic" )),
            td=mean(circular(td, units = "degrees",modulo = "2pi", 
                             template ="geographic" )))

Departures_Autumn <- Departures_Autumn %>%
  group_by(n.date) %>%
  summarise(mean_windspeed=mean(mean_windspeed),airspeed=mean(airspeed),gsp=mean(gsp),wind_ass=mean(wind_ass),
            mean_winddir=mean(circular(mean_winddir, units = "degrees",modulo = "2pi", 
                                       template ="geographic" )),
            heading=mean(circular(heading, units = "degrees",modulo = "2pi", 
                                  template ="geographic" )),
            td=mean(circular(td, units = "degrees",modulo = "2pi", 
                             template ="geographic" )))
#clean up
rm(list=c("Amer","Smer","r1","nights","vals"))


######################################################################################################################
####################################      SUMMARY STATISTICS    ######################################################
######################################################################################################################

#track direction
tdSrad <- circular(Spring_rad$trackheading,units = "degrees", modulo="2pi",template = 'geographic')
tdArad <- circular(Autumn_rad$trackheading,units = "degrees", modulo="2pi",template = 'geographic')
tdSradint <- circular(Spring_int_rad$trackheading,units = "degrees", modulo="2pi",template = 'geographic')
tdAradint <- circular(Autumn_int_rad$trackheading ,units = "degrees", modulo="2pi",template = 'geographic')
tdSdepst <- circular(Departures_Spring_int$td_start,units = "degrees", modulo="2pi",template = 'geographic')
tdAdepst <- circular(Departures_Autumn_int$td_start ,units = "degrees", modulo="2pi",template = 'geographic')
tdSdep <- circular(Departures_Spring$td,units="degrees",modulo="2pi",template = 'geographic')
tdAdep <- circular(Departures_Autumn$td,units="degrees",modulo="2pi",template = 'geographic')
tdSdepm <- circular(Departures_Spring_int$td_mean, units="degrees",modulo="2pi",template = 'geographic')
tdAdepm <- circular(Departures_Autumn_int$td_mean, units="degrees",modulo="2pi",template = 'geographic')

tds <- list(tdSrad,tdArad,tdSradint,tdAradint,tdSdepst,tdAdepst,tdSdep,tdAdep)#,tdSdepm,tdAdepm)
tdsmeans <- lapply(tds,mean.circular)
tdsangdev <- lapply(tds,angular.deviation)
tdsrho <- lapply(tds,rho.circular)

#groundspeed
gsSrad <- Spring_rad$groundspeedms
gsArad <- Autumn_rad$groundspeedms
gsSradint <- Spring_int_rad$groundspeedms
gsAradint <- Autumn_int_rad$groundspeedms
gsSdepst <- Departures_Spring_int$gsms_start
gsAdepst <- Departures_Autumn_int$gsms_start
gsSdep <- Departures_Spring$gsp
gsAdep <- Departures_Autumn$gsp
gsSdepm <- Departures_Spring_int$gsms_mean
gsAdepm <- Departures_Autumn_int$gsms_mean

gsp <- list(gsSrad,gsArad,gsSradint,gsAradint,gsSdepst,gsAdepst,gsSdep,gsAdep)#,gsSdepm,gsAdepm)
gsmeans <- lapply(gsp,mean)
gssd <- lapply(gsp,sd)

#heading
hSrad <- circular(Spring_rad$b.heading,units = "degrees", modulo="2pi",template = 'geographic')
hArad <- circular(Autumn_rad$b.heading,units = "degrees", modulo="2pi",template = 'geographic')
hSradint <- circular(Spring_int_rad$b.heading,units = "degrees", modulo="2pi",template = 'geographic')
hAradint <- circular(Autumn_int_rad$b.heading ,units = "degrees", modulo="2pi",template = 'geographic')
hSdepst <- circular(Departures_Spring_int$heading, units = "degrees", modulo="2pi",template = 'geographic')
hAdepst <- circular(Departures_Autumn_int$heading ,units = "degrees", modulo="2pi",template = 'geographic')
hSdep <- circular(Departures_Spring$heading ,units="degrees",modulo="2pi",template = 'geographic')
hAdep <- circular(Departures_Autumn$heading,units="degrees",modulo="2pi",template = 'geographic')
hSdepm <- circular(Departures_Spring_int$heading, units="degrees",modulo="2pi",template = 'geographic')
hAdepm <- circular(Departures_Autumn_int$heading, units="degrees",modulo="2pi",template = 'geographic')

headings <- list(hSrad,hArad,hSradint,hAradint,hSdepst,hAdepst,hSdep,hAdep)#,hSdepm,hAdepm)
headmeans <- lapply(headings,mean.circular)
headangdev <- lapply(headings,angular.deviation)
headrho <- lapply(headings,rho.circular)

#airspeed
asSrad <- Spring_rad$airspeedms
asArad <- Autumn_rad$airspeedms
asSradint <- Spring_int_rad$airspeedms
asAradint <- Autumn_int_rad$airspeedms 
asSdepst <- Departures_Spring_int$airspeed
asAdepst <- Departures_Autumn_int$airspeed
asSdep <- Departures_Spring$airspeed
asAdep <- Departures_Autumn$airspeed
asSdepm <- Departures_Spring_int$airspeed
asAdepm <- Departures_Autumn_int$airspeed

asp <- list(asSrad,asArad,asSradint,asAradint,asSdepst,asAdepst,asSdep,asAdep)#,asSdepm,asAdepm)
asmeans <- lapply(asp,mean)
assd <- lapply(asp,sd)

#Winddir
wSrad <- Spring_rad$new.winddir
wArad <- Autumn_rad$new.winddir
wSradint <- Spring_int_rad$new.winddir
wAradint <- Autumn_int_rad$new.winddir 
wdepstS <- Departures_Spring_int$winddir_start
wdepstA <- Departures_Autumn_int$winddir_start 
wdepS <- Departures_Spring$mean_winddir 
wdepA <- Departures_Autumn$mean_winddir 
wdepmS <- Departures_Spring_int$winddir_mean
wdepmA <- Departures_Autumn_int$winddir_mean

circwSrad <- circular(wSrad,units = "degrees",modulo="2pi",template = 'geographic')
circwArad <- circular(wArad,units = "degrees", modulo="2pi",template = 'geographic')
circwSradint <- circular(wSradint,units = "degrees",modulo="2pi",template = 'geographic')
circwAradint <- circular(wAradint,units = "degrees",modulo="2pi",template = 'geographic')
circwdepstS <- circular(wdepstS,units = "degrees",modulo="2pi",template = 'geographic')
circwdepstA <- circular(wdepstA,units = "degrees",modulo="2pi",template = 'geographic')
circwdepS <- circular(wdepS,units = "degrees",modulo="2pi",template = 'geographic')
circwdepA <- circular(wdepA,units = "degrees",modulo="2pi",template = 'geographic')
circwdepmS <- circular(wdepmS,units = "degrees",modulo="2pi",template = 'geographic')
circwdepmA <- circular(wdepmA,units = "degrees",modulo="2pi",template = 'geographic')

winddirs <- list(circwSrad,circwArad,circwSradint,circwAradint,circwdepstS,circwdepstA,circwdepS,circwdepA)#,
                 #circwdepstSnew,circwdepstAnew,
                # circwdepmS,circwdepmA)
winddirmns <- lapply(winddirs,mean.circular)
winddirangdev <- lapply(winddirs,angular.deviation)
winddirrho <- lapply(winddirs,rho.circular)

#wind speed
wsSrad <- Spring_rad$windspeedms
wsArad <- Autumn_rad$windspeedms
wsSradint <- Spring_int_rad$windspeedms
wsAradint <- Autumn_int_rad$windspeedms
wsdepstS <- Departures_Spring_int$windspeedms_start
wsdepstA <- Departures_Autumn_int$windspeedms_start
wsdepS <- Departures_Spring$mean_windspeed
wsdepA <- Departures_Autumn$mean_windspeed
wsdepmS <- Departures_Spring_int$windspeedms_mean
wsdepmA <- Departures_Autumn_int$windspeedms_mean

windsp <- list(wsSrad,wsArad,wsSradint,wsAradint,wsdepstS,wsdepstA,wsdepS,wsdepA)#,
               #wsdepstSnew,wsdepstAnew,
               #wsdepmS,wsdepmA)
meanssp <- lapply(windsp,mean)
sdwsp <- lapply(windsp,sd)

#wind assistance
waSrad <- Spring_rad$wind_ass
waArad <- Autumn_rad$wind_ass
waSradint <- Spring_int_rad$wind_ass
waAradint <- Autumn_int_rad$wind_ass
wadepstS <- Departures_Spring_int$wind_ass_start
wadepstA <- Departures_Autumn_int$wind_ass_start
wadepS <- Departures_Spring$wind_ass
wadepA <- Departures_Autumn$wind_ass
wadepmS <- Departures_Spring_int$wind_ass_mean
wadepmA <- Departures_Autumn_int$wind_ass_mean

wind_ass <- list(waSrad,waArad,waSradint,waAradint,wadepstS,wadepstA,
                 wadepS,wadepA)#,
                 #wadepmS,wadepmA)
wind_assmns <- lapply(wind_ass,mean)
wind_asssd <- lapply(wind_ass,sd)
#making a table
means <- rbind.fill(as.data.frame(unlist(tdsmeans)),as.data.frame(unlist(tdsangdev)),as.data.frame(unlist(tdsrho)),
                    as.data.frame(unlist(gsmeans)),as.data.frame(unlist(gssd)),as.data.frame(unlist(headmeans)),
                    as.data.frame(unlist(headangdev)),as.data.frame(unlist(headrho)),as.data.frame(unlist(asmeans)),
                    as.data.frame(unlist(assd)),as.data.frame(unlist(winddirmns)),as.data.frame(unlist(winddirangdev)),
                    as.data.frame(unlist(winddirrho)),as.data.frame(unlist(meanssp)),as.data.frame(unlist(sdwsp)),
                    as.data.frame(unlist(wind_assmns)),as.data.frame(unlist(wind_asssd)))
names(means) <- c("td_mean","td_angdev","td_r","gsp_mean","gsp_sd","h_mean","h_angdev","h_r","asp_mean","asp_sd",
                  "wdir_mean","wdir_angdev","wdir_r","wsp_mean","wsp_sd","wind_ass_mean","wind_ass_sd")
write.xlsx(means,file="C:/Users/mbradar/Documents/Merlin/OWEZ/Model/statistical_analysis/summary_stats_daily_new2.xlsx")

#######################################################################################################################
#######################                    STATISTICAL ANALYSIS              ##########################################
#######################################################################################################################

#Rayleigh's test to check for uniformity of the directional data (to check if it fills the reqs for WW test)
r.test_td <- lapply(tds,rayleigh.test)
r.test_h <- lapply(headings,rayleigh.test)
r.test_wd <- lapply(winddirs,rayleigh.test)
r.test_pmd <- lapply(pmds,rayleigh.test)


#airspeed
t.test(asSrad,asArad)
t.test(asSrad,asSradint)
t.test(asArad,asAradint)
t.test(asSradint,asAradint)
#groundspeed
t.test(gsSrad,gsArad)
t.test(gsSrad,gsSradint)
t.test(gsArad,gsAradint)
t.test(gsSradint,gsAradint)
#wind speed
t.test(wsSrad,wsArad)
t.test(wsSrad,wsSradint)
t.test(wsSrad,wsdepS)

t.test(wsArad,wsAradint)
t.test(wsArad,wsdepA)

t.test(wsSradint,wsAradint)
t.test(wsSradint,wsdepstS)
t.test(wsSradint,wsdepS)
t.test(wsSradint,wsdepmS)

t.test(wsAradint,wsdepstA)
t.test(wsAradint,wsdepA)
t.test(wsAradint,wsdepmA)

t.test(wsdepstS,wsdepstA)
t.test(wsdepstS,wsdepS)
t.test(wsdepstS,wsdepmS)

t.test(wsdepstA,wsdepA)
t.test(wsdepstA,wsdepmA)

t.test(wsdepS,wsdepA)
t.test(wsdepS,wsdepmS)

t.test(wsdepA,wsdepmA)

t.test(wsdepmS,wsdepmA)

#wind assistance
t.test(waSrad,waArad)
t.test(waSrad,waSradint)
t.test(waSrad,wadepS)

t.test(waArad,waAradint)
t.test(waArad,wadepA)

t.test(waSradint,waAradint)
t.test(waSradint,wadepstS)
t.test(waSradint,wadepS)
t.test(waSradint,wadepmS)

t.test(waAradint,wadepstA)
t.test(waAradint,wadepA)
t.test(waAradint,wadepmA)

t.test(wadepstS,wadepstA)
t.test(wadepstS,wadepS)
t.test(wadepstS,wadepmS)

t.test(wadepstA,wadepA)
t.test(wadepstA,wadepmA)

t.test(wadepS,wadepA)
t.test(wadepS,wadepmS)

t.test(wadepA,wadepmA)

t.test(wadepmS,wadepmA)

watson.wheeler.test(list(circwdepA,hArad))
watson.wheeler.test(list(circwdepS,hSrad))
watson.wheeler.test(list(circwdepstA,hArad))
watson.wheeler.test(list(circwdepstS,hSrad))

