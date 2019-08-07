###########################################################
########STATISTICAL ANALYSIS OF THE OWEZ DATA##############
###########################################################

#10.07.2019., University of Amsterdam
#Maja Bradaric

#load the data
rm(list=ls())
memory.limit(size=20000)
setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/statistical_analysis/data")
#data from radar location for the full season
Autumn_rad <- read.csv("Autumn925.csv",sep=",")
Spring_rad <- read.csv("Spring925.csv",sep=",")
#data from radar location from intense migration nights
Autumn_int_rad <- read.csv("Autumn_int925.csv",sep=",")
Spring_int_rad<- read.csv("Spring_int925.csv",sep = ",")
#data from departure locations on intense nights
path <- setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/statistical_analysis/data/departures")
listcsv <- dir(path,pattern = "*.csv",ignore.case = FALSE)#list all the csvs from the folder

library(lubridate)
file_date <- ymd()
for(k in 1:length(listcsv)){ 
  file_date[k] <- as.Date(sub( ".*?_(\\d{8})_.*", "\\1", listcsv[k]), "%Y%m%d" )#extract the date from file name
}
dumdat <- lapply(listcsv,read.csv) #read in all the files as a list
for(k in 1:length(dumdat)){
  dumdat[[k]]$date <- file_date[k] #add the date column in all the list elements 
}

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
Departures_int <- do.call("rbind",dumdat) # 
rm(list=c("dumdat"))
Departures_Autumn_int <- subset(Departures_int, month(date)>=8 & month(date)<12)
Departures_Spring_int <- subset(Departures_int,month(date)>=2 & month(date)<=5)
rm(list=c("Departures_int"))
#data from departure locations for the full season 
path <- setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/ecmwf_weather_NSea/Forecast/925")
listcsv1 <- dir(path,pattern = "*.csv",ignore.case = FALSE)#list all the csvs from the folder
dum_Depart <- lapply(listcsv1, read.csv)
dep_loc_lat_a <- summary(Departures_Autumn_int$Lat_start)
dep_loc_long_a <- summary(Departures_Autumn_int$Long_start)
dep_loc_lat_s <- summary(Departures_Spring_int$Lat_start)
dep_loc_long_s <- summary(Departures_Spring_int$Long_start)


for (k in 1:length(dum_Depart)){
  dum_Depart[[k]]$windspeedms_mean <-  sqrt((dum_Depart[[k]]$u * dum_Depart[[k]]$u) + (dum_Depart[[k]]$v * dum_Depart[[k]]$v))
  winddirR_mean <- atan2(dum_Depart[[k]]$u, dum_Depart[[k]]$v)
  dum_Depart[[k]]$winddir_mean <- winddirR_mean*(180/pi)
  dum_Depart[[k]]$winddir_mean <- ifelse(dum_Depart[[k]]$winddir_mean<0, 360+dum_Depart[[k]]$winddir_mean, dum_Depart[[k]]$winddir_mean)
}

#filter based on months and bounding boxes for latitude and longitude
Departures <- do.call("rbind",dum_Depart)
rm(list=c("dum_Depart"))

Departures_Spring <- subset(Departures, month(timestamp)>=2 & month(timestamp)<=5 & lat>=dep_loc_lat_s[1]*100 & 
                              lat<= dep_loc_lat_s[6]*100 & long>= dep_loc_long_s[1]*100 & long<= dep_loc_long_s[6]*100)
Departures_Autumn <- subset(Departures, month(timestamp)>=8 & month(timestamp)<=11 & long>= dep_loc_long_a[1]*100 
                            & long<= dep_loc_long_a[6] * 100 & lat>=dep_loc_lat_a[1] * 100 & lat<=dep_loc_lat_a[6] *100 )

rm(list=c("Departures", "dep_loc_lat_a", "dep_loc_lat_s","dep_loc_long_a","dep_loc_long_s","file_date",
          "k","listcsv","listcsv1","path","winddirR_mean","winddirR_start"))


library(circular)
library(CircStats)

#Winddir

wArad <- Autumn_rad$new.winddir
wSrad <- Spring_rad$new.winddir
wAradint <- Autumn_int_rad$new.winddir 
wSradint <- Spring_int_rad$new.winddir
wdepA <- Departures_Autumn$winddir_mean 
wdepS <- Departures_Spring$winddir_mean 
wdepstA <- Departures_Autumn_int$winddir_start 
wdepstS <- Departures_Spring_int$winddir_start  
wdepmnA <- Departures_Autumn_int$winddir_mean 
wdepmnS <- Departures_Spring_int$winddir_mean 

#wind speed
wsArad <- Autumn_rad$windspeedms
wsSrad <- Spring_rad$windspeedms
wsAradint <- Autumn_int_rad$windspeedms
wsSradint <- Spring_int_rad$windspeedms
wsdepA <- Departures_Autumn$windspeedms_mean
wsdepS <- Departures_Spring$windspeedms_mean
wsdepstA <- Departures_Autumn_int$windspeedms_start
wsdepstS <- Departures_Spring_int$windspeedms_start 

windsp <- list(wsArad,wsSrad,wsAradint,wsSradint,wsdepA,wsdepS,wsdepstA,wsdepstS)
meanssp <- lapply(windsp,mean)
sdwsp <- lapply(windsp,sd)
windspan <- list()
set.seed(234)
for(k in 1:length(windsp)){
  windspan[[k]] <- sample(windsp[[k]],windsp[[k]]/2)
}

#heading
hArad <- circular(Autumn_rad$b.heading,units = "degrees", modulo="2pi",template = 'geographic')
hSrad <- circular(Spring_rad$b.heading,units = "degrees", modulo="2pi",template = 'geographic')
hAradint <- circular(Autumn_int_rad$b.heading ,units = "degrees", modulo="2pi",template = 'geographic')
hSradint <- circular(Spring_int_rad$b.heading,units = "degrees", modulo="2pi",template = 'geographic')

headings <- list(hArad,hSrad,hAradint,hSradint)
headmeans <- lapply(headings,mean.circular)
headangdev <- lapply(headings,angular.deviation)
headrho <- lapply(headings,rho.circular)


#track direction
tdArad <- circular(Autumn_rad$trackheading,units = "degrees", modulo="2pi",template = 'geographic')
tdSrad <- circular(Spring_rad$trackheading,units = "degrees", modulo="2pi",template = 'geographic')
tdAradint <- circular(Autumn_int_rad$trackheading ,units = "degrees", modulo="2pi",template = 'geographic')
tdSradint <- circular(Spring_int_rad$trackheading,units = "degrees", modulo="2pi",template = 'geographic')
tdAdepst <- circular(Departures_Autumn_int$td_start ,units = "degrees", modulo="2pi",template = 'geographic')
tdSdepst <- circular(Departures_Spring_int$td_start,units = "degrees", modulo="2pi",template = 'geographic')

tds <- list(tdArad,tdSrad,tdAradint,tdSradint)
tdsmeans <- lapply(tds,mean.circular)
tdsangdev <- lapply(tds,angular.deviation)
tdsrho <- lapply(tds,rho.circular)

#airspeed
asArad <- Autumn_rad$airspeedms
asSrad <- Spring_rad$airspeedms
asAradint <- Autumn_int_rad$airspeedms 
asSradint <- Spring_int_rad$airspeedms

asp <- list(asArad,asSrad,asAradint,asSradint)
asmeans <- lapply(asp,mean)
assd <- lapply(asp,sd)

#groundspeed
gsArad <- Autumn_rad$groundspeedms
gsSrad <- Spring_rad$groundspeedms
gsAradint <- Autumn_int_rad$groundspeedms 
gsSradint <- Spring_int_rad$groundspeedms

gsp <- list(gsArad,gsSrad,gsAradint,gsSradint)
gsmeans <- lapply(gsp,mean)
gssd <- lapply(gsp,sd)

library(ggplot2)

ggplot(Departures_Spring_int, aes(winddir_mean)) + 
  geom_histogram() +
  ggtitle("wdepmnS") +
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = seq(0,360,10))


circwArad <- circular(wArad,units = "degrees", modulo="2pi",template = 'geographic')
circwSrad <- circular(wSrad,units = "degrees",modulo="2pi",template = 'geographic')
circwAradint <- circular(wAradint,units = "degrees",modulo="2pi",template = 'geographic')
circwSradint <- circular(wSradint,units = "degrees",modulo="2pi",template = 'geographic')
circwdepA <- circular(wdepA,units = "degrees",modulo="2pi",template = 'geographic')
circwdepS <- circular(wdepS,units = "degrees",modulo="2pi",template = 'geographic')
circwdepstA <- circular(wdepstA,units = "degrees",modulo="2pi",template = 'geographic')
circwdepstS <- circular(wdepstS,units = "degrees",modulo="2pi",template = 'geographic')
circwdepmnA <- circular(wdepmnA,units = "degrees",modulo="2pi",template = 'geographic')
circwdepmnS <- circular(wdepmnS,units = "degrees",modulo="2pi",template = 'geographic')

#heading
rayleigh.test(circular(Autumn_rad$b.heading,type="directions", units="degrees",modulo="2pi", template='geographics'))
rayleigh.test(circular(Spring_rad$b.heading,type="directions", units="degrees",modulo="2pi", template='geographics'))
rayleigh.test(circular(Autumn_int_rad$b.heading,type="directions", units="degrees",modulo="2pi", template='geographics'))
rayleigh.test(circular(Spring_int_rad$b.heading,type="directions", units="degrees",modulo="2pi", template='geographics'))
#track direction
rayleigh.test(circular(Autumn_rad$trackheading,type="directions", units="degrees",modulo="2pi", template='geographics'))
rayleigh.test(circular(Spring_rad$trackheading,type="directions", units="degrees",modulo="2pi", template='geographics'))
rayleigh.test(circular(Autumn_int_rad$trackheading,type="directions", units="degrees",modulo="2pi", template='geographics'))
rayleigh.test(circular(Spring_int_rad$trackheading,type="directions", units="degrees",modulo="2pi", template='geographics'))

#Watson's large non-parametric test for common mean direction of
#two or more samples of circular data 
#compares values with the quantiles of chi squared distribution


#compare seasonal winds at departure locations with winds on intense nights
#autumn
cdat <- c(circwdepA,circwdepstA)
ndat <- c(length(circwdepA),length(circwdepstA))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)

p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)
#spring
cdat <- c(circwdepS,circwdepstS)
ndat <- c(length(circwdepS),length(circwdepstS))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)
#compare seasonal winds at radar location with winds on intense nights
#autumn
cdat <- c(circwArad,circwAradint)
ndat <- c(length(circwArad),length(circwAradint))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
watson.williams.test.formula(circwArad,circwAradint)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)
#spring
cdat <- c(circwSrad,circwSradint)
ndat <- c(length(wSrad),length(wSradint))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)
#compare winds at the departure location,radar location and mean winds of trajectory on intense nights
#autumn
cdat <- c(wdepstA,wdepmnA,wAradint)
ndat <- c(length(wdepstA),length(wdepmnA),length(wAradint))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)
#spring
cdat <- c(wdepstS,wdepmnS,wSradint)
ndat <- c(length(wdepstS),length(wdepmnS),length(wSradint))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)

#compare seasonal winds between autumn and spring
cdat <- c(circwdepA,circwdepS)
ndat <- c(length(circwdepA),length(circwdepS))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)
#compare winds on intense nights at departure between autumn and spring
cdat <- c(circwdepstS,circwdepstA)
ndat <- c(length(circwdepstS),length(circwdepstA))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)

#compare winds at radar between autumn and spring
cdat <- c(circwArad,circwSrad)
ndat <- c(length(circwArad),length(circwSrad))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)

#compare winds on intense nights at radar between autumn and spring
cdat <- c(wAradint,wSradint)
ndat <- c(length(wAradint),length(wSradint))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)

####Calculating means
winds <- list(wArad,wSrad,wAradint,wSradint,wdepA,wdepS,wdepstA,wdepstS,wdepmnA,wdepmnS)
winds_rad <- list()
means <- list()
for (k in 1:length(winds)){
  winds_rad[[k]] <- mean.circular(winds[[k]])
  means[[k]] <-  winds_rad[[k]]*180/pi
  means[[k]] <- ifelse(means[[k]]<0, 360+means[[k]], means[[k]])
}


winds <- list(circwArad,circwSrad,circwAradint,circwSradint,circwdepA,circwdepS,circwdepstA,circwdepstS,circwdepmnA,circwdepmnS)
winds_rad <- list()
means <- list()
medians <- list()


sdev <- lapply(winds,angular.deviation)
s <- lapply(winds,median.circular)
rho <- lapply(winds,rho.circular)
means <- lapply(winds,mean.circular)
winds_an <- list()
set.seed(354)
for (k in 1:length(winds)){
  winds_an[[k]] <- sample(winds[[k]],length(winds[[k]])/2)
}

#compare seasonal winds at departure locations with winds on intense nights
#autumn
cdat <- c(winds_an[[5]],winds_an[[7]])
ndat <- c(length(winds_an[[5]]),length(winds_an[[7]]))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)
#spring
cdat <- c(winds_an[[6]],winds_an[[8]])
ndat <- c(length(winds_an[[6]]),length(winds_an[[8]]))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)
#compare seasonal winds at radar location with winds on intense nights
#autumn
cdat <- c(winds_an[[1]],winds_an[[3]])
ndat <- c(length(winds_an[[1]]),length(winds_an[[3]]))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)
#spring
cdat <- c(winds_an[[2]],winds_an[[4]])
ndat <- c(length(winds_an[[2]]),length(winds_an[[4]]))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)

 #compare seasonal winds at departure between autumn and spring
cdat <- c(winds_an[[5]],winds_an[[6]])
ndat <- c(length(winds_an[[5]]),length(winds_an[[6]]))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)
#compare winds on intense nights at departure between autumn and spring
cdat <- c(winds_an[[7]],winds_an[[8]])
ndat <- c(length(winds_an[[7]]),length(winds_an[[8]]))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)

#compare winds at radar between autumn and spring
cdat <- c(winds_an[[1]],winds_an[[2]])
ndat <- c(length(winds_an[[1]]),length(winds_an[[2]]))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)

#compare winds on intense nights at radar between autumn and spring
cdat <- c(winds_an[[3]],winds_an[[4]])
ndat <- c(length(winds_an[[3]]),length(winds_an[[4]]))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)
