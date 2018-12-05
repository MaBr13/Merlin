############################################################
##########making table with means per hour##################

###first run the script called "data_analysis_OWEZ_calendardays"

counts <- list()
mean.grspeed <- list()
mean.aspeed <- list()
mean.wspeed <- list()
mean.tr.dir <- list()
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
r <- list()
means <- list()
month <- list()

for(k in 1:length(Allyears)){
  counts[[k]] <- aggregate(Allyears[[k]]$id,by = list(Allyears[[k]]$timestep), FUN="length")
  mean.grspeed[[k]] <- aggregate(Allyears[[k]]$groundspeedms, by = list(Allyears[[k]]$timestep), FUN="mean")
  mean.aspeed[[k]] <- aggregate(Allyears[[k]]$airspeedms, by=list(Allyears[[k]]$timestep), FUN="mean")
  mean.wspeed[[k]] <- aggregate(Allyears[[k]]$windspeedms, by=list(Allyears[[k]]$timestep), FUN="mean")
  mean.tr.dir[[k]] <- aggregate(Allyears[[k]]$trackheading, by=list(Allyears[[k]]$timestep), FUN="mean")
  mean.heading[[k]] <- aggregate(Allyears[[k]]$b.heading, by=list(Allyears[[k]]$timestep), FUN="mean")
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
  r[[k]] <- merge(p[[k]],mean.tr.dir[[k]],by="Group.1",sort=TRUE)
  names(r[[k]])[c(1,2,3,4)] <- paste(c("Timestamp","Mean.head", "Mean.wdir", "Mean.tr.dir"))
  a[[k]]<- merge(g[[k]], date[[k]], by="Group.1", sort = TRUE)
  names(a[[k]])[c(1,2,3,4)]<-paste(c("Timestamp","Nr.tracks", "Mean.speed", "Date"))
  d[[k]] <- merge(s[[k]],r[[k]],by="Timestamp", sort = TRUE )
  l [[k]] <- merge(a[[k]],d[[k]],by="Timestamp", sort=TRUE)
  b[[k]] <- merge(dayP[[k]],month[[k]], by="Timestamp", sort=TRUE)
  e[[k]] <- merge(light[[k]],season[[k]],by="Timestamp", sort=TRUE)
  f[[k]] <- merge(b[[k]],l[[k]], by="Timestamp", sort=TRUE)
  means[[k]] <- merge(f[[k]],e[[k]], by="Timestamp", sort=TRUE)
}
rm(list =c("counts","mean.grspeed", "mean.aspeed", "mean.wspeed","mean.heading" ,"mean.tr.dir","mean.winddir","date", 
           "season","light","dayP","g", "s" ,"p" ,"a" ,"d","l" ,"r","b" ,"e" ,"f" ,"month")) 

#cut(capture.output(print(means),file="means.csv"))#if you want to save the table as csv
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
  
  SpringAll[[k]] <- subset(Allyears[[k]], season==2, select=id:Wspeed)
}

AutumnAll <- list()

for(k in 1:length(Allyears)){
  
  AutumnAll[[k]] <- subset(Allyears[[k]], season==4, select=id:Wspeed)
}

