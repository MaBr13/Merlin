#########################################################################
#############CREATE A TABLE OF DAILY MEANS (MIGRATION DAYS)##############
#########################################################################


######first run the script named "data_analysis_migr.days.conv"

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
