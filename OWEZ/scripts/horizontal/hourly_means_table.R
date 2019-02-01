############################################################
##########making table with means per hour##################

###first run the script called "data_analysis_OWEZ_calendardays" or "data_analysis_migr.days"
library(circular)

means<- lapply(1:4, function(n){
  Allyears[[n]] %>%
    group_by(timestep) %>%
    summarise(Nr.tracks=length(id),Mean.speed=mean(groundspeedms),Mean.aspeed=mean(airspeedms),
              Mean.wspeed=mean(windspeedms), 
              Mean.tr.dir=mean(circular(trackheading,type="angles", units="degrees",modulo="2pi", template='geographics')), 
              Mean.head=mean(circular(b.heading,type="angles", units="degrees",modulo="2pi", template='geographics')),
              Mean.wdir=mean(circular(new.winddir,type="angles", units="degrees",modulo="2pi", template='geographics')),
              Month=median(n.month),Season=median(season),Light=median(light),DayP=median(dawndusk),
              Timestamp=median(timestep),n.date=median(n.date),migr.day=median(migr.day))
}) 



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

