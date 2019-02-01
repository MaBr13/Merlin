#########################################################################
#############CREATE A TABLE OF DAILY MEANS (MIGRATION DAYS)##############
#########################################################################


######first run the script named "data_analysis_migr.days.conv"


means.migr <- list()
means.b.mid <- lapply(1:4, function(n){
  Allyears[[n]] %>%
    group_by(n.date) %>%
    summarise(Nr.tracks=length(id),Mean.speed=mean(groundspeedms),Mean.aspeed=mean(airspeedms),
              Mean.wspeed=mean(windspeedms), 
              Mean.tr.dir=mean(circular(trackheading,type="angles", units="degrees",modulo="2pi", template='geographics')), 
              Mean.head=mean(circular(b.heading,type="angles", units="degrees",modulo="2pi", template='geographics')),
              Mean.wdir=mean(circular(new.winddir,type="angles", units="degrees",modulo="2pi", template='geographics')),
              Month=median(n.month),Season=median(season),Light=median(light),DayP=median(dawndusk))
}) 


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
