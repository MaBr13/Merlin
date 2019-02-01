###MAKE TABLES WITH MEANS OF DIFFERENT PARTS OF THE NIGHT THROUGHOUT THE SEASON

######first run the script named "data_analysis_migr.days.conv"

dusk<- lapply(Allyears, subset, uur==c(17,18,19))
b.mid<- lapply(Allyears, subset, uur==c(20,21,22,23))
a.mid <- lapply(Allyears, subset, uur==c(0,1,2,3))
dawn <- lapply(Allyears, subset, uur==c(4,5,6,7))


library(dplyr)
library(circular)



means.dusk <- lapply(1:4, function(n){
  dusk[[n]] %>%
    group_by(n.date) %>%
    summarise(Nr.tracks=length(id),Mean.speed=mean(groundspeedms),Mean.aspeed=mean(airspeedms),
              Mean.wspeed=mean(windspeedms), 
              Mean.tr.dir=mean(circular(trackheading,type="directions", units="degrees",modulo="2pi", template='geographics')), 
              Mean.head=mean(circular(b.heading,type="directions", units="degrees",modulo="2pi", template='geographics')),
              Mean.wdir=mean(circular(new.winddir,type="directions", units="degrees",modulo="2pi", template='geographics')),
              Month=median(n.month),Season=median(season),Light=median(light),DayP=median(dawndusk),Date=median(date))
}) 



means.a.mid <- lapply(1:4, function(n){
  a.mid[[n]] %>%
    group_by(n.date) %>%
    summarise(Nr.tracks=length(id),Mean.speed=mean(groundspeedms),Mean.aspeed=mean(airspeedms),
              Mean.wspeed=mean(windspeedms), 
              Mean.tr.dir=mean(circular(trackheading,type="angles", units="degrees",modulo="2pi", template='geographics')), 
              Mean.head=mean(circular(b.heading,type="angles", units="degrees",modulo="2pi", template='geographics')),
              Mean.wdir=mean(circular(new.winddir,type="angles", units="degrees",modulo="2pi", template='geographics')),
              Month=median(n.month),Season=median(season),Light=median(light),DayP=median(dawndusk),Date=median(date))
}) 


means.b.mid <- lapply(1:4, function(n){
  b.mid[[n]] %>%
    group_by(n.date) %>%
    summarise(Nr.tracks=length(id),Mean.speed=mean(groundspeedms),Mean.aspeed=mean(airspeedms),
              Mean.wspeed=mean(windspeedms), 
              Mean.tr.dir=mean(circular(trackheading,type="angles", units="degrees",modulo="2pi", template='geographics')), 
              Mean.head=mean(circular(b.heading,type="angles", units="degrees",modulo="2pi", template='geographics')),
              Mean.wdir=mean(circular(new.winddir,type="angles", units="degrees",modulo="2pi", template='geographics')),
              Month=median(n.month),Season=median(season),Light=median(light),DayP=median(dawndusk),Date=median(date))
}) 


means.dawn <- lapply(1:4, function(n){
 dawn[[n]] %>%
    group_by(n.date) %>%
    summarise(Nr.tracks=length(id),Mean.speed=mean(groundspeedms),Mean.aspeed=mean(airspeedms),
              Mean.wspeed=mean(windspeedms), 
              Mean.tr.dir=mean(circular(trackheading,type="angles", units="degrees",modulo="2pi", template='geographics')), 
              Mean.head=mean(circular(b.heading,type="angles", units="degrees",modulo="2pi", template='geographics')),
              Mean.wdir=mean(circular(new.winddir,type="angles", units="degrees",modulo="2pi", template='geographics')),
              Month=median(n.month),Season=median(season),Light=median(light),DayP=median(dawndusk),Date=median(date))
}) 



dusk.spring <- lapply(means.dusk,subset,Month>=2 & Month<=5)
a.mid.spring <- lapply(means.a.mid,subset,Month>=2 & Month<=5)
b.mid.spring <- lapply(means.b.mid,subset,Month>=2 & Month<=5)
dawn.spring <- lapply(means.dawn,subset,Month>=2 & Month<=5)
dusk.autumn <- lapply(means.dusk,subset,Month>=8 & Month<=11)
a.mid.autumn <- lapply(means.a.mid,subset,Month>=8 & Month<=11)
b.mid.autumn <- lapply(means.b.mid,subset,Month>=8 & Month<=11)
dawn.autumn <- lapply(means.dawn,subset,Month>=8 & Month<=11)


for(k in 1:length(dusk.autumn)){
  
 dusk.autumn[[k]]$Size <- cut(dusk.autumn[[k]]$Nr.tracks,breaks=c(0,50,100,500,1000,100000), 
                           labels = c("0.1","0.5","2","3","5"))}
