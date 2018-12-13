dusk <- list()
b.mid <- list()
a.mid <- list()
dawn <- list()

for (k in 1:length(Allyears)){
  dusk[[k]] <- subset(Allyears[[k]], uur==c(15,16,17,18,19), select = id:migr.day)
  b.mid[[k]] <- subset(Allyears[[k]], uur==c(20,21,22,23), select = id:migr.day)
  a.mid[[k]] <- subset(Allyears[[k]], uur==c(0,1,2,3), select = id:migr.day)
  dawn[[k]] <- subset(Allyears[[k]], uur==c(4,5,6,7), select = id:migr.day)
}

library(dplyr)
library(circular)

means.b.mid <- list()

means.b.mid <- lapply(1:4, function(n){
  b.mid[[n]] %>%
    group_by(n.date) %>%
    summarise(Nr.tracks=length(id),Mean.speed=mean(groundspeedms),Mean.aspeed=mean(airspeedms),
              Mean.wspeed=mean(windspeedms), 
              Mean.tr.dir=mean(circular(trackheading,type="angles", units="degrees",modulo="2pi", template='geographics')), 
              Mean.head=mean(circular(b.heading,type="angles", units="degrees",modulo="2pi", template='geographics')),
              Mean.wdir=mean(circular(new.winddir,type="angles", units="degrees",modulo="2pi", template='geographics')),
              Month=median(n.month),Season=median(season),Light=median(light),DayP=median(dawndusk))
}) 


dusk.spring <- lapply(means.dusk,subset,Month>=2 & Month<=5)
a.mid.spring <- lapply(means.a.mid,subset,Month>=2 & Month<=5)
b.mid.spring <- lapply(means.b.mid,subset,Month>=2 & Month<=5)
dawn.spring <- lapply(means.dawn,subset,Month>=2 & Month<=5)
dusk.autumn <- lapply(means.dusk,subset,Month>=8 & Month<=11)
a.mid.autumn <- lapply(means.a.mid,subset,Month>=8 & Month<=11)
b.mid.autumn <- lapply(means.b.mid,subset,Month>=8 & Month<=11)
dawn.autumn <- lapply(means.dawn,subset,Month>=8 & Month<=11)
