#extract birds from sunrise to sunset with the radar data

Oct1a <- subset(Allyears[[1]], timestep>="2007-10-10 16:00:00" & timestep<="2007-10-11 16:00:00", select = id:Wspeed)
Oct2a <- subset(Allyears[[1]], timestep>="2007-10-13 16:00:00 UTC" & timestep<="2007-10-14 16:00:00 UTC" , select = id:Wspeed)
Oct3a <- subset(Allyears[[1]], timestep>="2007-10-18 16:00:00" & timestep<="2007-10-19 16:00:00", select = id:Wspeed)
Oct4a<- subset(Allyears[[1]], timestep>="2007-10-19 16:00:00" & timestep<="2007-10-20 16:00:00", select = id:Wspeed)
Oct5a <- subset(Allyears[[1]], timestep>="2007-10-20 16:00:00 UTC" & timestep<="2007-10-21 16:00:00 UTC", select = id:Wspeed)
Oct6a <- subset(Allyears[[2]], timestep>="2008-10-28 16:00:00" & timestep<="2008-10-29 16:00:00", select = id:Wspeed)
Oct7a <- subset(Allyears[[2]], timestep>="2008-10-29 16:00:00" & timestep<="2008-10-30 16:00:00", select = id:Wspeed)
Oct8a <- subset(Allyears[[2]], timestep>="2008-10-30 16:00:00 UTC" & timestep<="2008-10-31 16:00:00 UTC", select = id:Wspeed)
Mar1a <- subset(Allyears[[2]], timestep>="2008-03-27 16:00:00 UTC" & timestep<="2008-03-28 16:00:00 UTC", select = id:Wspeed)
Mar2a <- subset(Allyears[[3]], timestep>="2009-03-13 16:00:00" & timestep<="2009-03-14 16:00:00", select = id:Wspeed)
Mar3a <- subset(Allyears[[3]], timestep>="2009-03-16 16:00:00" & timestep<="2009-03-17 16:00:00", select = id:Wspeed)
Mar4a <- subset(Allyears[[4]], timestep>="2010-03-16 16:00:00 UTC" & timestep<="2010-03-17 16:00:00 UTC", select = id:Wspeed)
Mar5a <- subset(Allyears[[4]], timestep>="2010-03-21 16:00:00" & timestep<="2010-03-22 16:00:00" , select = id:Wspeed)
Mar6a<- subset(Allyears[[4]], timestep>="2010-03-21 16:00:00" & timestep<="2010-03-22 16:00:00", select = id:Wspeed)
Alldays <- list(Oct1a,Oct2a,Oct3a,Oct4a,Oct5a,Oct6a,Oct7a,Oct8a,Mar1a,Mar2a,Mar3a,Mar4a,Mar5a,Mar6a)

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

Join <- lapply(1:14, function(n){
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
  write.csv(Birds[[k]], file = paste0("Birds_sec_",datetime,".csv"))
}
