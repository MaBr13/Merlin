###clean up ecmwf data



ecmwf <- read.csv('march2008_ecmwf.csv', sep=',')
owez <- read.csv('march2008_owez.csv',sep=';')

library(lubridate)
Sys.setenv(TZ="UTC")
Sys.setlocale(category = "LC_ALL", locale = "English_United Kingdom.1252")#set the time on your computer to match

for(k in 1:nrow(owez)){
  owez[k,9] <- with(owez[k,], ymd(paste(jaar,maand,dag, sep=' ')))
}
colnames(owez)[9] <- "date"
owez[owez==-99999] <- NA
owez <- owez %>% drop_na()


owez <- owez %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))


for (k in 1:nrow(ecmwf)){
  heading<- atan2(ecmwf[k,2],ecmwf[k,3])
  ecmwf[k,5]<-sqrt((ecmwf[k,2]^2)+(ecmwf[k,3]^2)) 
  ecmwf[k,6] <- heading*(180/pi)
  ecmwf[k,6][ecmwf[k,6] < 0] <- 360 + ecmwf[k,6][ecmwf[k,6] < 0]
}


library(circular)
library(dplyr)
owez <- 
  owez %>%
  group_by(date) %>%
  summarise(m.dir=mean.circular(winddir),spd = mean(try))

#fill in the missing times in the table
library(tidyverse)
owez <- owez %>%
  complete(date = seq(date[1], date[31], by = "1 day"),
           fill = list(Val1 = 0, Val2 = 0))

#create timestamps

library(lubridate)
Sys.setenv(TZ="UTC")
Sys.setlocale(category = "LC_ALL", locale = "English_United Kingdom.1252")#set the time on your computer to match
owez$date <- as.Date(owez$date) 

#fill in the NA values of number of tracks as zeroes

trackcount2[is.na(trackcount2)] <- 0