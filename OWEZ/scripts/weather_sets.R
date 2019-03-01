ecmwf <- read.csv('ecmfw.csv',sep=',')

ecmwf$date <- as.Date(ecmwf$date)

library(ggplot2)

png(paste0("C:/Users/mbradar/Documents/Merlin/","weather_ts",".png"),height=9,width=40,unit="in",res=500)
ggplot(ecmwf, aes(date)) +
  geom_point(stat="count") +
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=14), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=12), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
  scale_x_date(date_breaks="days", date_labels="%d", limits=c(as.Date("2007-11-01"), as.Date("2009-01-01")))
dev.off()

library(dplyr)
library(tidyverse)
check <- 
  ecmwf %>%
  group_by(date) %>%
  summarise(obs=length(gh_925))

check <- check %>%
  complete(date = seq(date[1], date[932], by = "1 day"),
           fill = list(Val1 = 0, Val2 = 0))
check[is.na(check)] <- 0
sel <- subset(check,obs==0)

