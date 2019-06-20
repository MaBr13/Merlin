path <- setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/radar_data")
listcsv <- dir(path,pattern = "*.csv",ignore.case = FALSE)

autumn <- listcsv[c(26:30,32:33)]
spring <- listcsv[c(31,34:38)]



Birdsa_list <- lapply(autumn,read.csv,sep=',',header= TRUE)
Birdss_list <- lapply(spring,read.csv,sep=',',header= TRUE)


Birdsa <- do.call("rbind",Birdsa_list)

Birdss <- do.call("rbind",Birdss_list)



library(ggplot2)

ggplot(Birdsa,aes(b.heading))+
  geom_histogram(breaks=seq(0,360,15))+
 scale_colour_manual(values = c("black"))+
scale_x_continuous("",limits=c(0,360), breaks = seq(0,360,15))


ggplot(Birdss,aes(b.heading))+
  geom_histogram(breaks=seq(0,360,15))+
  scale_colour_manual(values = c("black"))+
  scale_x_continuous("",limits=c(0,360), breaks = seq(0,360,15))

