library(emdist)

setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/statistical_analysis/data/departures")

data1 <- read.csv('Departures_20100326_1000.csv',sep=',',colClasses = replace(rep("NULL", 14),c(1:2),NA))
data2 <- read.csv('Departures_20100326_925.csv',sep=',',colClasses = replace(rep("NULL", 14),c(1:2),NA))
data3 <- read.csv('Departures_20100326_850.csv',sep=',',colClasses = replace(rep("NULL", 14),c(1:2),NA))


#850 to 925
a <- c()

for (k in seed){
  set.seed(k) #setting seed to values of 1,5,10,15,20 and repeating process
  rnum <- sample(nrow(data1),500)
  a[k]<- emd2d(A=cbind(data3[rnum,1],data3[rnum,2]),
             B=cbind(data2[rnum,1],data2[rnum,2]))
}
emd <- na.omit(a)
(sum(emd))/5

#850 to 1000
for (k in seed){
  set.seed(k) #setting seed to values of 1,5,10,15,20 and repeating process
  rnum <- sample(nrow(data1),500)
  a[k] <- emd2d(A=cbind(data3[rnum,1],data3[rnum,2]),
             B=cbind(data1[rnum,1],data1[rnum,2]))
}
emd <- na.omit(a)
(sum(emd))/5
#925 to 1000
for (k in seed){
  set.seed(k) #setting seed to values of 1,5,10,15,20 and repeating process
  rnum <- sample(nrow(data1),500)
  a[k] <- emd2d(A=cbind(data2[rnum,1],data2[rnum,2]),
             B=cbind(data1[rnum,1],data1[rnum,2]))
}
emd <- na.omit(a)
(sum(emd))/5





check <- as.data.frame(cbind(k1$x,k1$y))
register_google(key = "AIzaSyC-bw1FEcHmDesL-zAPgW9RaWwolld2gRw")

#creating map

NSea <- get_map(location = c(lon=8, lat=54.5110 ), 
                maptype = "terrain-background",  source = "stamen", color="bw", zoom=5) #add a map
S <- ggmap(NSea)
S

S+geom_density2d(data=check,aes(x=check$Long,y=check$Lat),h=c(bandwidth.nrd(check$Long),bandwidth.nrd(check$Lat)))
 
S+geom_point(data=check,aes(x=check$V1,y=check$V2))
