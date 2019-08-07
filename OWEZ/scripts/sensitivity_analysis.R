library(ggmap)
library(ks)
library(MASS)
library(emdist)

setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/departures_95%_alldata_6h")

data1 <- read.csv('Departures_20071002_1000.csv',sep=',')
data2 <- read.csv('Departures_20071002_925.csv',sep=',')
data3 <- read.csv('Departures_20071002_850.csv',sep=',')
data4 <- read.csv('Departures_20071002_700.csv',sep=',')

k1 <- kde2d(data1$Long,data1$Lat,h=c(bandwidth.nrd(data1$Long),bandwidth.nrd(data1$Lat)))
k2 <- kde2d(cbind(data2$Long,data2$Lat))

a <- kde.test(x1=cbind(data1$Long,data1$Lat),x2=cbind(data2$Long,data2$Lat),h=c(bandwidth.nrd(data)))
kde.local.test(k1,k2)
#calculating earth mover's distance for a specific number of random points from my distribution
#as it can only deal with limited number of points

seed <- c(1,5,10,15,20)

set.seed() #setting seed to values of 1,5,10,15,20 and repeating process
rnum <- sample(nrow(data1),500)
emd2d(A=cbind(data3[,1],data3[,2]),
           B=cbind(data2[,1],data2[,2]))

#700 to 850
for (k in seed){
  set.seed(k) #setting seed to values of 1,5,10,15,20 and repeating process
  rnum <- sample(nrow(data1),500)
  a <- emd2d(A=cbind(data4[rnum,1],data4[rnum,2]),
             B=cbind(data3[rnum,1],data3[rnum,2]))
  print(a)
}
#700 to 925
for (k in seed){
  set.seed(k) #setting seed to values of 1,5,10,15,20 and repeating process
  rnum <- sample(nrow(data1),500)
  a <- emd2d(A=cbind(data4[rnum,1],data4[rnum,2]),
        B=cbind(data2[rnum,1],data2[rnum,2]))
  print(a)
}
#700 to 1000
for (k in seed){
  set.seed(k) #setting seed to values of 1,5,10,15,20 and repeating process
  rnum <- sample(nrow(data1),500)
  a <- emd2d(A=cbind(data4[rnum,1],data4[rnum,2]),
             B=cbind(data1[rnum,1],data1[rnum,2]))
  print(a)
}
#850 to 925
for (k in seed){
  set.seed(k) #setting seed to values of 1,5,10,15,20 and repeating process
  rnum <- sample(nrow(data1),500)
  a <- emd2d(A=cbind(data3[rnum,1],data3[rnum,2]),
             B=cbind(data2[rnum,1],data2[rnum,2]))
  print(a)
}
#850 to 1000
for (k in seed){
  set.seed(k) #setting seed to values of 1,5,10,15,20 and repeating process
  rnum <- sample(nrow(data1),500)
  a <- emd2d(A=cbind(data3[rnum,1],data3[rnum,2]),
             B=cbind(data1[rnum,1],data1[rnum,2]))
  print(a)
}
#925 to 1000
for (k in seed){
  set.seed(k) #setting seed to values of 1,5,10,15,20 and repeating process
  rnum <- sample(nrow(data1),500)
  a <- emd2d(A=cbind(data2[rnum,1],data2[rnum,2]),
             B=cbind(data1[rnum,1],data1[rnum,2]))
  print(a)
}

check <- as.data.frame(cbind(k1$x,k1$y))
register_google(key = "AIzaSyC-bw1FEcHmDesL-zAPgW9RaWwolld2gRw")

#creating map

NSea <- get_map(location = c(lon=8, lat=54.5110 ), 
                maptype = "terrain-background",  source = "stamen", color="bw", zoom=5) #add a map
S <- ggmap(NSea)
S

S+geom_density2d(data=check,aes(x=check$Long,y=check$Lat),h=c(bandwidth.nrd(check$Long),bandwidth.nrd(check$Lat)))
 
S+geom_point(data=check,aes(x=check$V1,y=check$V2))
