library(sp)
library(rgdal)
library(rgeos)
library(circular)
memory.size(max=19000)

library(raster)
r1 <- raster(xmn=-18,xmx=30,ymn=45,ymx=65,resolution=c(0.25,0.25))
vals <- 1:ncell(r1)
r1 <- setValues(r1,vals)
winds_A_int <- Departures_Autumn_int
winds_S_int <- Departures_Spring_int

winds_A_int$grid <- raster::extract(r1, winds_A_int[,c('Long_start', 'Lat_start')])
winds_A_int$winddir_start <- circular(winds_A_int$winddir_start,units = "degrees",modulo="2pi",template = 'geographic')

winds_S_int$grid <- raster::extract(r1, winds_S_int[,c('Long_start', 'Lat_start')])
winds_S_int$winddir_start <- circular(winds_S_int$winddir_start,units = "degrees",modulo="2pi",template = 'geographic')

library(dplyr)

winds_A_int_m <- 
  winds_A_int%>%
  group_by(grid) %>%
  summarise(mwind=mean.circular(winddir_start),obs=length(grid))

winds_S_int_m <- 
  winds_S_int%>%
  group_by(grid) %>%
  summarise(mwind=mean.circular(winddir_start),obs=length(grid))

#weighted mean

prA <- c()
cA <- c()
for(k in 1:nrow(winds_A_int_m)){
  winds_A_int_m[k,4] <- winds_A_int_m[k,2]* winds_A_int_m[k,3]
}
  
wmA <- sum(winds_A_int_m$mwind.1)/sum(winds_A_int_m$obs)

prS <- c()
for(k in 1:nrow(winds_S_int_m)){
  winds_S_int_m[k,4] <- winds_S_int_m[k,2]* winds_S_int_m[k,3]
}

wmS <- sum(winds_S_int_m$mwind.1)/sum(winds_S_int_m$obs)


watson.williams.test(winds_A_int_m$mwind,winds_S_int_m$mwind)

cdat <- c(winds_A_int_m$mwind,winds_S_int_m$mwind)
ndat <- c(length(winds_A_int_m$mwind),length(winds_S_int_m$mwind))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)


library(googleway)

## genreate some random data
key <- register_google(key = "AIzaSyC-bw1FEcHmDesL-zAPgW9RaWwolld2gRw")
NSea <- get_map(location = c(lon=4.7, lat=54.5110 ), 
                maptype = "toner-lite",  source = "stamen", color="bw", zoom=5) #add a map


##key <- 'your_api_key'

google_map(data = Departures_Autumn_int, key = key) %>%
  add_heatmap(weight = "", option_radius = 0.1)
S <- ggmap(NSea)
S
ggplot(data=Departures_Autumn_int, 
                  aes(x=Departures_Autumn_int$Long_start,
                            y=Departures_Autumn_int$Lat_start))

#this one works, but I don't like it because I lose a lot of info for the story when using kernel density
S+ stat_density2d(data=Departures_Autumn_int,aes(x=Departures_Autumn_int$Long_start,
                     y=Departures_Autumn_int$Lat_start,fill=..level..,alpha=..level..),geom='polygon') +
  scale_fill_continuous(low="white",high="purple") +
  guides(alpha="none") +
  ylab("Latitude")+
  xlab  ("Longitude")


####making raster plots of the data
#with kernel density

library(MASS)
pts <- Departures_Autumn_int

#without kernel density
coordinates(pts)=~Long_start+Lat_start
gridded(pts) = TRUE
library(raster)

longlat <- data.frame(pts$Long_start,pts$Lat_start)
r <- raster::rasterize(longlat, r1, fun="count")
e=raster::extent(-10,20,49,62)
tt=raster::crop(r,e)
plot(tt,xlim=c(-10,20))
list('sp.lines', boundaries, lwd=0.5),

library(maps)
library(mapdata)
library(maptools)
library(lattice)
library(gplots)

ext <- as.vector(e)

boundaries <- purrr::map('worldHires',
                  xlim=ext[1:2], ylim=ext[3:4],
                  plot=FALSE)
boundaries <- map2SpatialLines(boundaries,
                               proj4string=CRS(projection(tt)))

rgb.palette <- colorRampPalette(c("snow1","snow2","coral","orange","firebrick"),
                                space = "rgb",interpolate=c("spline"))

plot(tt)
plot(boundaries,add=TRUE)
       sp.layout=list('sp.lines', boundaries, lwd=0.5))


library(rasterVis)
levelplot(tt,col.regions=terrain.colors(n=255,rev=TRUE),margin=FALSE,
          colorkey=list(height=.3),
          scales = list(col = "black")) +
  layer(sp.lines(boundaries, lwd=0.5))

plot(r)
  
plot(r,xlim=c(xmin(r), xmax(r)), ylim=c(ymin(r), ymax(r)))
plot(Europe$geometry,add=TRUE)

S+geom_bin2d(data=Departures_Autumn_int,aes(x=Departures_Autumn_int$Long_start,
                                                 y=Departures_Autumn_int$Lat_start),alpha=0.8,binwidth=c(0.25,0.25)) +
  scale_fill_continuous(low="grey",high="darkcyan") +
  guides(alpha="none") +
  ylab("Latitude")+
  xlab  ("Longitude")
