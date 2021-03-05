#load required packages
library(sp)
library(rgdal)
library(rgeos)
library(circular)
library(dplyr)
library(maps)
library(mapdata)
library(maptools)
library(rasterVis)
library(raster)
#increase the memory size to be able to allocate large rasters
memory.size(max=19000)
#create a random raster of certain resolution over the whole area
r1 <- raster(xmn=-18,xmx=30,ymn=45,ymx=65,resolution=c(0.25,0.25))
#give values to individual raster cells
vals <- 1:ncell(r1)
r1 <- setValues(r1,vals)
#assign grid to each point in your data frame and convert wind direction columns into circulars
Departures_Autumn_int$grid <- raster::extract(r1, Departures_Autumn_int[,c('Long_start', 'Lat_start')])
Departures_Autumn_int$winddir_start <- circular(Departures_Autumn_int$winddir_start,units = "degrees",
                                                modulo="2pi",template = 'geographic')
Departures_Spring_int$grid <- raster::extract(r1, Departures_Spring_int[,c('Long_start', 'Lat_start')])
Departures_Spring_int$winddir_start <- circular(Departures_Spring_int$winddir_start,units = "degrees",
                                                modulo="2pi",template = 'geographic')
Departures_Autumn$winddir_mean <- circular(Departures_Autumn$winddir_mean,units = "degrees",modulo="2pi",
                                           template = 'geographic')
Departures_Spring$winddir_mean <- circular(Departures_Spring$winddir_mean,units = "degrees",modulo="2pi",
                                           template = 'geographic')
#summarize data per grid cell to be able to calculate weighted means
means_A_int <- 
  Departures_Autumn_int%>%
  group_by(grid) %>%
  summarise(mwind=mean.circular(winddir_start),mwspeed=mean(windspeedms_start),obs=length(grid))
means_A_int <- filter(means_A_int, mwind>0)
means_S_int <- 
  Departures_Spring_int%>%
  group_by(grid) %>%
  summarise(mwind=mean.circular(winddir_start),mwspeed=mean(windspeedms_start),obs=length(grid))
means_S_int<- filter(means_S_int, mwind>0)
  
means_A <- Departures_Autumn%>%
  group_by(long,lat) %>%
  summarise(mwind=mean.circular(winddir_mean),mwspeed=mean(windspeedms_mean),obs=length(grid))
means_A$mwind <- circular(means_A$mwind,units = "degrees",modulo="2pi",
                          template = 'geographic') 
means_S <-   Departures_Spring%>%
  group_by(long,lat) %>%
  summarise(mwind=mean.circular(winddir_mean),mwspeed=mean(windspeedms_mean),obs=length(grid))
means_S$mwind <- circular(means_S$mwind,units = "degrees",modulo="2pi",
                          template = 'geographic') 

#calculate weighted mean for each of the parameters
for(k in 1:nrow(means_A_int)){
  means_A_int[k,5] <- means_A_int[k,3]* means_A_int[k,4]
}
  
wdmAint <- weighted.mean.circular(x=means_A_int$mwind,w=means_A_int$obs)
wsmAint <- sum(means_A_int$mwspeed.1)/sum(means_A_int$obs)


for(k in 1:nrow(means_S_int)){
  means_S_int[k,5] <- means_S_int[k,3]* means_S_int[k,4]
}

wdmSint <-  weighted.mean.circular(x=means_S_int$mwind,w=means_S_int$obs)
wsmSint <- sum(means_S_int$mwspeed.1)/sum(means_S_int$obs)

wdmA <- mean.circular(means_A$mwind)
wdmS <- mean.circular(means_S$mwind)
wsmA <- mean(means_A$mwspeed)
wsmS <- mean(means_S$mwspeed)

watson.williams.test(list(means_A$mwind,means_A_int$mwind))
watson.williams.test(list(means_S$mwind,means_S_int$mwind))
watson.williams.test(list(means_A$mwind,means_S$mwind))
watson.williams.test(list(means_A_int$mwind,means_S_int$mwind))

cdat <- c(means_S$mwind,means_S_int$mwind)
ndat <- c(length(means_S$mwind),length(means_S_int$mwind))
g <- length(ndat)

YgObs <- YgVal(cdat,ndat,g)
p.value <- pchisq(YgObs,g-1,lower.tail = FALSE)


CSUScores <- CosSinUniScores(cdat)
WgObs <- WgVal(CSUScores,ndat,g);pchisq(WgObs,2*(g-1),lower.tail = F)
####make raster plots of the data
#copy your data into a different object to avoid changing the original
ptsS <- Departures_Spring_int
ptsA <- Departures_Autumn_int
#extract lat and long from data frame
longlatS <- data.frame(ptsS$Long_start,ptsS$Lat_start)
longlatA <- data.frame(ptsA$Long_start,ptsA$Lat_start)
#rasterize your data based on the same raster from the beginning and count number of points per grid cell
rS <- raster::rasterize(longlatS, r1, fun="count")
rA <- raster::rasterize(longlatA, r1, fun="count")
#determine the lat and long extent of your raster
e=raster::extent(-10,20,49,62)
#crop rasters based on the extent
ttS=raster::crop(rS,e)
ttA=raster::crop(rA,e)

#load map of the same extent as rasters
ext <- as.vector(e)
boundaries <- maps::map('worldHires',xlim=ext[1:2], ylim=ext[3:4],col = "blue",fill=T,add=TRUE)
#turn map boundaries into spatial polygons and lines and add projection based on raster
boundaries1 <- map2SpatialPolygons(boundaries,IDs=sapply(strsplit(boundaries$names, ":"), function(x) x[1]),
                                  proj4string=CRS(projection(ttS)))
boundaries2 <- map2SpatialLines(boundaries,proj4string=CRS(projection(ttS)))
#create a colour palette for your plot
rgb.palette <- colorRampPalette(c("#f6eff7","#1c9099","#016c59"),space="rgb")
#make background colour of your raster match the colour of grid cells with lowest values
myTheme <- BTCTheme()
myTheme$panel.background$col = 'bisque' 
#make land/sea difference
land <- mask(r1, boundaries)
catTheme <- rasterTheme(panel.background = list(col='white'))
#create level plots

p1 <-  levelplot(ttS,col.regions=rgb.palette,margin=FALSE,colorkey=list(height=.3),
                scales = list(col = "black"),alpha.regions=0.9,par.settings = catTheme) +
  layer(sp.polygons(boundaries1, lwd=0.8,fill ="gray84"),under=TRUE)+
  layer(sp.lines(boundaries2,lwd=0.8))
  
grid.text("H",x=2.5,y=1.5,gp = gpar(cex=1.6))
p2 <- levelplot(ttA,col.regions=rgb.palette,margin=FALSE,colorkey=list(height=.3),
                scales = list(col = "black"),par.settings = catTheme) +
  layer(sp.polygons(boundaries1, lwd=0.8,fill ="gray84"),under=TRUE)+
  layer(sp.lines(boundaries2,lwd=0.8))
#combine two level plots and plot them
comb_levObj <- c(p1, p2, layout = c(2, 1), merge.legends = FALSE)
print(comb_levObj)

a <- grid.arrange(p1,p2,nrow=1,ncol=2)
a+text_grob("maja",x=unit(2,"npc"),y = unit(.51, "npc"), gp=gpar(fontsize=20, col="grey"))
