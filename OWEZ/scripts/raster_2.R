library(ggmap)

setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/Model/statistical_analysis/data/from_stat_an_script")
Autumn_rad <- read.csv("Autumn_rad.csv",sep=",")
Spring_rad <- read.csv("Spring_rad.csv",sep = ",")
Autumn_int_rad <- read.csv("Autumn_int_rad.csv",sep = ",")
Spring_int_rad <- read.csv( "Spring_int_rad.csv",sep = ",")
Departures_Autumn <- read.csv("Departures_Autumn.csv",sep = ",")
Departures_Spring <- read.csv("Departures_Spring.csv",sep = ",")
Departures_Autumn_int <- read.csv("Departures_Autumn_int.csv",sep = ",")
Departures_Spring_int <- read.csv("Departures_Spring_int.csv",sep = ",")


shapefile <- readOGR("C:/Users/mbradar/Documents/Merlin/OWEZ/Model", "world-country-boundaries")
shapefile_df <- fortify(shapefile)

#creating map
register_google(key = "AIzaSyC-bw1FEcHmDesL-zAPgW9RaWwolld2gRw")
NSea <- get_map(location = c(lon=4.7, lat=54.5110 ), 
                maptype = "toner-background",  source = "stamen", color="bw", zoom=5) #add a map
S <- ggmap(NSea)

world <- map_data("world")

#raster map + kernel density
s1 <- S+stat_density2d(data=Departures_Autumn_int,aes(x=Departures_Autumn_int$Long_start,y=Departures_Autumn_int$Lat_start,fill=..level..),
                 geom='polygon',alpha=0.3,n=c(360,180))+
  scale_fill_continuous(low="white",high="cyan4",limits=c(0,0.1))+guides(alpha="none")+
  theme(legend.key.width = unit(3,"cm"))

s2 <- S+stat_density2d(data=Departures_Spring_int,aes(x=Departures_Spring_int$Long_start,y=Departures_Spring_int$Lat_start,
                                                fill=..level..,alpha=0.3),
                 geom='polygon',n=c(360,180))+
  scale_fill_continuous(low="white",high="cyan4",limits=c(0,0.1))+guides(alpha="none")+
  theme(legend.key.width =   unit(1,"cm"))


windows(8,4)
sf<- ggarrange(s2,s1,labels = c("(a)","(b)"),font.label = list(size = 22),ncol=2,nrow = 1,
               widths=c(1,1,0.8,0.8),common.legend = TRUE,legend ="bottom")+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), legend.text = element_text(size = 4))
ggsave(filename=paste0("C:/Users/mbradar/surfdrive/Documents/Maja_PhD/first_paper_ver1/Paper figures/map_kernel.png"),sf,dpi=500)

#polygon map + kernel density
s1 <- ggplot(data=Departures_Spring_int,aes(x=Departures_Spring_int$Long_start,y=Departures_Spring_int$Lat_start))+
geom_polygon(data = world, aes(x=long, y = lat, group = group), colour="black",fill="gray84")+
stat_density2d(aes(fill=..level..),geom = 'polygon',alpha=0.3,n=c(360,180))+
scale_fill_gradient2(low="white",high="cyan4",limits=c(0,0.05))+
coord_map(xlim = c(-11, 20), ylim = c(49, 62))+
  theme_bw()+
  theme(panel.grid=element_blank(),legend.key.width = unit(1,"cm"))+
  xlab("Longitude")+ylab("Latitude")

 s2 <-  ggplot(data=Departures_Autumn_int,aes(x=Departures_Autumn_int$Long_start,
                                        y=Departures_Autumn_int$Lat_start)) +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), colour="black",fill="gray84")+
  stat_density2d(aes(fill=..level..),geom = 'polygon',alpha=0.3,n=c(360,180))+
  scale_fill_gradient2(low="white",high="cyan4",limits=c(0,0.05))+
  coord_map(xlim = c(-11, 20), ylim = c(49, 62))+
    theme_bw()+
    theme(panel.grid=element_blank(),legend.key.width =   unit(1,"cm"),axis.title = element_blank(),
          axis.text = element_blank())+
    xlab("Longitude")+ylab("Latitude")
 library(ggpubr)
 library(grid)
  windows(8,4)
  sf<- ggarrange(s1,s2,labels = c("(a)","(b)"),font.label = list(size = 18),ncol=2,nrow = 1,
                 widths=c(1,0.9),common.legend = TRUE,legend ="bottom")+
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), legend.text = element_text(size = 4))
  annotate_figure(sf,bottom=text_grob("Longitude"))
  ggsave(filename=paste0("C:/Users/mbradar/surfdrive/Documents/Maja_PhD/first_paper_ver1/Paper figures/polygon_kernel.png"),sf,dpi=500)

#polygon map + raster
  #create a random raster of certain resolution over the whole area
r1 <- raster(xmn=-18,xmx=30,ymn=45,ymx=65,resolution=c(0.25,0.25))
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
mat <- as.matrix(ttS)
colmat <- matrix(rich.colors(7,palette = "temperature", alpha=0.3)[cut(mat, 10)], nrow=nrow(mat), ncol=ncol(mat))  

ggplot(data=Departures_Autumn_int,aes(x=Departures_Autumn_int$Long_start,
                                      y=Departures_Autumn_int$Lat_start)) +
  coord_map(xlim = c(-11, 20), ylim = c(49, 62))+
  geom_polygon(data = world, aes(x=long, y = lat, group = group), colour="black",fill="gray84")+
  inset_raster(colmat,e[1],e[2],e[3],e[4])
