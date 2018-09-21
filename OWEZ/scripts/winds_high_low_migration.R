#################################################################
###################MERLIN RADAR DATA#############################
#################################################################

##this script visualizes winds during high and low migration in order to check for obvious wind effects
##Spring and Autumn are lists that contain spring and autumn seasons of all years stored as separate
##parts of the list


##first we do some of the data categorization (we make speed bins for colouring our wind rose)

for(k in 1:length(Spring)){
  
  Spring[[k]]$Migrwind <- cut(Spring[[k]]$Mean.wspeed,breaks=c(0,10,20,30,40,50,60,70,80,90,100,150), 
                                  labels = c("0-10", "10-20", "20-30","30-40", "40-50",
                                             "50-60","60-70","70-80","80-90","90-100", ">100"))
  
}

for(k in 1:length(Autumn)){
  
  Autumn[[k]]$Migrwind <- cut(Autumn[[k]]$Mean.wspeed,breaks=c(0,10,20,30,40,50,60,70,80,90,100,150), 
                              labels = c("0-10", "10-20", "20-30","30-40", "40-50",
                                         "50-60","60-70","70-80","80-90","90-100", ">100"))
  
}

##then we put the into the right (ascending) order

for (k in 1:length(Spring)){
 Spring[[k]]$Migrwind <- factor(Spring[[k]]$Migrwind, levels = rev(levels(Spring[[k]]$Migrwind)))
  
}

for (k in 1:length(Autumn)){
  Autumn[[k]]$Migrwind <- factor(Autumn[[k]]$Migrwind, levels = rev(levels(Autumn[[k]]$Migrwind)))
  
}

###High migration
##we filter out the data that represents high migration (more than 2500 tracks per hour)
windS <- list()

for(k in 1:length(Spring)){
  
  windS[[k]] <- subset(Spring[[k]], Nr.tracks>2500, select=Timestamp:Migrwind)
}

windA <- list()

for(k in 1:length(Autumn)){
  
  windA[[k]] <- subset(Autumn[[k]], Nr.tracks>2500, select=Timestamp:Migrwind)
}

##we plot the wind roses for
#Spring for specific year
highS <- ggplot(windS[[2]], aes(Mean.wdir)) + 
  stat_bin(stat="identity", aes(fill=windS[[2]]$Migrwind, colour=windS[[2]]$Migrwind),breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values= c("black", "black","black","black","black","black","black","black","black",
                      "black", "black", "black"),name="Mean Wind speed ph (kph)", drop=F)+
  scale_fill_manual(values= c("grey", "mediumblue","royalblue4","midnightblue", "navy","mediumpurple4","mediumorchid4",
                              "magenta4","maroon","orangered4","orangered2", "coral3", "coral"),name="Mean Wind speed ph (kph)", drop=F)+
  ggtitle("High migration") +
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), 
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  ylab("Hours with high migration")+ylim(0,10)+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

#Autumn for specific year
highA <- ggplot(windA[[2]], aes(Mean.wdir)) + 
  stat_bin(stat="identity", aes(fill=windA[[2]]$Migrwind, colour=windA[[2]]$Migrwind),breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values= c("black", "black","black","black","black","black","black","black","black",
                                "black", "black", "black"),name="Mean Wind speed ph (kph)", drop=F)+
  scale_fill_manual(values= c("grey", "mediumblue","royalblue4","midnightblue", "navy","mediumpurple4","mediumorchid4",
                              "magenta4","maroon","orangered4","orangered2", "coral3", "coral"),name="Mean Wind speed ph (kph)", drop=F)+
  ggtitle("High migration") +
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  ylab("Hours with high migration")+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

#Spring for all years

for(k in 1:length(windS)){{
  s <- as.data.frame(windS[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(windS[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_histogram(stat="identity", aes(fill=windS[[k]]$Migrwind, colour=windS[[k]]$Migrwind)) +
    scale_colour_manual(name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_manual(name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Mean wind direction per hour (degrees) Spring High migration",' ', datetime,
                  sep='')) +
    theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
    ylab("Mean number of tracks per hour")+ylim(0,4000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirhighS','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}




for(k in 1:length(windA)){{
  s <- as.data.frame(windA[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(windA[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=windA[[k]]$Migrwind, colour=windA[[k]]$Migrwind)) +
    scale_colour_hue(name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_hue(name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Mean wind direction per hour (degrees) Autumn High migration",' ', datetime,
                  sep='')) +
    theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
    ylab("Mean number of tracks per hour")+ylim(0,5000)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirhighA','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

##Low migration
#we filter out the data for low migration

windSL <- list()

for(k in 1:length(Spring)){
  
  windSL[[k]] <- subset(Spring[[k]], Nr.tracks<100, select=Timestamp:Migrwind)
}

windAL <- list()

for(k in 1:length(Autumn)){
  
  windAL[[k]] <- subset(Autumn[[k]], Nr.tracks<100, select=Timestamp:Migrwind)
}

##plot the wind roses for all the years and save them in a specific folder
#Spring
for(k in 1:length(windSL)){{
  s <- as.data.frame(windSL[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(windSL[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=windSL[[k]]$Migrwind, colour=windSL[[k]]$Migrwind)) +
    scale_colour_hue(name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_hue(name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Mean wind direction per hour (degrees) Spring Low migration",' ', datetime,
                  sep='')) +
    theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
    ylab("Mean number of tracks per hour")+ylim(0,100)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirlowS','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

#Autumn
for(k in 1:length(windAL)){{
  s <- as.data.frame(windAL[[k]])
  datetime <- paste0(format(s[1,6], format="%Y"))
  plot <- ggplot(windAL[[k]], aes(Mean.wdir,Nr.tracks)) + 
    geom_bar(stat="identity", aes(fill=windAL[[k]]$Migrwind, colour=windAL[[k]]$Migrwind)) +
    scale_colour_hue(name="Mean Wind speed ph (kph)", drop=F)+
    scale_fill_hue(name="Mean Wind speed ph (kph)", drop=F)+
    ggtitle(paste("Mean wind direction per hour (degrees) Autumn Low migration",' ', datetime,
                  sep='')) +
    theme(axis.title.y = element_text(size=14), axis.title.x = element_blank(), plot.title = element_text(size = 16))+
    ylab("Mean number of tracks per hour")+ylim(0,100)+
    coord_polar(start = 0) +
    scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  ggsave(filename=paste('wrwswdirlowA','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}

#Spring for a specific year

lowS <- ggplot(windSL[[2]], aes(Mean.wdir)) + 
  stat_bin(stat="identity", aes(fill=windSL[[2]]$Migrwind, colour=windSL[[2]]$Migrwind),breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values= c("black", "black","black","black","black","black","black","black","black",
                                "black", "black", "black"),name="Mean Wind speed ph (kph)", drop=F)+
  scale_fill_manual(values= c("grey", "mediumblue","royalblue4","midnightblue", "navy","mediumpurple4","mediumorchid4",
                              "magenta4","maroon","orangered4","orangered2", "coral3", "coral"),name="Mean Wind speed ph (kph)", drop=F)+
  ggtitle("Low migration") +
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  ylab("Hours with low migration")+ylim(0,60)+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

#Autumn for a specific year

lowA <- ggplot(windAL[[2]], aes(Mean.wdir)) + 
  stat_bin(stat="identity", aes(fill=windAL[[2]]$Migrwind, colour=windAL[[2]]$Migrwind),breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
  scale_colour_manual(values= c("black", "black","black","black","black","black","black","black","black",
                                "black", "black", "black"),name="Mean Wind speed ph (kph)", drop=F)+
  scale_fill_manual(values= c("grey", "mediumblue","royalblue4","midnightblue", "navy","mediumpurple4","mediumorchid4",
                              "magenta4","maroon","orangered4","orangered2", "coral3", "coral"),name="Mean Wind speed ph (kph)", drop=F)+
  ggtitle("Low migration") +
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), 
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  ylab("Hours with low migration")+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0,30,60,90,120,150,180,210,240,270,300,330,360))

