##########SCRIPT FOR MERLIN RADAR DATA##############
#this is a script that uses total number of observations to plot them
#Allyears is a list that contacins the datasets from all the observed years (each year as a separate piece of list)
#The loop automatically goes through all of the years, produces plots and saves them in a specified folder

require(ggplot2)

#plot nocturnal and diurnal migrants during year
for(k in 1:length(Allyears)){{
  s <- as.data.frame(Allyears[[k]])
  datetime <- paste0(format(s[1,15], format="%Y"))
  plot <- ggplot(Allyears[[k]], aes(light, id))+
    ylim(0,8000000)+
    geom_bar(stat="identity" ,aes(colour=Allyears[[k]]$Aspeed, fill=Allyears[[k]]$Aspeed), show.legend = T) +
    scale_fill_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    scale_colour_manual(values = c("coral3", "forestgreen", "grey0", "grey"), name="Air speed (kph)", drop=F)+
    ggtitle(paste("Total number of migrants and their air speed",' ', datetime,sep='')) +
    xlab("Time of a day") + ylab("Number of tracks") + 
    scale_x_continuous(breaks = 0:1,labels = paste0(c("Night", "Day")))+
  ggsave(filename=paste('nctrnST','_',datetime, ".jpeg", sep=''), 
         path="C:/Users/mbradar/Documents/Radar projects/Merlin/OWEZ/plots/horizontal",scale=2)
}
  print(plot)
}


#wind rose (wind speed and direction)

for(k in 1:length(Allyears)){{
  s <- as.data.frame(Allyears[[k]])
  datetime <- paste0(format(s[1,15], format="%Y"))
  plot <- ggplot(Allyears[[k]], aes(winddir,Wspeed)) + 
  geom_bar(stat="identity", aes(fill=Allyears[[k]]$Wspeed, colour=Allyears[[k]]$Wspeed)) +
  scale_colour_manual(values = c("steelblue", "navy", "yellow",  "grey"), name="Wind speed (kph)", drop=F)+
  scale_fill_manual(values = c("steelblue","navy", "yellow",  "grey"), name="Wind speed (kph)", drop=F)+
  ylab("Mean wind direction (degrees)")+ylim(0,300)+
  coord_polar(start = 0) +
  scale_x_continuous("",limits=c(0,360), breaks = c(0, 90,180,270,360))
  
}
print(plot)
}

