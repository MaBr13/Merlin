library(ggpubr)
library(grid)
library(ggplot2)

w1=data.frame(x =c(as.numeric(winddirmns[[6]]),as.numeric(winddirmns[6])),y=c(as.numeric(winddirmns[6]),2000000))
h1=data.frame(x =c(as.numeric(headmeans[[2]]),as.numeric(headmeans[[2]])),y=c(as.numeric(headmeans[[2]]),2000000))
wp1 <- ggplot(Departures_Spring, aes(x=winddir_mean)) + 
  geom_histogram(breaks=seq(0,360,10),col='white',fill="cyan4") +
  geom_path(data=w1,aes(x =x,y=y),arrow = arrow(),size=2,colour="red")+
  geom_path(data=h1,aes(x =x,y=y),arrow = arrow(),size=2,colour="gold")+
  coord_polar(start = 0) +
  scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
  theme_minimal()+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
        axis.text.x = element_text(size = 16,face = "bold",colour="black"),
        panel.grid.major=element_line(colour = "cyan4"))


w2=data.frame(x =c(as.numeric(winddirmns[[5]]),as.numeric(winddirmns[5])),y=c(as.numeric(winddirmns[5]),2000000))
h2=data.frame(x =c(as.numeric(headmeans[[1]]),as.numeric(headmeans[[1]])),y=c(as.numeric(headmeans[[1]]),2000000))
wp2 <- ggplot(Departures_Autumn, aes(x=winddir_mean)) + 
  geom_histogram(breaks=seq(0,360,10),col='white',fill="cyan4") +
  geom_path(data=w2,aes(x =x,y=y),arrow = arrow(),size=2,colour="red")+
  geom_path(data=h2,aes(x =x,y=y),arrow = arrow(),size=2,colour="gold")+
  coord_polar(start = 0) +
  scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
  theme_minimal()+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
        axis.text.x = element_text(size = 16,face = "bold",colour="black"),
        panel.grid.major=element_line(colour = "cyan4"))

  w3=data.frame(x =c(as.numeric(winddirmns[[8]]),as.numeric(winddirmns[8])),y=c(as.numeric(winddirmns[8]),15000))
  h3=data.frame(x =c(as.numeric(headmeans[[4]]),as.numeric(headmeans[[4]])),y=c(as.numeric(headmeans[[4]]),15000))
  wp3 <- ggplot(Departures_Spring_int, aes(x=winddir_start)) + 
    geom_histogram(breaks=seq(0,360,10),col='white',fill="cyan4") +
    geom_path(data=w3,aes(x =x,y=y),arrow = arrow(),size=2,colour="red")+
    geom_path(data=h3,aes(x =x,y=y),arrow = arrow(),size=2,colour="gold")+
    coord_polar(start = 0) +
    scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
    theme_minimal()+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
          axis.text.x = element_text(size = 16,face = "bold",colour="black"),
          panel.grid.major=element_line(colour = "cyan4"))
  
w4=data.frame(x =c(as.numeric(winddirmns[[7]]),as.numeric(winddirmns[7])),y=c(as.numeric(winddirmns[7]),15000))
h4=data.frame(x =c(as.numeric(headmeans[[3]]),as.numeric(headmeans[[3]])),y=c(as.numeric(headmeans[[3]]),15000))
wp4 <- ggplot(Departures_Autumn_int, aes(x=winddir_start)) + 
  geom_histogram(breaks=seq(0,360,10),col='white',fill="cyan4") +
  geom_path(data=w4,aes(x =x,y=y),arrow = arrow(),size=2,colour="red")+
  geom_path(data=h4,aes(x =x,y=y),arrow = arrow(),size=2,colour="gold")+
  coord_polar(start = 0) +
  scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
  theme_minimal()+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
        axis.text.x = element_text(size = 16,face = "bold",colour="black"),
        panel.grid.major=element_line(colour = "cyan4"))

windows(8,8)
fwinds <- ggarrange(wp1,wp2,wp3,wp4,labels = c("(a)","(b)","",""),font.label = list(size = 16),ncol=2,nrow = 2,
                   widths=c(1,1,0.8,0.8),common.legend = TRUE,legend ="bottom",label.x=c(0.86,.86,.86,.86),label.y = c(.94,.94,.94,.94))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), legend.text = element_text(size = 4))
ggsave(filename=paste0("C:/Users/mbradar/surfdrive/Documents/Maja_PhD/first_paper_ver1/Paper figures/final_winds.png"),fwinds,dpi=500)
