library(ggpubr)
library(grid)
library(ggplot2)

w1=data.frame(x =c(as.numeric(winddirmns[[7]]),as.numeric(winddirmns[7])),y=c(0,12))
p1=data.frame(x =c(as.numeric(headmeans[[7]]),as.numeric(headmeans[[7]])),y=c(0,12))

wp1 <- ggplot(Departures_Spring, aes(x=mean_winddir)) + 
  geom_histogram(breaks=seq(0,360,10),col='white',fill="cyan4") +
  geom_path(data=w1,aes(x =x,y=y),arrow = arrow(),size=2,colour="navyblue")+
  geom_path(data=p1,aes(x =x,y=y),arrow = arrow(),size=2,colour="gold")+
  coord_polar(start = 0, clip = "off") +
  scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
  theme_minimal()+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
        axis.text.x = element_text(size = 16,face = "bold",colour="cyan4"),
        panel.grid.major=element_line(colour = "cyan4"))

w2=data.frame(x =c(as.numeric(winddirmns[[8]]),as.numeric(winddirmns[8])),y=c(0,12))
p2=data.frame(x =c(as.numeric(headmeans[[8]]),as.numeric(headmeans[[8]])),y=c(0,12))
wp2 <- ggplot(Departures_Autumn, aes(x=mean_winddir)) + 
  geom_histogram(breaks=seq(0,360,10),col='white',fill="cyan4") +
  geom_path(data=w2,aes(x =x,y=y),arrow = arrow(),size=2,colour="navyblue")+
  geom_path(data=p2,aes(x =x,y=y),arrow = arrow(),size=2,colour="gold")+
  coord_polar(start = 0, clip = "off") +
  scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
  theme_minimal()+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
        axis.text.x = element_text(size = 16,face = "bold",colour="cyan4"),
        panel.grid.major=element_line(colour = "cyan4"))

  w3=data.frame(x =c(as.numeric(winddirmns[[5]]),as.numeric(winddirmns[5])),y=c(0,2.3))
  p3=data.frame(x =c(as.numeric(headmeans[[5]]),as.numeric(headmeans[[5]])),y=c(0,2.3))
  wp3 <- ggplot(Departures_Spring_int, aes(x=winddir_start)) + 
    geom_histogram(breaks=seq(0,360,10),col='white',fill="cyan4") +
    geom_path(data=w3,aes(x =x,y=y),arrow = arrow(),size=2,colour="navyblue")+
    geom_path(data=p3,aes(x =x,y=y),arrow = arrow(),size=2,colour="gold")+
    coord_polar(start = 0,clip = "off") +
    scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
    theme_minimal()+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
          axis.text.x = element_text(size = 16,face = "bold",colour="cyan4"),
          panel.grid.major=element_line(colour = "cyan4"))
  
w4=data.frame(x =c(as.numeric(winddirmns[[6]]),as.numeric(winddirmns[6])),y=c(0,2.3))
p4=data.frame(x =c(as.numeric(headmeans[[6]]),as.numeric(headmeans[[6]])),y=c(0,2.3))
wp4 <- ggplot(Departures_Autumn_int, aes(x=winddir_start)) + 
  geom_histogram(breaks=seq(0,360,10),col='white',fill="cyan4") +
  geom_path(data=w4,aes(x =x,y=y),arrow = arrow(),size=2,colour="navyblue")+
  geom_path(data=p4,aes(x =x,y=y),arrow = arrow(),size=2,colour="gold")+
  coord_polar(start = 0, clip = "off") +
  scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
  theme_minimal()+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
        axis.text.x = element_text(size = 16,face = "bold",colour="cyan4"),
        panel.grid.major=element_line(colour = "cyan4"))

error <- list(wind_ass[[7]],wind_ass[[5]],wind_ass[[8]],wind_ass[[6]])
error <- lapply(error,as.data.frame)
for(k in 1:length (error)){
  error[[k]]$cat <- k
}

error1 <- do.call("rbind",error)
names(error1) <- c("wind_ass","cat")
error1$col <- ifelse(error1$cat<3,1,3)

windows(6,3)
er <- ggplot(error1, aes(x = error1$cat, y = error1$wind_ass)) +
  geom_boxplot(aes(group=error1$cat,y=error1$wind_ass,fill=as.factor(error1$col)),outlier.shape = 1, 
               outlier.color="black",color="black")+
  scale_fill_manual(values = c("white","#1c9099"),name="",labels=c("Spring","Autumn"))+
  geom_hline(yintercept=0,col="red")+
  ylab('Wind assistance (m/s)')+
  scale_x_continuous(breaks=c(1:4),labels=c("WMN","IMN","WMN","IMN"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 10,face = "bold",colour="black"),
        axis.text.y = element_text(size = 10,face = "bold",colour="black"),
        panel.grid.minor  =element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10,face = "bold",colour="black"),
        panel.grid = element_blank(),legend.position = "bottom",
        legend.text = element_text(size=10,face = "bold",color = "black"))

windows(6.5,8)
fwinds <- ggarrange(ggarrange(ggarrange(wp1,wp2,wp3,wp4,labels = c("WMN","","IMN",""),font.label = list(size = 10),ncol=2,nrow = 2,
                   label.x=c(0.86,.86,.9,.86),label.y = c(.94,.94,.94,.94)),
                   er,ncol=1,nrow=2,labels = c("(a)"),heights = c(2,1),vjust = 1),
                   ncol=1,nrow=1,labels=c("(b)"),vjust=27)+
  theme(plot.margin = unit(c(0.1, 0, 0.1, 0), "cm"), legend.text = element_text(size = 2))
ggsave(filename=paste0("C:/Users/mbradar/surfdrive/Documents/Maja_PhD/first_paper_ver1/Paper figures/final_winds_ass_f1.png"),fwinds,dpi=500)


w5=data.frame(x =c(as.numeric(winddirmns[[9]]),as.numeric(winddirmns[9])),y=c(as.numeric(winddirmns[9]),22000))
p5=data.frame(x =c(as.numeric(headmeans[[9]]),as.numeric(headmeans[[9]])),y=c(as.numeric(headmeans[[9]]),22000))
wp5 <- ggplot(Departures_Autumn_int, aes(x=winddir_mean)) + 
  geom_histogram(breaks=seq(0,360,10),col='white',fill="cyan4") +
  geom_path(data=w5,aes(x =x,y=y),arrow = arrow(),size=2,colour="navyblue")+
  geom_path(data=p5,aes(x =x,y=y),arrow = arrow(),size=2,colour="gold")+
  coord_polar(start = 0) +
  scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
  theme_minimal()+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
        axis.text.x = element_text(size = 16,face = "bold",colour="cyan4"),
        panel.grid.major=element_line(colour = "cyan4"))

w6=data.frame(x =c(as.numeric(winddirmns[[10]]),as.numeric(winddirmns[10])),y=c(as.numeric(winddirmns[10]),22000))
p6=data.frame(x =c(as.numeric(headmeans[[10]]),as.numeric(headmeans[[10]])),y=c(as.numeric(headmeans[[10]]),22000))
wp6 <- ggplot(Departures_Autumn_int, aes(x=winddir_mean)) + 
  geom_histogram(breaks=seq(0,360,10),col='white',fill="cyan4") +
  geom_path(data=w6,aes(x =x,y=y),arrow = arrow(),size=2,colour="navyblue")+
  geom_path(data=p6,aes(x =x,y=y),arrow = arrow(),size=2,colour="gold")+
  coord_polar(start = 0) +
  scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
  theme_minimal()+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
        axis.text.x = element_text(size = 16,face = "bold",colour="cyan4"),
        panel.grid.major=element_line(colour = "cyan4"))

w7=data.frame(x =c(as.numeric(winddirmns[[3]]),as.numeric(winddirmns[3])),y=c(as.numeric(winddirmns[3]),22000))
p7=data.frame(x =c(as.numeric(headmeans[[3]]),as.numeric(headmeans[[3]])),y=c(as.numeric(headmeans[[3]]),22000))
wp7 <- ggplot(Spring_int_rad, aes(x=new.winddir)) + 
  geom_histogram(breaks=seq(0,360,10),col='white',fill="cyan4") +
  geom_path(data=w7,aes(x =x,y=y),arrow = arrow(),size=2,colour="navyblue")+
  geom_path(data=p7,aes(x =x,y=y),arrow = arrow(),size=2,colour="gold")+
  coord_polar(start = 0) +
  scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
  theme_minimal()+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
        axis.text.x = element_text(size = 16,face = "bold",colour="cyan4"),
        panel.grid.major=element_line(colour = "cyan4"))

w8=data.frame(x =c(as.numeric(winddirmns[[4]]),as.numeric(winddirmns[4])),y=c(as.numeric(winddirmns[4]),45000))
p8=data.frame(x =c(as.numeric(headmeans[[4]]),as.numeric(headmeans[[4]])),y=c(as.numeric(headmeans[[4]]),45000))
wp8 <- ggplot(Autumn_int_rad, aes(x=new.winddir)) + 
  geom_histogram(breaks=seq(0,360,10),col='white',fill="cyan4") +
  geom_path(data=w8,aes(x =x,y=y),arrow = arrow(),size=2,colour="navyblue")+
  geom_path(data=p8,aes(x =x,y=y),arrow = arrow(),size=2,colour="gold")+
  coord_polar(start = 0) +
  scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
  theme_minimal()+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
        axis.text.x = element_text(size = 16,face = "bold",colour="cyan4"),
        panel.grid.major=element_line(colour = "cyan4"))

error2 <- list(wind_ass[[5]],wind_ass[[9]],wind_ass[[3]],wind_ass[[6]],wind_ass[[10]],wind_ass[[4]])
error2 <- lapply(error2,as.data.frame)
for(k in 1:length (error2)){
  error2[[k]]$cat <- k
}

error3 <- do.call("rbind",error2)
names(error3) <- c("wind_ass","cat")
error3$col <- ifelse(error3$cat<4,1,3)

windows(6,3)
er1 <- ggplot(error3, aes(x = error3$cat, y = error3$wind_ass)) +
  geom_boxplot(aes(group=error3$cat,y=error3$wind_ass,fill=as.factor(error3$col)),outlier.shape = 1, 
               outlier.color="black",color="black")+
  scale_fill_manual(values = c("white","#1c9099"),name="",labels=c("Spring","Autumn"))+
  geom_hline(yintercept=0,col="red")+
  ylab('Wind assistance (m/s)')+
  scale_x_continuous(breaks=c(1:6),labels=c("dep","rt","rad","dep","rt","rad"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12,face = "bold",colour="black"),
        axis.text.y = element_text(size = 12,face = "bold",colour="black"),
        panel.grid.minor  =element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12,face = "bold",colour="black"),
        panel.grid = element_blank(),legend.position = "bottom",
        legend.text = element_text(size=12,face = "bold",color = "black"))

windows(6.5,8)
fwinds1 <- ggarrange(ggarrange(ggarrange(wp3,wp5,wp7,wp4,wp6,wp8,labels = c("dep","rt","rad","","",""),font.label = list(size = 13),ncol=3,nrow = 2,
                              widths=c(1,1,1,1),label.x=c(0.75,.75,.75,.75),label.y = c(.94,.94,.94,0.94)),
                    er1,ncol=1,nrow=2,heights = c(2.3,1),widths=c(0.8,0.8),labels = c("(a)"),vjust = 1),
                    ncol = 1,nrow = 1,labels=c("(b)"),vjust = 36.5)+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), legend.text = element_text(size = 4))
ggsave(filename=paste0("C:/Users/mbradar/surfdrive/Documents/Maja_PhD/first_paper_ver1/Paper figures/winds_ass_diffloc.png"),fwinds1,dpi=500)
