##################################################################################################
##########Plot differences in wind directions between days with high and low migration############

#Spring
OnedayM <- subset(Spring[[4]],Date=="2010-03-21", select = Timestamp:DirectionS)#high
SecDayM <- subset(Spring[[4]],Date=="2010-03-19", select = Timestamp:DirectionS)#low
HLS <- rbind(OnedayM,SecDayM)

  
  p1 <- ggplot(OnedayM, aes(Date,Mean.wspeed))+
    geom_point()+
    ylim(0,80)+
    ylab("Mean wind speed per hour (kph)")+
   xlab("High migration")
  p2 <- ggplot(SecDayM, aes(Date,Mean.wspeed))+
    geom_point()+
    ylim(0,80)+
    ylab("Mean wind speed per hour (kph)")+
    xlab("Low migration")

grid.arrange(ggplotGrob(p1), ggplotGrob(p2), layout_matrix = rbind(c(1,2)), top="Wind speed during low and high migration Spring 2010")

#Autumn
OnedayA <- subset(Autumn[[2]],Date=="2008-10-29", select = Timestamp:DirectionS)#high
SecDayA <- subset(Autumn[[2]],Date=="2008-10-27", select = Timestamp:DirectionS)#low
HLA <- rbind(OnedayA,SecDayA)


p1 <- ggplot(OnedayA, aes(Date,Mean.wspeed))+
  geom_point()+
  ylim(0,80)+
  ylab("Mean wind speed per hour (kph)")+
  xlab("High migration")
p2 <- ggplot(SecDayA, aes(Date,Mean.wspeed))+
  geom_point()+
  ylim(0,80)+
  ylab("Mean wind speed per hour (kph)")+
  xlab("Low migration")

grid.arrange(ggplotGrob(p1), ggplotGrob(p2), layout_matrix = rbind(c(1,2)), 
             top="Wind speed during low and high migration Autumn 2008")
