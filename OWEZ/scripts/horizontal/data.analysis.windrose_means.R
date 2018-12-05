###Spring

for(k in 1:length(Spring)){
  
  Spring[[k]]$gspms <- (Spring[[k]]$Mean.speed)*0.277778
}

for(k in 1:length(Spring)){
  
  Spring[[k]]$aspms <- (Spring[[k]]$Mean.aspeed)*0.277778
}

for(k in 1:length(Spring)){
  
  Spring[[k]]$wspms <- (Spring[[k]]$Mean.wspeed)*0.277778
}

#wind speeds and directions
p1 <- plot.windrose(spd=Spring[[1]]$wspms, dir = Spring[[1]]$Mean.wdir,spdseq = c(0,10,20,30,40,50))
p2 <- plot.windrose(spd=Spring[[2]]$wspms, dir = Spring[[2]]$Mean.wdir,spdseq = c(0,10,20,30,40,50))
p3 <- plot.windrose(spd=Spring[[3]]$wspms, dir = Spring[[3]]$Mean.wdir,spdseq = c(0,10,20,30,40,50))
p4 <- plot.windrose(spd=Spring[[4]]$wspms, dir = Spring[[4]]$Mean.wdir,spdseq = c(0,10,20,30,40,50))
grid.arrange(p1,p2,p3,p4,nrow=2,top="Wind speed and track direction Spring 2007-2010")
s1 <- plot.windrose(spd=Spring[[1]]$aspms, dir = Spring[[1]]$Mean.head,spdseq = c(0,15,20,30,40,50))
s2 <- plot.windrose(spd=Spring[[2]]$aspms, dir = Spring[[2]]$Mean.head,spdseq = c(0,15,20,30,40,50))
s3 <- plot.windrose(spd=Spring[[3]]$aspms, dir = Spring[[3]]$Mean.head,spdseq = c(0,15,20,30,40,50))
s4 <- plot.windrose(spd=Spring[[4]]$aspms, dir = Spring[[4]]$Mean.head,spdseq = c(0,15,20,30,40,50))
grid.arrange(s1,s2,s3,s4,nrow=2,top="Air speed and track direction Spring 2007-2010")

#Autumn
for(k in 1:length(Autumn)){
  
  Autumn[[k]]$gspms <- (Autumn[[k]]$Mean.speed)*0.277778
}

for(k in 1:length(Autumn)){
  
  Autumn[[k]]$aspms <- (Autumn[[k]]$Mean.aspeed)*0.277778
}

for(k in 1:length(Autumn)){
  
  Autumn[[k]]$wspms <- (Autumn[[k]]$Mean.wspeed)*0.277778
}

#wind speeds and directions
p1 <- plot.windrose(spd=Autumn[[1]]$wspms, dir = Autumn[[1]]$Mean.wdir,spdseq = c(0,10,20,30,40,50))
p2 <- plot.windrose(spd=Autumn[[2]]$wspms, dir = Autumn[[2]]$Mean.wdir,spdseq = c(0,10,20,30,40,50))

s1 <- plot.windrose(spd=Autumn[[1]]$aspms, dir = Autumn[[1]]$Mean.head,spdseq = c(0,15,20,30,40,50))
s2 <- plot.windrose(spd=Autumn[[2]]$aspms, dir = Autumn[[2]]$Mean.head,spdseq = c(0,15,20,30,40,50))

grid.arrange(s1,s2,nrow=1,top="Air speed and track direction Autumn 2007-2008")

grid.arrange(p1,p2,nrow=1,top="Wind speed and direction Autumn 2007-2008")
