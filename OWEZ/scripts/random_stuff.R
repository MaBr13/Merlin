
Departures_Spring<- Departures_Spring %>%
  group_by(date) %>%
  summarise(u=mean(u),v=mean(v),mean_windspeed=mean(windspeedms_mean),
            mean_winddir=mean(circular(winddir_mean,units = "degrees", modulo="2pi",template = 'geographic')))

Departures_Autumn<- Departures_Autumn %>%
  group_by(date) %>%
  summarise(u=mean(u),v=mean(v),mean_windspeed=mean(windspeedms_mean),
            mean_winddir=mean(circular(winddir_mean,units = "degrees", modulo="2pi",template = 'geographic')))

Departures_Spring_int <- Departures_Spring_int %>%
  group_by(date) %>%
  summarise(wu_start=mean(wu_start),wv_start=mean(wv_start), wu_mean=mean(wu_mean),wv_mean=mean(wv_mean),
                td_start=mean(circular(td_start,units = "degrees", modulo="2pi",template = 'geographic')),
                td_mean=mean(circular(td_mean,units = "degrees", modulo="2pi",template = 'geographic')),
                gsms_start=mean(gsms_start),gsms_mean=mean(gsms_mean),
                heading=mean(circular(heading,units = "degrees", modulo="2pi",template = 'geographic')),
                airspeed=mean(airspeed),windspeedms_start=mean(windspeedms_start),windspeedms_mean=mean(windspeedms_mean),
                winddir_start=mean(circular(winddir_start,units = "degrees", modulo="2pi",template = 'geographic')),
                winddir_mean=mean(circular(winddir_mean,units = "degrees", modulo="2pi",template = 'geographic')),
                pmd_start=mean(circular(pmd_start, units = "degrees", modulo="2pi",template = 'geographic')),
                pmd_mean=mean(circular(pmd_mean,units = "degrees", modulo="2pi",template = 'geographic')),
                wind_ass_start=mean(wind_ass_start),wind_ass_mean=mean(wind_ass_mean),drift_start=mean(drift_start),
                drift_mean=mean(drift_mean))


Departures_Autumn_int <- Departures_Autumn_int %>%
  group_by(date) %>%
  summarise(wu_start=mean(wu_start),wv_start=mean(wv_start), wu_mean=mean(wu_mean),wv_mean=mean(wv_mean),
                td_start=mean(circular(td_start,units = "degrees", modulo="2pi",template = 'geographic')),
                td_mean=mean(circular(td_mean,units = "degrees", modulo="2pi",template = 'geographic')),
                gsms_start=mean(gsms_start),gsms_mean=mean(gsms_mean),
                heading=mean(circular(heading,units = "degrees", modulo="2pi",template = 'geographic')),
                airspeed=mean(airspeed),windspeedms_start=mean(windspeedms_start),windspeedms_mean=mean(windspeedms_mean),
                winddir_start=mean(circular(winddir_start,units = "degrees", modulo="2pi",template = 'geographic')),
                winddir_mean=mean(circular(winddir_mean,units = "degrees", modulo="2pi",template = 'geographic')),
                pmd_start=mean(circular(pmd_start, units = "degrees", modulo="2pi",template = 'geographic')),
                pmd_mean=mean(circular(pmd_mean,units = "degrees", modulo="2pi",template = 'geographic')),
                wind_ass_start=mean(wind_ass_start),wind_ass_mean=mean(wind_ass_mean),drift_start=mean(drift_start),
                drift_mean=mean(drift_mean))

names(Departures_Spring)[names(Departures_Spring)==c("date")] <- c("n.date")
Departures_Spring$n.date <- as.factor(Departures_Spring$n.date)
Departures_Spring <- left_join(Departures_Spring,Spring_rad,by="n.date")
Departures_Spring <- na.omit(Departures_Spring)
names(Departures_Autumn)[names(Departures_Autumn)==c("date")] <- c("n.date")
Departures_Autumn$n.date <- as.factor(Departures_Autumn$n.date)
Departures_Autumn <- left_join(Departures_Autumn,Autumn_rad,by="n.date")
Departures_Autumn <- na.omit(Departures_Autumn)


Departures_Spring$heading <- Departures_Spring$b.heading
Departures_Spring$airspeed <- Departures_Spring$airspeedms
Departures_Autumn$heading <- Departures_Autumn$b.heading
Departures_Autumn$airspeed <- Departures_Autumn$airspeedms

Departures_Spring$drift <- atan((Departures_Spring$airspeed*sin(Departures_Spring$heading)+
                                       Departures_Spring$mean_windspeed*sin(Departures_Spring$mean_winddir))/
                                      (Departures_Spring$airspeed*cos(Departures_Spring$heading)+
                                         Departures_Spring$mean_windspeed*cos(Departures_Spring$mean_winddir)))
Departures_Spring$pmd <- Departures_Spring$heading+Departures_Spring$drift
Departures_Spring$wind_ass <- NCEP.Tailwind(Departures_Spring$u,Departures_Spring$v,
                                            as.numeric(Departures_Spring$pmd),Departures_Spring$airspeed)[,1]


Departures_Autumn$drift <- atan((Departures_Autumn$airspeed*sin(Departures_Autumn$heading)+
                                       Departures_Autumn$mean_windspeed*sin(Departures_Autumn$mean_winddir))/
                                      (Departures_Autumn$airspeed*cos(Departures_Autumn$heading)+
                                         Departures_Autumn$mean_windspeed*cos(Departures_Autumn$mean_winddir)))
Departures_Autumn$pmd <- Departures_Autumn$heading+Departures_Autumn$drift
Departures_Autumn$wind_ass <- NCEP.Tailwind(Departures_Autumn$u,Departures_Autumn$v,
                                            as.numeric(Departures_Autumn$pmd),Departures_Autumn$airspeed)[,1]
Departures_Spring$pmd <- ifelse(Departures_Spring$pmd>360,0+(Departures_Spring$pmd-360),Departures_Spring$pmd)
Departures_Spring$pmd <- ifelse(Departures_Spring$pmd<0,360-Departures_Spring$pmd,Departures_Spring$pmd)
Departures_Autumn$pmd <- ifelse(Departures_Autumn$pmd>360,0+(Departures_Autumn$pmd-360),Departures_Autumn$pmd)
Departures_Autumn$pmd <- ifelse(Departures_Autumn$pmd<0,360-Departures_Autumn$pmd,Departures_Autumn$pmd)


#change names of columns
names(Departures_Spring_int)[names(Departures_Spring_int) == c("Long_start","Lat_start")] <- c("long","lat")
names(Departures_Autumn_int)[names(Departures_Autumn_int) == c("Long_start","Lat_start")] <- c("long","lat")
#summarize data per location for the rest of the season 
#Departures_Spring<- Departures_Spring %>%
#  group_by(lat,long) %>%
#  summarise(mean_windspeed=mean(windspeedms_mean),
#            mean_winddir=mean(circular(winddir_mean,units = "degrees", modulo="2pi",template = 'geographic')))
#Departures_Autumn<- Departures_Autumn %>%
#  group_by(lat,long) %>%
#  summarise(mean_windspeed=mean(windspeedms_mean),
#            mean_winddir=mean(circular(winddir_mean,units = "degrees", modulo="2pi",template = 'geographic')))

#extract columns that contain coordinates and put them in the same order
coords1S <- Departures_Spring_int[,c(1,2)]
coords2S <- Departures_Spring[,2:3]
coords2S <- coords2S[,c(2,1)]
coords1A <- Departures_Autumn_int[,c(1,2)]
coords2A <- Departures_Autumn[,2:3]
coords2A <- coords2A[,c(2,1)]
# find nearest neighbours by calculating euclidian distance
knn.outS <- ann(as.matrix(coords2S), as.matrix(coords1S), k=1)
knn.outA <- ann(as.matrix(coords2A), as.matrix(coords1A), k=1)
#define rows of one data frame that correspond to rows of another data frame
distS <- knn.outS$knnIndexDist
distA <- knn.outA$knnIndexDist
#initialize data frames
Departures_Spring_new <- data.frame(matrix(ncol = 9, nrow = nrow(Departures_Spring_int)))
Departures_Autumn_new <- data.frame(matrix(ncol = 9, nrow = nrow(Departures_Autumn_int)))
#join and create a new data frame for the rest of the season which correspond to a seasonal data per location of
#each trajectory
for (k in 1:nrow(Departures_Spring_int)){
  Departures_Spring_new[k,] <- Departures_Spring[distS[k,1],]
}
for (k in 1:nrow(Departures_Autumn_int)){
  Departures_Autumn_new[k,] <- Departures_Autumn[distA[k,1],]
}

names(Departures_Spring_new)<- c("nr","lat","long","u","v","mean_windspeed","mean_winddir")
names(Departures_Autumn_new)<- c("nr","lat","long","u","v","mean_windspeed","mean_winddir")
Departures_Spring_new$heading <- Departures_Spring_int$heading
Departures_Spring_new$airspeed <- Departures_Spring_int$airspeed
Departures_Autumn_new$heading <- Departures_Autumn_int$heading
Departures_Autumn_new$airspeed <- Departures_Autumn_int$airspeed

Departures_Spring_new$drift <- atan((Departures_Spring_new$airspeed*sin(Departures_Spring_new$heading)+
                                       Departures_Spring_new$mean_windspeed*sin(Departures_Spring_new$mean_winddir))/
                                      (Departures_Spring_new$airspeed*cos(Departures_Spring_new$heading)+
                                         Departures_Spring_new$mean_windspeed*cos(Departures_Spring_new$mean_winddir)))
Departures_Spring_new$pmd <- Departures_Spring_new$heading+Departures_Spring_new$drift
Departures_Spring_new$u <- ds2uv(Departures_Spring_new$mean_winddir,Departures_Spring_new$mean_windspeed)[,1]
Departures_Spring_new$v <- ds2uv(Departures_Spring_new$mean_winddir,Departures_Spring_new$mean_windspeed)[,2]
Departures_Spring_new$wind_ass <- NCEP.Tailwind(Departures_Spring_new$u,Departures_Spring_new$v,
                                                Departures_Spring_new$pmd,Departures_Spring_new$airspeed)[,1]


Departures_Autumn_new$drift <- atan((Departures_Autumn_new$airspeed*sin(Departures_Autumn_new$heading)+
                                       Departures_Autumn_new$mean_windspeed*sin(Departures_Autumn_new$mean_winddir))/
                                      (Departures_Autumn_new$airspeed*cos(Departures_Autumn_new$heading)+
                                         Departures_Autumn_new$mean_windspeed*cos(Departures_Autumn_new$mean_winddir)))
Departures_Autumn_new$pmd <- Departures_Autumn_new$heading+Departures_Autumn_new$drift
Departures_Autumn_new$u <- ds2uv(Departures_Autumn_new$mean_winddir,Departures_Autumn_new$mean_windspeed)[,1]
Departures_Autumn_new$v <- ds2uv(Departures_Autumn_new$mean_winddir,Departures_Autumn_new$mean_windspeed)[,2]
Departures_Autumn_new$wind_ass <- NCEP.Tailwind(Departures_Autumn_new$u,Departures_Autumn_new$v,
                                                Departures_Autumn_new$pmd,Departures_Autumn_new$airspeed)[,1]


Departures_Spring_new$pmd <- ifelse(Departures_Spring_new$pmd>360,0+(Departures_Spring_new$pmd-360),Departures_Spring_new$pmd)
Departures_Spring_new$pmd <- ifelse(Departures_Spring_new$pmd<0,360-Departures_Spring_new$pmd,Departures_Spring_new$pmd)
Departures_Autumn_new$pmd <- ifelse(Departures_Autumn_new$pmd>360,0+(Departures_Autumn_new$pmd-360),Departures_Autumn_new$pmd)
Departures_Autumn_new$pmd <- ifelse(Departures_Autumn_new$pmd<0,360-Departures_Autumn_new$pmd,Departures_Autumn_new$pmd)

#make categories based on 3-hour timestamp
Spring_rad$hour <- hour(Spring_rad$timestep)
Autumn_rad$hour <- hour(Autumn_rad$timestep)
Spring_int_rad$hour <- hour(Spring_int_rad$timestep)
Autumn_int_rad$hour <- hour(Autumn_int_rad$timestep)
Spring_rad$hour_cat<- cut(Spring_rad$hour,breaks=c(-1,2,5,8,11,14,17,20,24), 
                          labels = c("1","2","3","4","5","6","7","8"))
Autumn_rad$hour_cat<- cut(Autumn_rad$hour,breaks=c(-1,2,5,8,11,14,17,20,24), 
                          labels = c("1","2","3","4","5","6","7","8"))
Spring_int_rad$hour_cat <- cut(Spring_int_rad$hour,breaks=c(-1,2,5,8,11,14,17,20,24), 
                               labels = c("1","2","3","4","5","6","7","8"))
Autumn_int_rad$hour_cat <- cut(Autumn_int_rad$hour,breaks=c(-1,2,5,8,11,14,17,20,24), 
                               labels = c("1","2","3","4","5","6","7","8"))
#agreggate per 3-hour timestamp
Spring_rad <- Spring_rad %>%
  group_by(n.date) %>%
  summarise(trackheading=mean(circular(trackheading,units = "degrees", modulo="2pi",template = 'geographic')),
            groundspeedms=mean(groundspeedms),airspeedms=mean(airspeedms),windspeedms=mean(windspeedms),
            b.heading=mean(circular(b.heading,units = "degrees", modulo="2pi",template = 'geographic')),
            new.winddir=mean(circular(new.winddir,units = "degrees", modulo="2pi",template = 'geographic')),
            wind_ass=mean(wind_ass))
Autumn_rad <- Autumn_rad %>%
  group_by(n.date) %>%
  summarise(trackheading=mean(circular(trackheading,units = "degrees", modulo="2pi",template = 'geographic')),
            groundspeedms=mean(groundspeedms),airspeedms=mean(airspeedms),windspeedms=mean(windspeedms),
            b.heading=mean(circular(b.heading,units = "degrees", modulo="2pi",template = 'geographic')),
            new.winddir=mean(circular(new.winddir,units = "degrees", modulo="2pi",template = 'geographic')),
            wind_ass=mean(wind_ass))
Spring_int_rad <- Spring_int_rad %>%
  group_by(n.date) %>%
  summarise(trackheading=mean(circular(trackheading,units = "degrees", modulo="2pi",template = 'geographic')),
            groundspeedms=mean(groundspeedms),airspeedms=mean(airspeedms),windspeedms=mean(windspeedms),
            b.heading=mean(circular(b.heading,units = "degrees", modulo="2pi",template = 'geographic')),
            new.winddir=mean(circular(new.winddir,units = "degrees", modulo="2pi",template = 'geographic')),
            wind_ass=mean(wind_ass))
Autumn_int_rad <- Autumn_int_rad %>%
  group_by(n.date) %>%
  summarise(trackheading=mean(circular(trackheading,units = "degrees", modulo="2pi",template = 'geographic')),
            groundspeedms=mean(groundspeedms),airspeedms=mean(airspeedms),windspeedms=mean(windspeedms),
            b.heading=mean(circular(b.heading,units = "degrees", modulo="2pi",template = 'geographic')),
            new.winddir=mean(circular(new.winddir,units = "degrees", modulo="2pi",template = 'geographic')),
            wind_ass=mean(wind_ass))

Departures_Spring_int <- Departures_Spring_int %>%
  group_by(date) %>%
  summarise(td_start=mean(circular(td_start,units = "degrees", modulo="2pi", template = "geographic")),
            gsms_start=mean(gsms_start), airspeed=mean(airspeed), windspeedms_start=mean(windspeedms_start),
            heading=mean(circular(heading,units="degrees",modulo="2pi", template = "geographic")),
            winddir_start=mean(circular(winddir_start,units="degrees",modulo="2pi", template="geographic")),
            wind_ass_start=mean(wind_ass_start))

Departures_Autumn_int <- Departures_Autumn_int %>%
  group_by(date) %>%
  summarise(td_start=mean(circular(td_start,units = "degrees", modulo="2pi", template = "geographic")),
            gsms_start=mean(gsms_start), airspeed=mean(airspeed), windspeedms_start=mean(windspeedms_start),
            heading=mean(circular(heading,units="degrees",modulo="2pi", template = "geographic")),
            winddir_start=mean(circular(winddir_start,units="degrees",modulo="2pi", template="geographic")),
            wind_ass_start=mean(wind_ass_start))

Departures_Spring_int$wu_start <- ds2uv(Departures_Spring_int$winddir_start,Departures_Spring_int$windspeedms_start)[,1]
Departures_Spring_int$wv_start <- ds2uv(Departures_Spring_int$winddir_start,Departures_Spring_int$windspeedms_start)[,2]  

Departures_Autumn_int$wu_start <- ds2uv(Departures_Autumn_int$winddir_start,Departures_Autumn_int$windspeedms_start)[,1]
Departures_Autumn_int$wv_start <- ds2uv(Departures_Autumn_int$winddir_start,Departures_Autumn_int$windspeedms_start)[,2]  
