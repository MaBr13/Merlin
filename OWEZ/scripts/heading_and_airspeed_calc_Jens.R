for (k in 1:length(Allyears)){
strack<- sin(trackdirection)#calculate sinus and cosinus of track and wind direction
ctrack <- cos(trackdirection)#I converted track direction and wind direction in radians before doing this
swind <- sin(winddirR)
cwind <- cos(winddirR)

xa <- (Allyears_[[k]]$groundspeedms*strack)-(Allyears_[[k]]$windspeedms*swind)
ya <- (Allyears_[[k]]$groundspeedms*ctrack)-(Allyears_[[k]]$windspeedms*cwind)

heading<- atan2(xa,ya)
Allyears[[k]]$airspeedms<-sqrt((xa^2)+(ya^2)) 
Allyears[[k]]$r.heading <- heading*(180/pi)#formula for conversion back to angles
Allyears[[k]]$new.winddir <- winddirR*(180/pi)
Allyears[[k]]$new.winddir <- ifelse(Allyears_[[k]]$new.winddir<0, #putting it back into 360 modulo
                                    360+Allyears_[[k]]$new.winddir, 
                                    Allyears_[[k]]$new.winddir)

}