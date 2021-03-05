Departs <- lapply(listcsv, read.csv)
meansDep <- list()
for(k in 1:length(Departs)){
  Departs[[k]]$date <- file_date[k] #add the date column in all the list elements 
}

for (k in 1:length(Departs)){
  Departs[[k]]$windspeedms_start <-  sqrt((Departs[[k]]$wu_start * Departs[[k]]$wu_start) + (Departs[[k]]$wv_start * Departs[[k]]$wv_start))
  winddirR_start <- atan2(Departs[[k]]$wu_start, Departs[[k]]$wv_start)
  Departs[[k]]$winddir_start <- winddirR_start*(180/pi)
  Departs[[k]]$winddir_start <- ifelse(Departs[[k]]$winddir_start<0, 360+Departs[[k]]$winddir_start, Departs[[k]]$winddir_start)
  
}

Deps <- as.data.frame(file_date)


for (k in 1:length(Departs)){
 winddir[[k]] <- mean(circular(Departs[[k]]$winddir,units = "degrees", modulo="2pi",template = 'geographic'))
 wdirad[[k]] <- angular.deviation(circular(Departs[[k]]$winddir,units = "degrees", modulo="2pi",template = 'geographic'))
 wdirR[[k]] <- rho.circular(circular(Departs[[k]]$winddir,units = "degrees", modulo="2pi",template = 'geographic'))
 windsp[[k]] <- mean(Departs[[k]]$windspeedms_start)
 wspSD[[k]] <- sd(Departs[[k]]$windspeedms_start)
 heading[[k]] <- mean(circular(Departs[[k]]$heading,units = "degrees", modulo="2pi",template = 'geographic'))
 headad[[k]] <- angular.deviation(circular(Departs[[k]]$heading,units = "degrees", modulo="2pi",template = 'geographic'))
 headR[[k]] <- rho.circular(circular(Departs[[k]]$heading,units = "degrees", modulo="2pi",template = 'geographic'))
 airspeed[[k]] <- mean(Departs[[k]]$airspeed)
 airspSD[[k]] <- sd(Departs[[k]]$airspeed)
 trdir[[k]] <- mean(circular(Departs[[k]]$td_start,units = "degrees", modulo="2pi",template = 'geographic'))
 tdirAD[[k]] <- angular.deviation(circular(Departs[[k]]$td_start,units = "degrees", modulo="2pi",template = 'geographic'))
 tdirR[[k]] <- rho.circular(circular(Departs[[k]]$td_start,units = "degrees", modulo="2pi",template = 'geographic'))
 gspeed[[k]] <- mean(Departs[[k]]$gsms_start)
 gspSD[[k]] <- sd(Departs[[k]]$gsms_start)
}

Deps <- cbind(trdir,tdirAD,tdirR,gspeed,gspSD,winddir,wdirad,wdirR,windsp,wspSD,heading,headad,headR,airspeed,airspSD)
Deps <- as.data.frame(Deps)
Deps$date <- file_date
write.xlsx(Deps,"Individual_days_stats.xlsx")
