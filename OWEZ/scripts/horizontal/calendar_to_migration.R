library(lubridate)

Sys.setenv(TZ="UTC")
Sys.setlocale(category = "LC_ALL", locale = "English_United Kingdom.1252")#set the time on your computer to match

for(k in 1:length(listFstyear)){
  listFstyear[[k]]$date <- with(listFstyear[[k]], ymd(paste(jaar,maand,dag, sep=' ')))
  listFstyear[[k]]$timestep <- with(listFstyear[[k]], ymd_h(paste(jaar,maand,dag, uur, sep= ' ')))
}

diff <- period(hours=24)
listFstyear[[1]]$migr_days <- as.period(diff,ymd_h("2007-04-03 17:00:00"))


tomigr <- function(x) {
  op <- as.vector(first(x))
  hl <- range(x, na.rm = TRUE)
  cl <- as.vector(last(x))
  xts(cbind(Open = op, High = hl[2], Low = hl[1], Close = cl), end(x))
}

library(xts)
# Convert to xts

y <- as.xts(listFstyear[[1]]$timestep)
# Convert index to UTC
indexTZ(y) <- Sys.timezone(location = TRUE)
# Set first observation to epoch (zero)
.index(y) <- .index(y) - .index(y)[1]
# Get endpoints for y by day
ep <- endpoints(y, "days")

period.apply(listFstyear[[1]], ep, tomigr)

