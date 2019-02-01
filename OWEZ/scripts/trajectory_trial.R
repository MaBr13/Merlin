dir <- 270
sp <- 10
ts <- 1
latlong <- c(52.60636,4.389639)
sunrise <- 18:00:00
sunset <- 05:00:00


ProcTraj(lat=52.60636,lon = 4.389639, hour.interval = 1, name = 'london', start.hour = "18:00",
         end.hour = "05:00",hours= -11, tz="UTC")

## lat, lng in degrees. Bearing in degrees. Distance in m
calcPosBear = function(lat,lon,bea,time,vel) {
  earthR = 6378137; 
  
  ## Units meter, seconds and meter/seconds
  dist = time * vel
  
  lat2 = asin(sin(
    pi / 180 * lat) * 
      cos(dist / earthR) + 
      cos(pi / 180 * lat) * 
      sin(dist / earthR) * 
      cos(pi / 180 * bea));
  
  lon2 = pi / 180 * lon + 
    atan2(sin( pi / 180 * bea) * 
            sin(dist / earthR) * 
            cos( pi / 180 * lat ), 
          cos(dist / earthR) - 
            sin( pi / 180 * lat) * 
            sin(lat2));
  
  latR = (180 * lat2) / pi
  lonR = (180 * lon2) / pi
  
  print(latR)
  print(lonR)
}        

calcPosBear(lat=52.60636,lon=4.389639, bea=270,time=3600,vel=10)


for (k in 1:length(dataf)){
  lat=52.60636
  lon=4.389639
  vel=dataf[k]$aspeed
  bea=360-dataf[k]$dir
  sunr=dataf[k]$sunr
  suns=dataf[k]$suns 
  time=
  
  earthR = 6378137; 
  
  ## Units meter, seconds and meter/seconds
  dist = time * vel
  
  lat2 = asin(sin(
    pi / 180 * lat) * 
      cos(dist / earthR) + 
      cos(pi / 180 * lat) * 
      sin(dist / earthR) * 
      cos(pi / 180 * bea));
  
  lon2 = pi / 180 * lon + 
    atan2(sin( pi / 180 * bea) * 
            sin(dist / earthR) * 
            cos( pi / 180 * lat ), 
          cos(dist / earthR) - 
            sin( pi / 180 * lat) * 
            sin(lat2));
  
  latR = (180 * lat2) / pi
  lonR = (180 * lon2) / pi
  
  print(latR)
  print(lonR)
}