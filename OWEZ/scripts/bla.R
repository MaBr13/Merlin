par(mar = c(5,5,2,5))
with(Firstyear, plot(date, Altitudem_mean, col="aquamarine", 
                 ylab="Altitude", xlab = "Month",  xlim=c(as.Date("2008-08-01"), as.Date("2009-01-01")),ylim=c(0,1500), main="Bird density and altitude:Autumn 2008")) 
par(new = T)
with(Firstyear, plot(date, Track_ID, type="h", pch=5,cex=0.5, axes=F, xlab=NA, ylab=NA, col="aquamarine4", ylim=c(0,1000000), xlim=c(as.Date("2008-08-01"), as.Date("2009-01-01"))))
axis(side = 4)
mtext(side = 4, line = 3, 'Density')
legend("topleft",
       legend=c("Altitude", "Bird density-total number of tracks"),
       lty=c(1,1), pch=c(NA, NA), col=c("aquamarine", "aquamarine4"))

