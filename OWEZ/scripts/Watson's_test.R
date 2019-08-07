#############WATSON'S LARGE-SAMPLE NONPARAMETRIC TEST################

#Test for common mean direction without any assumptions about common shape or dispersion of
#underlying distributions

#parameters for the function
#cdat <- c(cdat1,cdat2,cdat3) - vector of all the samples you want to compare
#ndath <- c(length(cdat1),length(cdat2),length(cdat3)) - vector of lengths of each of the samples that are being compared
#g <- number of samples being compared 

#it returns statistics and p-value of the test


YgVal <- function(cdat, ndat, g) {
  N <- length(cdat) ; ndatcsum <- cumsum(ndat) 
  delhat <- 0 ; tbar <- 0
  for (k in 1:g) {
    sample <- circular(0)
    if (k==1) {low <- 0} else
      if (k > 1) {low <- ndatcsum[k-1]}
    for (j in 1:ndat[k]) { sample[j] <- cdat[j+low] }
    tm1 <- trigonometric.moment(sample, p=1)
    tm2 <- trigonometric.moment(sample, p=2)
    Rbar1 <- tm1$rho; Rbar2 <- tm2$rho ; tbar[k] <- tm1$mu
    delhat[k] <- (1-Rbar2)/(2*Rbar1*Rbar1)
  }
  dhatmax <- max(delhat) ; dhatmin <- min(delhat)
  if (dhatmax/dhatmin <= 4) {
    CP <- 0 ; SP <- 0 ; dhat0 <- 0
    for (k in 1:g) {
      CP <- CP + ndat[k]*cos(tbar[k])
      SP <- SP + ndat[k]*sin(tbar[k])
      dhat0 <- dhat0 + ndat[k]*delhat[k] 
    }
    dhat0 <- dhat0/N
    RP <- sqrt(CP*CP+SP*SP)
    Yg <- 2*(N-RP)/dhat0
    return(Yg) } 
  else if (dhatmax/dhatmin > 4) {
    CM <- 0 ; SM <- 0 ; Yg <- 0
    for (k in 1:g) {
      CM <- CM + (ndat[k]*cos(tbar[k])/delhat[k])
      SM <- SM + (ndat[k]*sin(tbar[k])/delhat[k])
      Yg <- Yg + (ndat[k]/delhat[k]) 
    }
    RM <- sqrt(CM*CM+SM*SM)
    Yg <- 2*(Yg-RM)
    return(Yg) }
}
