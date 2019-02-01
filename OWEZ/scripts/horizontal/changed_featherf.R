feather.plot2 <- function (r, theta,xpos,colour, yref = 0, use.arrows = TRUE, col.refline = "lightgray",
                           fp.type = "s", main = "", xlab = "", ylab = "", xlabels = NULL,
                           ...)
{
  if (missing(xpos))
   xpos <- 1:length(theta)
  if (fp.type == "m") 
    theta <- 5 * pi/2 - theta
  x <- r * cos(theta)
  y <- r * sin(theta)
  xmult <- diff(range(xpos))/(diff(range(y)) * 2)
  x <- x * xmult
  xlim <- range(c(xpos, x + xpos))
  ylim <- range(c(y,yref))
  oldpin <- par("pin")
  xdiff <- xlim[2] - xlim[1]
  ydiff <- ylim[2] - ylim[1]
  plot(0,xlim = xlim,ylim=ylim, type = "n", main = main,
       xlab = xlab, ylab = ylab,axes = FALSE, xaxt = "n")
  box()
  if (is.null(xlabels))
    axis(1)
  else axis(1, at = xpos, col=NULL, labels = xlabels,font=0.1)
 if (is.null(col))
  col= "black"
  abline(h = yref, col = col.refline)
  if (use.arrows)
    arrows(xpos, yref, col=colour, xpos + x, y, length = 0.1, ...)
  else segments(xpos, yref, xpos + x, y, ...)
  par(pin = oldpin)

}

