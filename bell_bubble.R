# returns a 200x4 data frame, with columns id, X, Y, Z
# X and Y sample a circle, with amplitude depending on Z
# X,Y,Z range between -50 and 50 by default
# Plots the persistence barcode.
bell_bubble <- function(X_amp=50, Y_amp=50, Z_amp=50){
      library("TDA")
      set.seed(137)
      
      t <- runif(200)
      s <- runif(200, -0.95, 0.95)
      x <- exp(-(s^2)/.4)*sin(2*pi*t)/2.1
      y <- exp(-(s^2)/.4)*cos(2*pi*t)/2.1
      w <- cbind(X_amp*2*x,Y_amp*2*y,Z_amp*s)     
      d <- as.data.frame((w))
      names(d) <- c("X_value", "Y_value", "Z_value")

      # use "TDA" package to plot barcode
      maxscale <- 60 # when i tried 80 it froze up!
      maxdimension <- 2
      Diag <- ripsDiag(X = d, maxdimension, maxscale, library = "GUDHI", printProgress = TRUE)      
      plot(Diag[["diagram"]], barcode = TRUE)
      mtext("Barcode of bell curve cross circle")
      legend("right", lty=c(1,1,1), lwd=c(3,3,3), col=c("blue", "red", "black"), legend=c( "H2", "H1", "H0"))

      d
}

# feed d into this code for 2d plot of points colored by Z_value
color_bubble <- function(d){
      library(ggplot2)
      # qplot(Z_value, Y_value, data=d)  # shows the bell-curviness
      qplot(X_value, Y_value, data=d, color=Z_value)
      # you can see that the darkest and lightest points are in the center,
      # and the middle-blue points are on the outside
}