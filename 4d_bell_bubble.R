# like bell_bubble, but with two bells going
# the result is a 3D bubble living in 4D

fourd_bell_bubble <- function(X_amp=50, Y_amp=50, Z1_amp=50, Z2_amp=50){
      library("TDA")
      set.seed(137)

      t <- runif(200)
      s <- runif(200, -0.95, 0.95)
      r <- runif(200, -0.95, 0.95)
      x <- exp(-(r^2)/.4)*exp(-(s^2)/.4)*sin(2*pi*t)/2.1
      y <- exp(-(r^2)/.4)*exp(-(s^2)/.4)*cos(2*pi*t)/2.1
      w <- cbind(X_amp*2*x,Y_amp*2*y,Z1_amp*s, Z2_amp*r)     
      d <- as.data.frame((w))
      names(d) <- c("X_value", "Y_value", "Z1_value", "Z2_value")
      
      # optional code to reduce to 100 points... speeds things up
      #d <- d[1:100,]
      
      # use "TDA" package to plot barcode. 
      maxscale <- 60 # when i tried 80 it froze up!
      maxdimension <- 3
      Diag <- ripsDiag(X = d, maxdimension, maxscale, library = "GUDHI", printProgress = TRUE)      
      plot(Diag[["diagram"]], barcode = TRUE)
      mtext("Barcode of bell curve cross bell curve cross circle")
      legend("right", lty=c(1,1,1,1), lwd=c(3,3,3,3), col=c("green", "blue", "red", "black"), legend=c("H3", "H2", "H1", "H0"))

      d
}
