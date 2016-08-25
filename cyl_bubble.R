# similar to bell_bubble, but with cylinder instead of bell

cyl_bubble <- function(X_amp=50, Y_amp=50, Z_amp=50){
      set.seed(137)
      t <- runif(200)
      s <- runif(200, -0.95, 0.95)
      x <- sin(2*pi*t)/2.1
      y <- cos(2*pi*t)/2.1
      w <- cbind(X_amp*2*x,Y_amp*2*y,Z_amp*s)     
      d <- as.data.frame((w))
      names(d) <- c("X_value", "Y_value", "Z_value")
      
      #plot(d$X_value, d$Y_value)               # shows circle
      #plot(d$Z_value, d$Y_value)               # side of the sampled cylinder
      #plot(d$Z_value, (d$Y_value)^2 + (d$X_value)^2) #line at (100/2.1)^2
      
      # optional code to save to file
      #png(filename = "cyl_bubble.png")
      
      
      # use "TDA" package to plot barcode. may need to add line: library("TDA)
      maxscale <- 60 # when i tried 80 it froze up!
      maxdimension <- 2
      Diag <- ripsDiag(X = d, maxdimension, maxscale, library = "GUDHI", printProgress = TRUE)      
      plot(Diag[["diagram"]], barcode = TRUE)
      mtext("Barcode of cylinder / circle non-bubble")
      legend("right", lty=c(1,1,1), lwd=c(3,3,3), col=c("blue", "red", "black"), legend=c( "H2", "H1", "H0"))
      #dev.off()
      
      d
      
}