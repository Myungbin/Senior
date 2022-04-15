# binorm.r

binormalpdf <- function(r=0){
    x <- seq(-3, 3, length = 30) # -3 3 까지 길이가 30인 등차수열
    y <- x # y = x
    z <- matrix(0, nrow = length(x),
                ncol = length(y))

    
    for (i in 1:length(x)) {
        for(j in 1:length(y)){
            z[i,j] <- exp(-(x[i]^2-2*r*x[i]*y[j] +
                                y[j]^2/(2*(1-r^2))))
        }
        
    }
    z <- z/(2*pi*sqrt(1-r^2))
    
    list(x=x, y=y, z=z)
}

drawbinnorm <- function(rr=0.4){
    
    
    win.graph()
    persp(binormalpdf(rr)$z,
          border = 'cyan', theta = -150,
          xlab = 'x', ylab = 'y')
    
    win.graph()
    contour(binormalpdf(rr)$x,
            binormalpdf(rr)$y,
            binormalpdf(rr)$z)
}


drawbinnorm()
drawbinnorm(0.9)