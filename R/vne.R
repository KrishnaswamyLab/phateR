vne <- function(diff.op, t.max=100) {
  svd <- svd(diff.op)
  eigs <- svd$d
  eigs.t <- eigs
  entropy <- c()
  for (i in 1:t.max) {
    prob <- eigs.t / sum(eig.t)
    prob <- prob + .Machine$double.eps
    entropy <- c(entropy, -sum(prob * log(prob)))
    eigs.t <- eigs.t * eigs
  }
  return(entropy)
}

#' Returns the x-location of a (single) knee of curve y=f(x)
#'   
#' @param y array, shape=[n]. Data for which to find the knee point
#' @param x array, optional, shape=[n], default=np.arange(len(y)). Indices of the data points of y, if these are not in order and evenly spaced
#' 
#' @return knee_point int The index (or x value) of the knee point on y
find.knee.point <- function(y, x=NA) {
  
  if (length(y) < 3) {
    message("Cannot find knee point on vector of length 3")
    return(NA)
  } else if (!is.vector(y)) {
    message("y must be a vector")
    return(NA)
  }

  if (is.na(x)) {
    x <- 1:length(y)
  } else if (!is.vector(x)) {
    message("x must be a vector or NA")
    return(NA)
  } else if (length(x) != length(y)) {
    message("x and y must be the same length")
    return(NA)
  } else {
    # ensure x is sorted
    idx <- order(x)
    x <- x[idx]
    y <- y[idx]
  }
  
  n <- 2:length(y)
  # figure out the m and b (in the y=mx+b sense) for the "left-of-knee"
  sigma_xy <- cumsum(x * y)[2:length(y)]
  sigma_x <- cumsum(x)[2:length(y)]
  sigma_y <- cumsum(y)[2:length(y)]
  sigma_xx <- cumsum(x * x)[2:length(y)]
  det <- (n * sigma_xx - sigma_x * sigma_x)
  mfwd <- (n * sigma_xy - sigma_x * sigma_y) / det
  bfwd <- -(sigma_x * sigma_xy - sigma_xx * sigma_y) / det
  
  # figure out the m and b (in the y=mx+b sense) for the "right-of-knee"
  sigma_xy <- cumsum(rev(x) * rev(y))[2:length(y)]
  sigma_x <- cumsum(rev(x))[2:length(y)]
  sigma_y <- cumsum(rev(y))[2:length(y)]
  sigma_xx <- cumsum(rev(x) * rev(x))[2:length(y)]
  det <- (n * sigma_xx - sigma_x * sigma_x)
  mbck <- rev((n * sigma_xy - sigma_x * sigma_y) / det)
  bbck <- rev(-(sigma_x * sigma_xy - sigma_xx * sigma_y) / det)
  
  # figure out the sum of per-point errors for left- and right- of-knee fits
  error.curve <- sapply(2:(length(y)-1), function(breakpt) { 
    delsfwd <- (mfwd[breakpt - 1] * x[1:breakpt] + bfwd[breakpt - 1]) - y[1:breakpt]
    delsbck <- (mbck[breakpt - 1] * x[breakpt:length(x)] + bbck[breakpt - 1]) - y[breakpt:length(y)]
    sum(abs(delsfwd)) + sum(abs(delsbck))
  })
  
  # find location of the min of the error curve
  loc <- which.min(error.curve) + 1
  knee.point <- x[loc]
  return(knee.point)
}

optimal.t <- function(diff.op, t.max=100, plot=FALSE) {
  h <- vne(diff.op, t=t.max)
  t.opt <- find.knee.point(h)
  
  if (plot) {
    plot(1:t.max, h, 
         type='l', 
         xlab='t', ylab='Von Neumann Entropy', 
         main=paste0('Optimal t = ', t.opt))
    points(t.opt, h[t.opt], pch="*", cex=3)
  }
  return(t.opt)
}