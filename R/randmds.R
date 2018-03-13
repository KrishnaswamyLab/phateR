randmds <- function(X, ndim) {
  X <- X * X
  X <- X - colMeans(X)
  X <- X - rowMeans(X)
  return(rsvd::rpca(X, ndim, retx = TRUE)$x)
}
