cos.dissim <- function(i, j, x=NA) {
  if (is.na(x)) stop('Must pass a matrix');
  a <- x[, i]; b <- x[, j];
  if (length(a) != length(b)) stop('Length of both vectors must be the same');
  return(1 - (a %*% b) / (sqrt(sum(a^2)) * sqrt(sum(b^2))))
}
