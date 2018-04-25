#' Performs specified method of PCA
#'
#' Implements different methods of PCA
#'
#' @param X Data matrix.
#' @param k Number of PCA dimensions returned.
#' @param method The desired method for implementing pca for preprocessing the
#'        data. Options include 'svd', 'random', and 'none' (no pca).
#'
#' @return A matrix containing the rotated data is returned.
#'
svdpca <- function(X, k, method, verbose=FALSE) {
  if (ncol(X) <= k) {
    method <- 'none'
  } else if (!(method %in% c("svd", "random", "none"))) {
    message(paste0('PCA method ', method, 
                   ' not recognized. Choose from "random", "svd" and "none". Using "random"...'))
    method <- "random"
  }
  if (method == 'svd') {
    if (verbose) message('PCA using svd')
    out_matrix <- prcomp(X)$x[,1:k]
  } else if (method == 'random') {
    if (verbose) message('PCA using random SVD')
    out_matrix <- irlba::prcomp_irlba(X, k)$x
  } else if (method == 'none') {
    if (verbose) message('No PCA performed')
    out_matrix <- X
  } else {
    stop("PCA method not recognized")
  }
  return(out_matrix)
}
