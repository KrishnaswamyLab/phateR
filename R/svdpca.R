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
svdpca <- function(X, k, method) {

  if (method == 'svd') {
    message('PCA using svd')
    u <- svd(t(X), k)$u
    out_matrix <- X %*% u
  } else if (method == 'random') {
    message('PCA using random SVD')
    out_matrix <- rsvd::rpca(X, k, retx = TRUE)$x
  } else if (method == 'none') {
    message('No PCA performed')
    out_matrix <- X
  } else {
    message(paste0('PCA method ', method, 
                   ' not recognised. Choose from "random", "svd" and "none". Using "random"...'))
    out_matrix <- rsvd::rpca(X, k, retx = TRUE)$x
  }
  return(out_matrix)
}
