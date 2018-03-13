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
  } else {
    message('No PCA performed')
    out_matrix <- X
  }
  return(out_matrix)
}
