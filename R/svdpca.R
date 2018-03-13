svdpca <- function(X, k, method) {
  # Performs specified method of PCA
  #
  # Args:
  #   X: Data matrix.
  #   k: Number of PCA dimensions returned.
  #   method: The desired method for implementing pca for preprocessing the
  #           data. Options include 'svd', 'random', and 'none' (no pca).
  #
  # Returns:
  #   A matrix containing the rotated data is returned.

  if (method == 'svd') {
    print('PCA using svd')
    u <- svd(t(X), k)$u
    return(X %*% u)
  } else if (method == 'random') {
    print('PCA using random SVD')
    return(rsvd::rpca(X, k, retx = TRUE)$x)
  } else {
    print('No PCA performed')
    return(X)
  }
}
