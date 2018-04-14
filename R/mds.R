#' Performs specified method of PCA
#'
#' Implements different methods of PCA
#'
#' @param X Data matrix.
#' @param k Number of PCA dimensions returned.
#' @param method The desired method for implementing pca for preprocessing the
#'        data. Options include 'svd', 'random', and 'none' (no pca).
#' @param dist.method The desired distance function for MDS. Choices are 'euclidean'
#'                     and 'cosine'. Default is 'euclidean'.
#'
#' @return A matrix containing the embedded data is returned.
#'
mds <- function(data, ndim=2, method='metric', 
                dist.method='euclidean') {
  if (!(method %in% c("classic", "metric", "nonmetric"))) {
    message(paste0("method ", method, " not recognized. Choose from c('classic', 'metric', 'nonmetric'). Using 'metric'..."))
    method <- 'metric'
  }
  if (!(dist.method %in% c('euclidean', 'cosine', 'maximum', 'manhattan', 
                           'canberra', 'binary', 'minkowski'))) {
    message(paste0("dist.method ", dist.method, " not recognized. Choose from ",
                   "c('euclidean', 'cosine', 'maximum', 'manhattan', ",
                   "'canberra', 'binary', 'minkowski'). Using 'euclidean'..."))
    method <- 'euclidean'
  }
  if (dist.method == "cosine") {
    n <- dim(data)[1]
    X.pairs <- expand.grid(i=1:n, j=1:n)
    X.dist <- matrix(apply(X.pairs, 1, cos.dissim, x = data), n, n)
    rm(X.pairs)
  } else {
    X.dist <- as.matrix(dist(data, dist.method, diag = TRUE, upper = TRUE))
  }
  embedding <- cmdscale(X.dist, k = ndim)
  if (method != "classic") {
    embedding <- smacof::mds(X.dist, ndim = ndim, init = embedding, type="ratio", itmax = 3000)$conf
  }
  if (method == "nonmetric") {
    smacof::mds(X.dist, ndim = ndim, init = embedding, type = "ordinal", itmax = 3000)$conf
  }
  embedding
}
