#' Calculate a Gaussian or kNN kernel
#' 
#' @param data The data (n.samples x n.samples)
#' @param k number of neighbors
#' @param alpha decay constant
#' @param npca number of dimensions of PCA reduction to do before KNN
#' @param pca.method The desired method for implementing pca for preprocessing the
#'               data. Options include 'svd', 'random', and 'none' (no pca).
#'               Default is 'random'.
#'               
#' @return g.kernel Kernel matrix (n.samples x n.samples)
calculate.kernel <- function(data, k=15, alpha=10, 
                             npca=100, pca.method='random',
                             knn.dist.method = 'euclidean') {
  # kernel includes self as connection but not in k
  # actually search for k+1 neighbors including self
  k <- k + 1
  if (ncol(data) > npca) {
    data <- svdpca(data, npca, pca.method)
  }
  if (!is.na(alpha)) {
    pdx <- as.matrix(dist(data, knn.dist.method, diag = TRUE, upper = TRUE))
    knn.eps <- apply(pdx, 1, sort, partial=k)[k,]
    g.kernel <- exp(-(pdx / knn.eps) ^ alpha)
    rm(pdx, knn.eps)
  } else {
    knn.index <- cbind(1:nrow(data), # add ones on diagonal
                       FNN::knn.index(data, k=k-1, algorithm="kd_tree"))
    g.kernel <- Matrix::sparseMatrix(i=rep(1:nrow(data), k), j=as.vector(knn.index), x=1)
  }
  g.kernel <- g.kernel + Matrix::t(g.kernel)
  g.kernel
}

#' Calculate the landmark operator
#'
#' @param g.kernel Kernel matrix (n.samples x n.samples)
#' @param n.landmark Number of landmarks to compute, or NA. Default is 2000. If NA, then exact PHATE is computed.
#' @param seed Integer or NA, integer value for random number generator.
#' @param n.svd Number of singular vectors to calculate for landmark selection. Default is 100.
#'
#' @return List containing:
#'
#'  * **diff.op** The diffusion operator (n.landmark x n.landmark)
#'  * **landmark.transitions** Transition matrix between cells and landmark (n.cells x n.landmark)
calculate.landmark.operator <- function(g.kernel, n.landmark=2000, n.svd=100) {
  diff.op <- g.kernel / Matrix::rowSums(g.kernel)
  if (!is.na(n.landmark) && n.landmark < nrow(g.kernel)) {
    # Compute landmark operator
    svd <- irlba::irlba(diff.op, nu=n.svd, nv=0, work=500)
    weighted.svd <- svd$u %*% diag(svd$d)
    init_fraction <- min(1, 3*n.landmark/nrow(g.kernel))
    clusters <- kmeans(weighted.svd, n.landmark)$cluster
    landmarks <- unique(clusters)
    p.nm <- sapply(landmarks, function(i) Matrix::Matrix(tryCatch(Matrix::rowSums(g.kernel[,clusters==i]),
                                                                  error=function(e) {
                                                                    # throws error if only one sample per cluster
                                                                    g.kernel[,clusters==i]
                                                                  })))
    # convert sparse vectors to sparse matrix
    p.nm <- Matrix::sparseMatrix(i=unlist(sapply(p.nm, function(i) i@i))+1, # i is input 1-based but converted to 0-based
                                 p=c(0,cumsum(sapply(p.nm, function(i) i@p[2]))),
                                 x=unlist(sapply(p.nm, function(i) i@x)),
                                 dims=c(nrow(g.kernel), length(landmarks)))
    p.mn <- Matrix::t(p.nm)
    # row normalize
    p.nm <- p.nm / Matrix::rowSums(p.nm)
    p.mn <- p.mn / Matrix::rowSums(p.mn)
    diff.op <- Matrix::as.matrix(p.mn %*% p.nm)
  } else {
    # Compute diffusion operator
    diff.op <- Matrix::as.matrix(diff.op)
    p.nm <- NULL
  }
  return(list(diff.op=diff.op,
              landmark.transitions=p.nm))
}
