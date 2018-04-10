#' Calculate the landmark operator
#' 
#' @param g.kernel Kernel matrix [n.samples x n.samples]
#' @param n.landmark Number of landmarks to compute, or NA. Default is 1000. If NA, then exact PHATE is computed.
#' @param seed Integer or NA, integer value for random number generator.
#' @param n.svd Number of singular vectors to calculate for landmark selection. Default is 100.
#' 
#' @return List containing:
#' 
#'  * **diff.op** The diffusion operator [n.landmark x n.landmark]
#'  * **landmark.transitions** Transition matrix between cells and landmark [n.cells x n.landmark]
calculate.landmark.operator <- function(g.kernel, n.landmark=1000, n.svd=100) {
  diff.op <- g.kernel / Matrix::rowSums(g.kernel)
  if (!is.na(n.landmark) && n.landmark < nrow(g.kernel)) {
    # Compute landmark operator
    svd <- irlba::irlba(diff.op, nu=n.svd, nv=0, work=500)
    if (is.na(seed)) seed <- ceiling(runif(1, 0, 2**15))
    weighted.svd <- svd$u %*% diag(svd$d)
    init_fraction <- min(1, 3*n.landmark/nrow(g.kernel))
    k.means <- ClusterR::MiniBatchKmeans(weighted.svd, n.landmark, 
                                     batch_size=100, num_init=3, max_iters=100,
                                     init_fraction=init_fraction,
                                     initializer="kmeans++", seed=seed)
    clusters <- ClusterR::predict_MBatchKMeans(weighted.svd, k.means$centroids)
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
                                 dims=c(nrow(data), length(landmarks)))
    p.mn <- Matrix::t(p.nm)
    # row normalize
    p.nm <- p.nm / Matrix::rowSums(p.nm)
    p.mn <- p.mn / Matrix::rowSums(p.mn)
    diff.op <- Matrix::as.matrix(p.mn %*% p.nm)
  } else {
    # Compute diffusion operator
    p.nm <- NA
  }
  return(list(diff.op=diff.op,
              landmark.transitions=p.nm))
}

#' Runs PHATE on an input data matrix
#'
#' PHATE is a data reduction method specifically designed for visualizing **high**
#' dimensional data in **low** dimensional spaces.
#'
#' @param data Data matrix. Must have cells on the rows and genes on the columns
#' @param t Diffusion time scale. Default is 20.
#' @param k k for the adaptive kernel bandwidth. Default is 5.
#' @param alpha The alpha parameter in the exponent of the kernel function. Determines the kernel decay rate. Default is 10.
#' @param ndim The number of desired PHATE dimensions in the output Y. 2 or 3
#'         is best for visualization. A higher number can be used for
#'         running other analyses on the PHATE dimensions.
#' @param pca.method The desired method for implementing pca for preprocessing the
#'               data. Options include 'svd', 'random', and 'none' (no pca).
#'               Default is 'random'.
#' @param npca The number of PCA components for preprocessing the data. Default is 100.
#' @param mds.method Method for implementing MDS. Choices are 'cmds', 'mmds', and 'nmmds'. Default is 'cmds'.
#' @param knn.dist.method The desired distance function for calculating pairwise
#'                distances on the data. Default is 'euclidean'.
#' @param mds.dist.method The desired distance function for MDS. Choices are 'euclidean'
#'                     and 'cosine'. Default is 'euclidean'.
#' @param diff.op If the diffusion operator has been computed on a prior run with the
#'             desired parameters, then this option can be used to directly input the
#'             diffusion operator to save on computational time. Default is NA.
#' @param diff.op.t Same as for 'DiffOp', if the powered diffusion operator has been
#'               computed on a prior run with the desired parameters then this
#'               option can be used to directly input the diffusion operator to
#'               save on computational time. Default is NA.
#'
#' @export
#'
#' @return List containing:
#'
#'  * **Y**: the PHATE embedding
#'
#'  * **diff.op**: The diffusion operator which can be used as optional input with another run.
#'
#'  * **diff.op.t**: diff.op^t
#'
#'  * **g.kernel**: The kernel used to construct the diffusion operator
#'
#'
#'
phate <- function(data, t = 'auto', k = 5, alpha = NA, ndim = 2, n.landmark=1000,
                  potential.method = 'log', t.max=100,
                  pca.method = 'random', npca = 100, n.svd = 100, mds.method = 'mmds',
                  knn.dist.method = 'euclidean', mds.dist.method = 'euclidean', diff.op = NA,
                  diff.op.t = NA, dist.method=NA) {
  if (!is.na(dist.method)) {
    message("Argument dist.method is deprecated. Use knn.dist.method instead.")
    knn.dist.method <- dist.method
  }
  eps <- .Machine$double.eps
  g.kernel <- 0 # initial value, in case it is not required for the output
  if (is.na(diff.op) & is.na(diff.op.t)) {
    if (ncol(data) > npca) {
      data <- svdpca(data, npca, pca.method)
    }
    if (!is.na(alpha)) {
      pdx <- as.matrix(dist(data, knn.dist.method, diag = TRUE, upper = TRUE))
      knn.eps <- apply(pdx, 1, sort, partial=4)[k,]
      g.kernel <- exp(-(pdx / knn.eps) ^ alpha)
      rm(pdx, knn.eps)
    } else {
      knn.index <- FNN::knn.index(data, k=k)
      g.kernel <- Matrix::sparseMatrix(i=rep(1:nrow(data), k), j=as.vector(knn.index))
    }
    g.kernel <- g.kernel + Matrix::t(g.kernel)
    result <- calculate.landmark.operator(g.kernel, n.landmark)
    diff.op <- result$diff.op
    landmark.transitions <- result$landmark.transitions
    rm(result)
  }
  
  if (t == 'auto') {
    t <- optimal.t(diff.op, t.max=t.max)
  }
  if (is.na(diff.op.t)) {
    diff.op.t <- expm::`%^%`(diff.op, t)
  }
  
  diff.op.t[diff.op.t <= eps] <- eps
  if (potential.method == 'log') {
    diff.op.t <- -log(diff.op.t)
  } else if (potential.method == 'sqrt') {
    diff.op.t <- sqrt(diff.op.t)
  } else {
    message(paste0('Potential method ', potential.method, 
                   ' not recognised. Choose from "log" and "sqrt". Using "log"...'))
    diff.op.t <- -log(diff.op.t)
  }
  
  message(paste0('MDS distance method: ', mds.dist.method))
  if (mds.dist.method == 'euclidean') {
    X <- svdpca(diff.op.t, npca, pca.method)
    X.dist <- as.matrix(dist(X, mds.dist.method, diag = TRUE, upper = TRUE))
  } else if (mds.dist.method == "cosine") {
    n <- dim(diff.op.t)[1]
    X.pairs <- expand.grid(i=1:n, j=1:n)
    X.dist <- matrix(apply(X.pairs, 1, cos.dissim, x = diff.op.t), n, n)
    rm(X.pairs)
  }
  rm(X)
  message(paste0('MDS method: ', mds.method))
  embedding <- switch(mds.method, cmds = cmdscale(X.dist, k = ndim),
                      mmds = smacof::mds(X.dist, ndim = ndim, init = cmdscale(X.dist, k = ndim), type="ratio", itmax = 3000)$conf,
                      nmmds = smacof::mds(X.dist, ndim = ndim, init = "torgerson", type = "ordinal", itmax = 3000)$conf)
  if (is.matrix(landmark.transitions) || is(landmark.transitions, "sparseMatrix")) {
    embedding <- landmark.transitions %*% embedding
  }
  return(list("embedding" = embedding, "diff.op" = diff.op, "diff.op.t" = diff.op.t, "g.kernel" = g.kernel))
}
