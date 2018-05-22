#' Run PHATE on an input data matrix
#'
#' PHATE is a data reduction method specifically designed for visualizing
#' **high** dimensional data in **low** dimensional spaces.
#'
#' @param data matrix (n_samples, n_dimensions)
#' 2 dimensional input data array with
#' n_samples samples and n_dimensions dimensions.
#' If `knn.dist.method` is 'precomputed', `data` is treated as a
#' (n_samples, n_samples) distance or affinity matrix
#' @param ndim int, optional, default: 2
#' number of dimensions in which the data will be embedded
#' @param k int, optional, default: 15
#' number of nearest neighbors on which to build kernel
#' @param alpha int, optional, default: NA
#' sets decay rate of kernel tails.
#' If NA, alpha decaying kernel is not used
#' @param use.alpha boolean, default: NA
#' forces the use of alpha decaying kernel
#' If NA, alpha decaying kernel is used for small inputs
#' (n_samples < n_landmark) and not used otherwise
#' @param n.landmark int, optional, default: 2000
#' number of landmarks to use in fast PHATE
#' @param potential.method string, optional, default: 'log'
#' choose from 'log' and 'sqrt'
#' which transformation of the diffusional operator is used
#' to compute the diffusion potential
#' @param t int, optional, default: 'auto'
#' power to which the diffusion operator is powered
#' sets the level of diffusion
#' @param knn.dist.method string, optional, default: 'euclidean'.
#' The desired distance function for calculating pairwise distances on the data.
#' If 'precomputed', `data` is treated as a
#' (n_samples, n_samples) distance or affinity matrix
#' @param init phate object, optional
#' object to use for initialization. Avoids recomputing
#' intermediate steps if parameters are the same.
#' @param mds.method string, optional, default: 'metric'
#' choose from 'classic', 'metric', and 'nonmetric'
#' which MDS algorithm is used for dimensionality reduction
#' @param mds.dist.method string, optional, default: 'euclidean'
#' recommended values: 'euclidean' and 'cosine'
#' @param t.max int, optional, default: 100.
#' Maximum value of t to test for automatic t selection.
#' @param npca int, optional, default: 100
#' Number of principal components to use for calculating
#' neighborhoods. For extremely large datasets, using
#' n_pca < 20 allows neighborhoods to be calculated in
#' log(n_samples) time.
#' @param plot.optimal.t boolean, optional, default: FALSE
#' If TRUE, produce a plot showing the Von Neumann Entropy
#' curve for automatic t selection.
#' @param verbose `int` or `boolean`, optional (default : 1)
#' If `TRUE` or `> 0`, print verbose updates.
#' @param n.jobs `int`, optional (default: 1)
#' The number of jobs to use for the computation.
#' If -1 all CPUs are used. If 1 is given, no parallel computing code is
#' used at all, which is useful for debugging.
#' For n_jobs below -1, (n.cpus + 1 + n.jobs) are used. Thus for
#' n_jobs = -2, all CPUs but one are used
#' @param seed int or `NA`, random state (default: `NA`)
#' @param n.svd Deprecated.
#' @param pca.method Deprecated.
#' @param g.kernel Deprecated.
#' @param diff.op Deprecated.
#' @param diff.op.t Deprecated.
#' @param landmark.transitions Deprecated.
#' @param dist.method Deprecated.
#'
#' @return "phate" object containing:
#'  * **embedding**: the PHATE embedding
#'  * **operator**: The PHATE operator (python phate.PHATE object)
#'  * **params**: Parameters passed to phate
#'
#' @examples
#' if (reticulate::py_module_available("phate")) {
#'
#' # Load data
#' data(tree.data)
#'
#' # Run PHATE
#' phate.tree <- phate(tree.data$data)
#' ## PHATE embedding
#' ## k = 5, alpha = NA, t = 58
#' ## Data: (3000, 100)
#' ## Embedding: (3000, 2)
#'
#' library(graphics)
#' # Plot the result with base graphics
#' plot(phate.tree, col=tree.data$branches)
#' # Plot the result with ggplot2
#' if (require(ggplot2)) {
#'   ggplot(phate.tree) +
#'     geom_point(aes(x=PHATE1, y=PHATE2, color=tree.data$branches))
#' }
#'
#' # Run PHATE again with different parameters
#' # We use the last run as initialization
#' phate.tree2 <- phate(tree.data$data, t=150, init=phate.tree)
#' # Extract the embedding matrix to use in downstream analysis
#' embedding <- as.matrix(phate.tree2)
#'
#' }
#' @export
phate <- function(data, ndim = 2, k = 15,
                  alpha = 10, use.alpha=NA,
                  n.landmark=2000, potential.method = "log",
                  t = "auto", knn.dist.method = "euclidean",
                  init=NULL,
                  mds.method = "metric", mds.dist.method = "euclidean",
                  t.max=100, npca = 100, plot.optimal.t=FALSE,
                  verbose=1, n.jobs=1, seed=NA,
                  # deprecated args, remove in v3
                  n.svd = NA,
                  pca.method = NA,
                  g.kernel=NULL, diff.op = NULL, landmark.transitions=NULL,
                  diff.op.t = NULL, dist.method=NA) {
  # check installation
  if (!reticulate::py_module_available(module = "phate")) {
    install.phate()
  }
  tryCatch(pyphate, error = function(e) load_pyphate())
  data <- as.matrix(data)
  if (!is.numeric(data)) {
    stop("data should be a numeric matrix")
  }
  # check for deprecated arguments
  if (!is.na(dist.method)) {
    message("Argument dist.method is deprecated. Use knn.dist.method instead.")
    knn.dist.method <- dist.method
  }
  if (!is.na(n.svd)) {
    message("Setting n.svd is currently not supported. Using n.svd=100")
  }
  if (!is.na(pca.method)) {
    message("Setting pca.method is deprecated. Using pca.method='random'")
  }
  if (!is.null(g.kernel)) {
    message(paste0("Setting g.kernel is deprecated. Using instead ",
                   "`knn.dist.method='precomputed'` and `data=g.kernel`"))
    data <- g.kernel
    knn.dist.method <- "precomputed"
  }
  if (!is.null(diff.op)) {
    stop(paste0("Setting diff.op is deprecated. Use `init` with a `phate` ",
                "object instead"))
  }
  if (!is.null(diff.op.t)) {
    stop(paste0("Setting diff.op.t is deprecated. Use `init` with a `phate` ",
                "object instead"))
  }
  if (!is.null(landmark.transitions)) {
    stop(paste0("Setting landmark.transitions is deprecated. Use `init` with a",
                " `phate` object instead"))
  }
  if (mds.method == "mmds") {
    message(paste0("Argument mds.method = 'mmds' is deprecated. ",
                   "Use mds.method = 'metric' instead."))
    mds.method <- "metric"
  } else if (mds.method == "cmds") {
    message(paste0("Argument mds.method = 'cmds' is deprecated. ",
                   "Use mds.method = 'classic' instead."))
    mds.method <- "classic"
  } else if (mds.method == "nmmds") {
    message(paste0("Argument mds.method = 'nmmds' is deprecated. ",
                   "Use mds.method = 'nonmetric' instead."))
    mds.method <- "nonmetric"
  } else if (!(mds.method %in% c("classic", "metric", "nonmetric"))) {
    message(paste0("mds.method ", mds.method, " not recognized. ",
                   "Choose from c('classic', 'metric, 'nonmetric'). ",
                   "Using 'metric'..."))
    mds.method <- "metric"
  }
  # decide whether or not to use the alpha decay kernel
  if (is.na(use.alpha)) {
    if (is.na(alpha) || (!is.na(n.landmark) && n.landmark < nrow(data))) {
      use.alpha <- FALSE
      alpha <- NA
    } else {
      use.alpha <- TRUE
    }
  }
  # check validity of use.alpha and alpha combination
  if (use.alpha && is.na(alpha)) {
    message("use.alpha is set to TRUE but alpha is NA. Setting use.alpha=FALSE")
    use.alpha <- FALSE
  } else if (!use.alpha && !is.na(alpha)) {
    message("use.alpha is set to FALSE but alpha is not NA. Setting alpha=NA")
    alpha <- NA
  }
  ndim <- as.integer(ndim)
  k <- as.integer(k)
  n.landmark <- as.integer(n.landmark)
  npca <- as.integer(npca)
  t.max <- as.integer(t.max)
  n.jobs <- as.integer(n.jobs)
  if (is.numeric(alpha)) {
    alpha <- as.double(alpha)
  }
  if (is.numeric(t)) {
    t <- as.integer(t)
  }
  if (is.numeric(seed)) {
    seed <- as.integer(seed)
  }
  if (is.numeric(verbose)) {
    verbose <- as.integer(verbose)
  }
  data <- as.matrix(data)
  # store parameters
  params <- list("data" = data, "k" = k, "alpha" = alpha, "t" = t,
                 "n.landmark" = n.landmark, "ndim" = ndim,
                 "potential.method" = potential.method,
                 "npca" = npca, "mds.method" = mds.method,
                 "knn.dist.method" = knn.dist.method,
                 "mds.dist.method" = mds.dist.method)
  # use pre-initialized values if given
  operator <- NULL
  if (!is.null(init)) {
    if (!methods::is(init, "phate")) {
      warning("object passed to init is not a phate object")
    }
    if (all(data == init$data) &&
        npca == init$params$npca && k == init$params$k &&
        na_equal(alpha, init$params$alpha) &&
        knn.dist.method == init$params$knn.dist.method) {
      # currently doesn't store the kernel
      if (na_equal(n.landmark, init$params$n.landmark)) {
        # TODO: should we allow n_svd to be set?
        operator <- init$operator
        if (t == init$params$t &&
            potential.method == init$params$potential.method) {
          # great! precomputed diffusion operator
          if (mds.method == init$params$mds.method &&
              mds.dist.method == init$params$mds.dist.method &&
              ndim == init$params$ndim) {
            # great! precomputed embedding
          } else {
            operator$reset_mds(mds = mds.method,
                               mds_dist = mds.dist.method)
          }
        } else {
          operator$reset_potential(t = t,
                                   potential_method = potential.method)
        }
      } else {
        # have to recompute the kernel
        operator <- NULL
      }
    }
  }
  if (is.null(operator)) {
    operator <- pyphate$PHATE(n_components = ndim,
                              k = k,
                              a = alpha,
                              t = t,
                              alpha_decay = use.alpha,
                              n_landmark = n.landmark,
                              potential_method = potential.method,
                              n_pca = npca,
                              mds = mds.method,
                              mds_dist = mds.dist.method,
                              knn_dist = knn.dist.method,
                              n_jobs = n.jobs,
                              random_state = seed,
                              verbose = verbose)
  }
  embedding <- operator$fit_transform(data,
                                t_max = t.max)
  colnames(embedding) <- c("PHATE1", "PHATE2")
  rownames(embedding) <- rownames(data)
  if (plot.optimal.t) {
    out <- operator$von_neumann_entropy(t_max = t.max)
    t <- out[[1]]
    h <- out[[2]]
    t.opt <- pyphate$vne$optimal_t(h, t)
    graphics::plot(t, h,
                   type = "l",
                   xlab = "t", ylab = "Von Neumann Entropy",
                   main = paste0("Optimal t = ", t.opt))
    graphics::points(t.opt, h[which(t == t.opt)], pch = "*", cex = 3)
  }
  result <- list("embedding" = embedding, "operator" = operator,
                 "params" = params)
  class(result) <- c("phate", "list")
  return(result)
}

#' Plot a PHATE object in base R
#'
#' @param x A fitted PHATE object
#' @param ... Arguments for plot()
#' @examples
#' if (reticulate::py_module_available("phate")) {
#'
#' library(graphics)
#' data(tree.data)
#' phate.tree <- phate(tree.data$data)
#' plot(phate.tree, col=tree.data$branches)
#'
#' }
#' @export
plot.phate <- function(x, ...) {
  graphics::plot(x$embedding[, 1], x$embedding[, 2], type = "p",
                 xlab = "PHATE1", ylab = "PHATE2", ...)
}

#' Print a PHATE object
#'
#' This avoids spamming the user's console with a list of many large matrices
#'
#' @param x A fitted PHATE object
#' @param ... Arguments for print()
#' @examples
#' if (reticulate::py_module_available("phate")) {
#'
#' data(tree.data)
#' phate.tree <- phate(tree.data$data)
#' print(phate.tree)
#' ## PHATE embedding with elements
#' ## $embedding : (3000, 2)
#' ## $operator : Python PHATE operator
#' ## $params : list with elements (data, k, alpha, t, n.landmark, ndim,
#' ##                               potential.method, npca, mds.method,
#' ##                               knn.dist.method, mds.dist.method)
#'
#' }
#' @export
print.phate <- function(x, ...) {
  result <- paste0("PHATE embedding with elements\n",
                   "  $embedding : (", nrow(x$embedding), ", ",
                   ncol(x$embedding), ")\n",
                   "  $operator : Python PHATE operator\n",
                   "  $params : list with elements (",
                   paste(names(x$params), collapse = ", "), ")\n")
  cat(result)
}

#' Summarize a PHATE object
#'
#' @param object A fitted PHATE object
#' @param ... Arguments for summary()
#' @examples
#' if (reticulate::py_module_available("phate")) {
#'
#' data(tree.data)
#' phate.tree <- phate(tree.data$data)
#' summary(phate.tree)
#' ## PHATE embedding
#' ## k = 5, alpha = NA, t = 58
#' ## Data: (3000, 100)
#' ## Embedding: (3000, 2)
#'
#' }
#' @export
summary.phate <- function(object, ...) {
  result <- paste0("PHATE embedding\n",
             "k = ", object$params$k,
             ", alpha = ", object$params$alpha,
             ", t = ", object$params$t, "\n",
            "Data: (", nrow(object$params$data),
            ", ", ncol(object$params$data), ")\n",
            "Embedding: (", nrow(object$embedding),
            ", ", ncol(object$embedding), ")\n")
  cat(result)
}

#' Convert a PHATE object to a matrix
#'
#' Returns the embedding matrix. All components can be accessed
#' using phate$embedding, phate$diff.op, etc
#'
#' @param x A fitted PHATE object
#' @param ... Arguments for as.matrix()
#' @export
as.matrix.phate <- function(x, ...) {
  x$embedding
}
#' Convert a PHATE object to a data.frame
#'
#' Returns the embedding matrix with column names PHATE1 and PHATE2
#'
#' @param x A fitted PHATE object
#' @param ... Arguments for as.data.frame()
#' @export
as.data.frame.phate <- function(x, ...) {
  as.data.frame(as.matrix(x), ...)
}

#' Convert a PHATE object to a data.frame for ggplot
#'
#' Passes the embedding matrix to ggplot with column names PHATE1 and PHATE2
#' @importFrom ggplot2 ggplot
#' @param data A fitted PHATE object
#' @param ... Arguments for ggplot()
#' @examples
#' if (reticulate::py_module_available("phate") && require(ggplot2)) {
#'
#' data(tree.data)
#' phate.tree <- phate(tree.data$data)
#' ggplot(phate.tree, aes(x=PHATE1, y=PHATE2, color=tree.data$branches)) +
#'   geom_point()
#'
#' }
#' @export
ggplot.phate <- function(data, ...) {
  ggplot2::ggplot(as.data.frame(data), ...)
}
