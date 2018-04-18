#' Run PHATE on an input data matrix
#'
#' PHATE is a data reduction method specifically designed for visualizing **high**
#' dimensional data in **low** dimensional spaces.
#'
#' @param data matrix (n_samples, n_dimensions)
#' 2 dimensional input data array with
#' n_samples samples and n_dimensions dimensions
#' @param ndim int, optional, default: 2
#' number of dimensions in which the data will be embedded
#' @param k int, optional, default: 5
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
#' @param t int, optional, default: 'auto'
#' power to which the diffusion operator is powered
#' sets the level of diffusion
#' @param potential.method string, optional, default: 'log'
#' choose from 'log' and 'sqrt'
#' which transformation of the diffusional operator is used
#' to compute the diffusion potential
#' @param init phate object, optional
#' object to use for initialization. Avoids recomputing 
#' intermediate steps if parameters are the same.
#' @param verbose boolean, optional, default : TRUE
#' If TRUE, print verbose updates.
#' @param t.max int, optional, default: 100.
#' Maximum value of t to test for automatic t selection.
#' @param plot.optimal.t boolean, optional, default: FALSE
#' If TRUE, produce a plot showing the Von Neumann Entropy 
#' curve for automatic t selection.
#' @param pca.method string, optional, default: 'random'
#' The desired method for implementing pca for preprocessing the
#' data. Options include 'svd', 'random', and 'none' (no pca).
#' @param npca int, optional, default: 100
#' Number of principal components to use for calculating
#' neighborhoods. For extremely large datasets, using
#' n_pca < 20 allows neighborhoods to be calculated in
#' log(n_samples) time.
#' @param n.svd int, optional, default: 100
#' The number of SVD components to use for landmark selection
#' @param mds.method string, optional, default: 'metric'
#' choose from 'classic', 'metric', and 'nonmetric'
#' which MDS algorithm is used for dimensionality reduction
#' @param knn.dist.method string, optional, default: 'euclidean'.
#' The desired distance function for calculating pairwise distances on the data. 
#' @param mds.dist.method string, optional, default: 'euclidean'
#' recommended values: 'euclidean' and 'cosine'
#' @param g.kernel Precomputed kernel matrix
#' @param diff.op Precomputed diffusion operator
#' @param landmark.transitions Precomputed landmark transitions
#' @param diff.op.t Precomputed powered diffusion operator
#'
#' @return "phate" object containing:
#'  * **embedding**: the PHATE embedding
#'  * **diff.op**: The diffusion operator which can be used as optional input with another run.
#'  * **diff.op.t**: diff.op^t
#'  * **g.kernel**: The kernel used to construct the diffusion operator
#'  * **params**: Parameters passed to phate
#'
#' @export

phate <- function(data, ndim = 2, t = 'auto', k = 5, alpha = 10, use.alpha=NA,
                  n.landmark=2000, potential.method = 'log', t.max=100,
                  pca.method = 'random', npca = 100, n.svd = 100, mds.method = 'metric',
                  knn.dist.method = 'euclidean', mds.dist.method = 'euclidean',
                  init=NULL, verbose=TRUE, plot.optimal.t=FALSE,
                  g.kernel=NULL, diff.op = NULL, landmark.transitions=NULL,
                  diff.op.t = NULL, dist.method=NA) {
  start_time <- Sys.time()
  tmp_start_time <- Sys.time()
  # check for deprecated arguments
  if (!is.na(dist.method)) {
    message("Argument dist.method is deprecated. Use knn.dist.method instead.")
    knn.dist.method <- dist.method
  }
  if (mds.method == "mmds") {
    message("Argument mds.method = 'mmds' is deprecated. Use mds.method = 'metric' instead.")
    mds.method <- 'metric'
  } else if (mds.method == "cmds") {
    message("Argument mds.method = 'cmds' is deprecated. Use mds.method = 'classic' instead.")
    mds.method <- 'classic'
  } else if (mds.method == "nmmds") {
    message("Argument mds.method = 'nmmds' is deprecated. Use mds.method = 'nonmetric' instead.")
    mds.method <- 'nonmetric'
  } else if (!(mds.method %in% c("classic", "metric", "nonmetric"))) {
    message(paste0("mds.method ", mds.method, " not recognized. Choose from c('classic', 'metric, 'nonmetric'). Using 'metric'..."))
    mds.method <- 'metric'
  }

  # decide whether or not to use the alpha decay kernel
  if (is.na(use.alpha)) {
    if (is.na(alpha) || (!is.na(n.landmark) && n.landmark < nrow(data))) {
      use.alpha=FALSE
      alpha=NA
    } else {
      use.alpha=TRUE
    }
  }

  # check validity of use.alpha and alpha combination
  if (use.alpha && is.na(alpha)) {
    message("use.alpha is set to TRUE but alpha is NA. Setting use.alpha=FALSE")
    use.alpha=FALSE
  } else if (!use.alpha && !is.na(alpha)) {
    message("use.alpha is set to FALSE but alpha is not NA. Setting alpha=NA")
    alpha=NA
  }

  # store parameters
  params <- list("data"=data, "k"=k, "alpha"=alpha, "t"=t, "n.landmark"=n.landmark,
                 "potential.method"=potential.method, "pca.method"=pca.method,
                 "npca"=npca, "n.svd"=n.svd, "mds.method"=mds.method,
                 "knn.dist.method"=knn.dist.method,
                 "mds.dist.method"=mds.dist.method)

  # use pre-initialized values if given
  if (!is.null(init)) {
    if (!methods::is(init, "phate")) {
      warning("object passed to init is not a phate object")
    }
    if (all(data==init$data) && pca.method==init$params$pca.method &&
        npca==init$params$npca && k==init$params$k && 
        na_equal(alpha, init$params$alpha) &&
        knn.dist.method==init$params$knn.dist.method) {
      g.kernel=init$g.kernel
      if (na_equal(n.landmark, init$params$n.landmark) && n.svd==init$params$n.svd) {
        diff.op <- init$diff.op
        landmark.transitions <- init$landmark.transitions
        if (((is.numeric(t) && t == init$t) || (is.character(t) && t == init$params$t)) &&
            potential.method==init$params$potential.method) {
          diff.op.t <- init$diff.op.t
          t <- init$t
        }
      }
    }
  }

  eps <- .Machine$double.eps

  if (is.null(g.kernel) && is.null(diff.op) && is.null(diff.op.t)) {
    if (verbose) message("Calculating kernel...")
    g.kernel <- calculate.kernel(data, k=k, alpha=alpha, 
                                 npca=npca, pca.method=pca.method,
                                 knn.dist.method=knn.dist.method)
    if (verbose) {
      end_time <- Sys.time()
      message(paste0("Calculated kernel in ",
              format(round(end_time-tmp_start_time, 1), format="%S"),
              "."))
      tmp_start_time <- end_time
    }
  } else {
    if (verbose) message("Using precomputed kernel...")
  }

  if (is.null(diff.op) & is.null(diff.op.t)) {
    if (verbose) message("Calculating diffusion operator...")
    result <- calculate.landmark.operator(g.kernel, n.landmark, n.svd)
    diff.op <- result$diff.op
    landmark.transitions <- result$landmark.transitions
    rm(result)
    if (verbose) {
      end_time <- Sys.time()
      message(paste0("Calculated diffusion operator in ",
                     format(round(end_time-tmp_start_time, 1), format="%S"),
                     "."))
      tmp_start_time <- end_time
    }
  } else {
    if (verbose) message("Using precomputed diffusion operator...")
  }

  if (is.null(diff.op.t)) {
    if (verbose) message("Calculating diffusion potential...")
    if (t == 'auto') {
      t <- optimal.t(diff.op, t.max=t.max, plot=plot.optimal.t,
                     double.step=!is.null(landmark.transitions))
      if (verbose) message(paste0("Automatically selected t = ", t))
    }
    diff.op.t <- expm::`%^%`(diff.op, if (is.null(landmark.transitions)) t else t %/% 2)
    if (potential.method == 'log') {
      diff.op.t[diff.op.t <= eps] <- eps
      diff.op.t <- -log(diff.op.t)
    } else if (potential.method == 'sqrt') {
      diff.op.t <- sqrt(diff.op.t)
    } else {
      message(paste0('Potential method ', potential.method,
                     ' not recognised. Choose from "log" and "sqrt". Using "log"...'))
      diff.op.t <- -log(diff.op.t)
    }
    if (verbose) {
      end_time <- Sys.time()
      message(paste0("Calculated diffusion potential in ",
                     format(round(end_time-tmp_start_time, 1), format="%S"),
                     "."))
      tmp_start_time <- end_time
    }
  } else {
    if (verbose) message("Using precomputed diffusion potential...")
  }

  if (verbose) message(paste0('Embedding ', mds.method, " MDS..."))
  embedding <- mds(diff.op.t, ndim=ndim, method=mds.method, 
                   dist.method=mds.dist.method)
  if (!is.null(landmark.transitions)) {
    embedding <- Matrix::as.matrix(landmark.transitions %*% embedding)
  }
  colnames(embedding) <- paste0("PHATE", 1:ncol(embedding))
  rownames(embedding) <- rownames(data)
  if (verbose) {
    end_time <- Sys.time()
    message(paste0("Calculated MDS in ",
                   format(round(end_time-tmp_start_time, 1), format="%S"),
                   "."))
  }
  result <- list("embedding" = embedding, "diff.op" = diff.op, "t"=t,
                 "diff.op.t" = diff.op.t, "g.kernel" = g.kernel,
                 "landmark.transitions" = landmark.transitions,
                 "params"=params)
  class(result) <- c("phate", "list")
  if (verbose) {
    message(paste0("Embedded PHATE in ", 
                   format(round(Sys.time()-start_time, 1), format="%S"),
                   "."))
  }
  return(result)
}

#' Plot a PHATE object in base R
#' 
#' @param phate A fitted PHATE object
#' @param ... Arguments for plot()
#' @examples 
#' data(tree.data)
#' phate.tree <- phate(tree.data$data)
#' plot(phate.tree, col=tree.data$branches))
#' @export
plot.phate <- function(phate, ...) {
  plot(phate$embedding[,1], phate$embedding[,2], type='p', 
       xlab="PHATE1", ylab="PHATE2", ...)
}

#' Print a PHATE object
#' 
#' This avoids spamming the user's console with a list of many large matrices
#' 
#' @param phate A fitted PHATE object
#' @param ... Arguments for print()
#' @examples 
#' data(tree.data)
#' phate.tree <- phate(tree.data$data)
#' print(phate.tree)
#' ## PHATE embedding with elements
#' ##   $embedding : (3000, 2)
#' ##   $g.kernel : (3000, 3000)
#' ##   $diff.op : (2000, 2000)
#' ##   $diff.op.t : (2000, 2000)
#' ##   $params : list with elements (data, k, alpha, t, n.landmark, 
#' ##                                 potential.method, pca.method, 
#' ##                                 npca, n.svd, mds.method, 
#' ##                                 knn.dist.method, mds.dist.method)
#' @export
print.phate <- function(phate, ...) {
  result <- paste0("PHATE embedding with elements\n",
                   "  $embedding : (", nrow(phate$embedding), ", ", ncol(phate$embedding), ")\n",
                   "  $g.kernel : (", nrow(phate$g.kernel), ", ", ncol(phate$g.kernel), ")\n",
                   "  $diff.op : (", nrow(phate$diff.op), ", ", ncol(phate$diff.op), ")\n",
                   "  $diff.op.t : (", nrow(phate$diff.op.t), ", ", ncol(phate$diff.op.t), ")\n",
                   "  $params : list with elements (", paste(names(phate$params), collapse=", "), ")")
  cat(result)
}

#' Summarize a PHATE object
#' 
#' @param phate A fitted PHATE object
#' @param ... Arguments for summary()
#' @examples 
#' data(tree.data)
#' phate.tree <- phate(tree.data$data)
#' print(phate.tree)
#' ## PHATE embedding
#' ## k = 5, alpha = NA, t = 58
#' ## Data: (3000, 100)
#' ## Embedding: (3000, 2)
#' @export
summary.phate <- function(phate, ...) {
  result <- paste0("PHATE embedding\n",
             "k = ", phate$params$k, ", alpha = ", phate$params$alpha, ", t = ", phate$t, "\n",
            "Data: (", nrow(phate$params$data), ", ", ncol(phate$params$data), ")\n",
            "Embedding: (", nrow(phate$embedding), ", ", ncol(phate$embedding), ")")
  cat(result)
}


#' Convert a PHATE object to a matrix
#' 
#' Returns the embedding matrix. All components can be accessed 
#' using phate$embedding, phate$diff.op, etc
#' 
#' @param phate A fitted PHATE object
#' @param ... Arguments for as.matrix()
#' @export
as.matrix.phate <- function(phate, ...) {
  phate$embedding
}

#' Convert a PHATE object to a data.frame
#' 
#' Returns the embedding matrix with column names PHATE1 and PHATE2
#' 
#' @param phate A fitted PHATE object
#' @param ... Arguments for as.data.frame()
#' @export
as.data.frame.phate <- function(phate, ...) {
  as.data.frame(as.matrix(phate))
}

#' Convert a PHATE object to a data.frame for ggplot
#' 
#' Passes the embedding matrix to ggplot with column names PHATE1 and PHATE2
#' 
#' @param phate A fitted PHATE object
#' @param ... Arguments for ggplot()
#' @examples 
#' data(tree.data)
#' phate.tree <- phate(tree.data$data)
#' ggplot2::ggplot(phate.tree, aes(x=PHATE1, y=PHATE2, color=tree.data$branches)) +
#'   geom_point()
#' @export
ggplot.phate <- function(phate, ...) {
  ggplot2::ggplot(as.data.frame(phate), ...)
}

