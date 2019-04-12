#' KMeans on the PHATE potential

#' Clustering on the PHATE operator as introduced in Moon et al.
#' This is similar to spectral clustering.
#' 
#' @param phate `phate()` output
#' @param k Number of clusters (default: 8)
#' @param seed Random seed for kmeans (default: NULL)
#' 
#' @return clusters Integer vector of cluster assignments
#' 
#' @examples
#' if (reticulate::py_module_available("phate")) {
#'
#' # Load data
#' # data(tree.data)
#' # We use a smaller tree to make examples run faster
#' data(tree.data.small)
#'
#' # Run PHATE
#' phate.tree <- phate(tree.data.small$data)
#' 
#' # Clustering
#' cluster_phate(phate.tree)
#' }
#' @export
cluster_phate <- function(phate, k=8, seed=NULL) {
  # check installation
  if (!reticulate::py_module_available(module = "phate")) {
    load_pyphate()
  }
  tryCatch(pyphate, error = function(e) load_pyphate())
  if (!methods::is(phate, "phate")) {
    stop(paste0("Expected phate_op to be a phate object. Got a ", class(phate)))
  }
  k <- as.integer(k)
  if (is.numeric(seed)) {
    seed <- as.integer(seed)
  } else if (!is.null(seed) && is.na(seed)) {
    seed <- NULL
  }
  pyphate$cluster$kmeans(phate$operator, k=k, random_state=seed)
}