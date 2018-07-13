# Return TRUE if x and y are equal or both NA
null_equal <- function(x, y) {
  if (is.null(x) && is.null(y)) {
    return(TRUE)
  } else if (is.null(x) || is.null(y)) {
    return(FALSE)
  } else {
    return(x == y)
  }
}

load_pyphate <- function(delay_load = FALSE) {
  result <- try(pyphate <<- reticulate::import("phate", delay_load = delay_load))
  if (methods::is(result, "try-error")) {
    install.phate()
  }
}

#' Install PHATE Python Package
#'
#' Install PHATE Python package into a virtualenv or conda env.
#'
#' On Linux and OS X the "virtualenv" method will be used by default
#' ("conda" will be used if virtualenv isn't available). On Windows,
#' the "conda" method is always used.
#'
#' @param envname Name of environment to install packages into
#' @param method Installation method. By default, "auto" automatically finds
#' a method that will work in the local environment. Change the default to
#' force a specific installation method. Note that the "virtualenv" method
#' is not available on Windows.
#' @param conda Path to conda executable (or "auto" to find conda using the PATH
#'  and other conventional install locations).
#' @param pip Install from pip, if possible.
#' @param ... Additional arguments passed to conda_install() or
#' virtualenv_install().
#'
#' @export
install.phate <- function(envname = "r-reticulate", method = "auto",
                          conda = "auto", pip=TRUE, ...) {
  tryCatch({
    message("Attempting to install PHATE python package with reticulate")
    reticulate::py_install("phate",
      envname = envname, method = method,
      conda = conda, pip=pip, ...
    )
  },
  error = function(e) {
    stop(paste0(
      "Cannot locate PHATE Python package, please install through pip ",
      "(e.g. pip install --user phate)."
    ))
  }
  )
}

pyphate <- NULL

.onLoad <- function(libname, pkgname) {
  load_pyphate(delay_load = TRUE)
}
