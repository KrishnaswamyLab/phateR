# Return TRUE if x and y are equal or both NA
na_equal <- function(x, y) {
  if (is.na(x) && is.na(y)) {
    return(TRUE)
  } else if (is.na(x) || is.na(y)) {
    return(FALSE)
  } else {
    return(x == y)
  }
}