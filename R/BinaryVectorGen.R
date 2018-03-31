#' Make a binary vector with all values equal to zero except for one
#'
#' @param n Desired vector length.
#' @param i0 Index whose value is one.
#' @return A vector

binary_vector_gen <- function(n, i0){
  a <- rep(0, n)
  a[i0] <- 1
  return(a)
}