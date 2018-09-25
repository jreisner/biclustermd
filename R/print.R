#' Print an object of class biclustermd 
#' @param x a \code{biclustermd} object.
#' @param ... arguments passed to or from other methods
#' 
#' @export

print.biclustermd <- function(x, ...) {
  cat("\n", x$iteration, "Iterations")
  cat("\n", "Cell-Average Matrix:", "\n")
  print(x$A)
  invisible(x)
}