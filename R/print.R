#' Print an object of class biclustermd 
#' @param x a \code{biclustermd} object.
#' @param ... arguments passed to or from other methods
#' 
#' @export

print.biclustermd <- function(x, ...) {
  cat("\n ", x$iteration, " Iterations", sep = "")
  cat("\n ", "Initial SSE = ", format(round(x$InitialSSE), big.mark = ","),
      "; Final SSE = ", format(round(x$SSE[x$iteration, 1]), big.mark = ","), sep = "")
  cat("\n ", "Rand Indices: ", "P = ", round(x$RIs[x$iteration, 1], 3), ", Q = ",
      round(x$RIs[x$iteration, 2], 3), sep = "")
  cat("\n ", "Cell-Average Matrix:", "\n", sep = "")
  print(data.frame(x$A))
  invisible(x)
}