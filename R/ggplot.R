#' Plot the SSE or Rand Indices of a \code{biclustermd} object.
#'
#' @param data an object of class \code{biclustermd}. 
#' @param mapping unused; included to match \code{ggplot2} generic 
#' @param value which value to plot. Can be either "sse" or "ri".
#' @param ... unused; included to match \code{ggplot2} generic 
#' @param environment unused; included to match \code{ggplot2} generic 
#' @export
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual geom_line geom_point theme_bw scale_x_continuous
#' @importFrom tidyr gather
#' @return A data frame
ggplot.biclustermd <- function(data, mapping = NULL, value = c("sse", "ri"), ..., environment = NULL) {
  if(value == "sse") {
    value_df <- data.frame(x$SSE)
    
    p <- value_df %>%
      ggplot(aes(Iteration, SSE)) 
  } else if(value == "ri") {
    value_df <- data.frame(x$RIs)
    names(value_df)[-3] <- c("P", "Q")
    
    p <- value_df %>%
      gather(`Rand Index`, Value, -Iteration) %>%
      ggplot(aes(Iteration, Value, colour = `Rand Index`)) +
      scale_colour_manual(values = c("red", "blue"))
  }
  p +
    geom_line() +
    geom_point() +
    theme_bw() +
    scale_x_continuous(breaks = 0:(x$iteration - 1))
  
}