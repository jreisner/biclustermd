#' Plot the SSE or Rand Indices of a \code{biclustermd} object.
#'
#' @param data an object of class \code{biclustermd}.
#' @param mapping unused; included to match \code{ggplot2} generic
#' @param value which value to plot. Can be either "sse" or "similarity".
#' @param ... unused; included to match \code{ggplot2} generic
#' @param environment unused; included to match \code{ggplot2} generic
#' @export
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual geom_line geom_point theme_bw scale_x_continuous
#' @importFrom tidyr gather
#' @return A data frame
ggplot.biclustermd <- function(data, mapping = NULL, value = c("sse", "similarity"), ..., environment = NULL) {
  if(value == "sse") {
    value_df <- data.frame(data$SSE)

    p <- value_df %>%
      ggplot(aes(Iteration, SSE))
  } else if(value == "similarity") {
    value_df <- data.frame(data$Similarities)
    if(x$params$similarity == 'Rand') {
      value_df <- x$Similarities[, c("P_rand", "Q_rand")]
    } else if(x$params$similarity == 'HA') {
      value_df <- x$Similarities[, c("P_ha", "Q_ha")]
    } else if(x$params$similarity == 'Jaccard') {
      value_df <- x$Similarities[, c("P_jaccard", "Q_jaccard")]
    }
    names(value_df) <- c("Columns (P)", "Rows (Q)")

    p <- value_df %>%
      gather(`Rand Index`, Value, -Iteration) %>%
      ggplot(aes(Iteration, Value, colour = `Rand Index`)) +
      scale_colour_manual(paste0(x$params$similarity, " Index"), values = c("red", "blue"))
  }
  p +
    geom_line() +
    geom_point() +
    theme_bw()

}
