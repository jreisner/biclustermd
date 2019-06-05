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
    if(data$params$similarity == 'Rand') {
      value_df <- data$Similarities[, c("P_rand", "Q_rand", "Iteration")]
    } else if(data$params$similarity == 'HA') {
      value_df <- data$Similarities[, c("P_ha", "Q_ha", "Iteration")]
    } else if(data$params$similarity == 'Jaccard') {
      value_df <- data$Similarities[, c("P_jaccard", "Q_jaccard", "Iteration")]
    }
    names(value_df)[-3] <- c("Columns (P)", "Rows (Q)")

    p <- value_df %>%
      gather(similarity_index, Value, -Iteration) %>%
      ggplot(aes(Iteration, Value, colour = similarity_index)) +
      scale_colour_manual(paste0(data$params$similarity, " Index"), values = c("red", "blue"))
  }
  p +
    geom_line() +
    geom_point() +
    theme_bw()

}
