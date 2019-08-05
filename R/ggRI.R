#' Plot each iteration's Rand Indices using the ggplot2 framework
#'
#' @param x A bicluster object.
#' @param ... Arguments to be passed to `geom_point()` and `geom_line()`.
#'
#' @importFrom graphics points
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_colour_manual theme_bw
#' @return A ggplot
#'
#'
gg_ri <- function(x, ...) {
  model_df <- data.frame(Column = as.vector(na.omit(x$RIs[, 1])),
                         Row = as.vector(na.omit(x$RIs[, 2])),
                         Iteration = as.vector(na.omit(x$RIs[, 3])))

  gg <- model_df %>%
    gather(`Rand Index`, Value, -Iteration) %>%
    ggplot(aes(x = Iteration, y = Value, colour = `Rand Index`)) +
    geom_line(...) +
    geom_point(...) +
    scale_colour_manual(values = c("red", "blue")) +
    theme_bw()

  return(gg)
}
