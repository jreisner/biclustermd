#' Plot each iteration's SSE.
#'
#' @param x A bicluster object.
#' @param ... Arguments to be passed to `geom_point`
#' @param smoother Logical. If TRUE, a smoother is added to the plot.
#'     By default it is FALSE and no smoother is added to the plot.
#' @param linear Logical. If TRUE, a line is added to the plot.
#'     By default it is FALSE and no line is added to the plot.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth theme_bw
#' @return A ggplot.

gg_sse <- function(x, ..., smoother = FALSE, linear = FALSE) {

  model_df <- data.frame(SSE = as.vector(na.omit(x$SSE[, 1])),
                         Iteration = as.vector(na.omit(x$SSE[, 2])))

  gg <- model_df %>%
    ggplot(aes(x = Iteration, y = SSE)) +
    geom_point(...) +
    theme_bw()

  if(smoother == TRUE) {
    gg <- gg  + geom_smooth(se = FALSE, colour = "red")
  }

  if(linear == TRUE) {
    gg <- gg + geom_smooth(method = "lm", se = FALSE, colour = "green")
  }

  return(gg)

}
