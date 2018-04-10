#' Plot each iteration's SSE.
#'
#' @param bicluster_obj A bicluster object.
#' @param smoother Logical. If TRUE, a smoother is added to the plot. By default it is FALSE and no smoother is added to the plot.
#' @param linear Logical. If TRUE, a line is added to the plot. By default it is FALSE and no line is added to the plot.
#' @param pt_size Point size. It's default value is 1.25.
#' @param line_width Width of lines. Default is 1.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth theme_bw
#' @return A ggplot.

gg_sse <- function(bicluster_obj, smoother = FALSE, linear = FALSE,
                   pt_size = 1.25, line_width = 1) {

  model_df <- data.frame(SSE = as.vector(na.omit(bicluster_obj$sumSSE)))

  gg <- model_df %>%
    mutate(Iteration = row_number()) %>%
    ggplot(aes(x = Iteration, y = SSE)) +
    geom_point(size = pt_size) +
    theme_bw()

  if(smoother == TRUE) {
    gg <- gg  + geom_smooth(se = FALSE, colour = "red", size = line_width)
  }

  if(linear == TRUE) {
    gg <- gg + geom_smooth(method = "lm", se = FALSE, colour = "green",
                           size = line_width)
  }

  return(gg)

}
