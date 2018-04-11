#' Plot each iteration's Rand Indices using the ggplot2 framework
#'
#' @param bicluster_obj A bicluster object.
#' @param ... Arguments to be passed to `geom_point()` and `geom_line()`.
#' @export
#' @importFrom graphics points
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_colour_manual theme_bw
#' @return A ggplot
#' 
#' 
gg_ri <- function(bicluster_obj, ...) {
  model_df <- data.frame(Column = as.vector(na.omit(bicluster_obj$RIs[, 1])),
                         Row = as.vector(na.omit(bicluster_obj$RIs[, 2])))
  model_df$Iteration <- seq_along(model_df$Column)
  
  gg <- model_df %>%
    gather(`Rand Index`, value, -Iteration) %>%
    ggplot(aes(x = Iteration, y = value, colour = `Rand Index`)) +
    geom_line(...) +
    geom_point(...) +
    scale_colour_manual(values = c("red", "blue")) +
    theme_bw()
  
  return(gg)
}
