#' Make a heatmap of cell MSEs
#'
#' @param x An object of class \code{biclustermd}.
#' @param linewidth Width of vertical and horizontal lines. Default is 0.1.
#' @param log_scale Logical. If TRUE, fill legend will be on a log10 scale. Default is FALSE.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradientn theme_bw labs
#' @importFrom stats pnorm quantile
#' @importFrom grDevices rainbow
#' @return A ggplot object.
#' @examples
#' data("synthetic")
#' P01 <- partition_gen(12, 3)
#' Q01 <- partition_gen(6, 2)
#'
#' bc <- bicluster(synthetic, P01, Q01, miss_val = mean(synthetic, na.rm = TRUE),
#'                 miss_val_sd = sd(synthetic, na.rm = TRUE),
#'                 col_min_num = 2, row_min_num = 2,
#'                 col_num_to_move = 1, row_num_to_move = 1,
#'                 max.iter = 10)
#' mse_heatmap(bc)

mse_heatmap <- function(x, linewidth = 0.1, log_scale = FALSE) {

  mse_obj <- cell_mse(x)

  gg <- mse_obj %>%
    ggplot(aes(x = col_cluster, y = row_cluster, fill = CellMSE)) +
    geom_tile(colour = "gray35", size = linewidth) +
    theme_bw() +
    labs(x = "Column Cluster Index",
         y = "Row Cluster Index",
         fill = "Cell MSE")

  if(log_scale == TRUE) {
    gg <- gg +
      scale_fill_gradientn(colours = rev(rainbow(250, start = 0, end = 0.7)),
                           na.value = "white", trans = "log10")
  } else {
    gg <- gg +
      scale_fill_gradientn(colours = rev(rainbow(250, start = 0, end = 0.7)),
                           na.value = "white")
  }

  return(gg)
}
