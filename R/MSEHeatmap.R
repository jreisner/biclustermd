#' Make a heatmap of cell MSEs
#'
#' @param mse_obj A data frame returned by `cell_mse`.
#' @param linewidth Width of vertical and horizontal lines. Default is 0.1.
#' @param log_scale Logical. If TRUE, fill legend will be on a log10 scale. Default is FALSE.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradientn theme_bw labs
#' @importFrom stats pnorm quantile
#' @importFrom grDevices rainbow
#' @return A ggplot object.

mse_heatmap <- function(mse_obj, linewidth = 0.1, log_scale = FALSE) {

  gg <- mse_obj %>%
    mutate(trans_mse = pnorm(CellMSE / 50)) %>%
    mutate(CellMSE_plus1 = CellMSE + 1) %>%
    ggplot(aes(x = col_cluster, y = row_cluster, fill = CellMSE)) +
    geom_tile(colour = "gray35", size = linewidth) +
    theme_bw() +
    labs(x = "Column Cluster Index",
         y = "Row Cluster Index")

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
