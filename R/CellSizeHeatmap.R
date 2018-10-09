#' Make a heat map of bicluster cell sizes.
#'
#' @param x An object of class \code{biclustermd}.
#' @param linewidth Width of vertical and horizontal lines. Default is 0.1.
#' @param log_scale Logical. If TRUE, log10 scale on the legend is used. Default is FALSE.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate row_number
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradientn theme_bw theme labs element_blank
#' @importFrom stats quantile
#' @importFrom grDevices rainbow
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
#' cell_heatmap(bc)

cell_heatmap <- function(x, linewidth = 0.1, log_scale = FALSE) {

  bc <- x

  cell_sizes <- colSums(bc$Q) %*% t(colSums(bc$P))
  colnames(cell_sizes) <- seq(1, ncol(cell_sizes))
  cell_sizes <- as.data.frame(cell_sizes)

  cell_sizes <- cell_sizes %>%
    mutate(row_proto = row_number()) %>%
    gather(col_proto, cell_size, -row_proto)

  gg <- cell_sizes %>%
    ggplot(aes(x = col_proto, y = row_proto, fill = cell_size)) +
    geom_tile(colour = "grey30", size = linewidth) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = "Column Cluster Index", y = "Row Cluster Index",
         fill = "Cell Size")


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
