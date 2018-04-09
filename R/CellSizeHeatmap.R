#' Make a heat map of bicluster cell sizes.
#'
#' @param bc_object An object created by biclustering
#' @param linewidth Width of vertical and horizontal lines. Default is 0.1.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate row_number
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradientn theme_bw theme labs element_blank
#' @importFrom stats quantile
#' @importFrom grDevices rainbow

cell_heatmap <- function(bc_object, linewidth = 0.1) {

  bc <- bc_object

  cell_sizes <- colSums(bc$Q) %*% t(colSums(bc$P))
  colnames(cell_sizes) <- seq(1, ncol(cell_sizes))
  cell_sizes <- as.data.frame(cell_sizes)

  cell_sizes <- cell_sizes %>%
    mutate(row_proto = row_number()) %>%
    gather(col_proto, cell_size, -row_proto)

  gg <- cell_sizes %>%
    ggplot(aes(x = col_proto, y = row_proto, fill = cell_size)) +
    geom_tile(colour = "grey30", size = linewidth) +
    scale_fill_gradientn(colours = rev(rainbow(250, start = 0, end = 0.7)),
                         na.value = "white", trans = "log10") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = "Column Prototype Index", y = "Row Prototype Index",
         fill = "Cell Size")

  return(gg)
}
