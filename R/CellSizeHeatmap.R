#' Make a heat map of bicluster cell sizes.
#' 
#' @param bc_object An object created by biclustering
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate row_number
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradientn theme_bw theme labs element_blank
#' @importFrom stats quantile
#' @importFrom grDevices rainbow

cell_heatmap <- function(bc_object) {
  
  bc <- bc_object
  
  cell_sizes <- colSums(bc$Q) %*% t(colSums(bc$P))
  colnames(cell_sizes) <- seq(1, ncol(cell_sizes))
  cell_sizes <- as.data.frame(cell_sizes)
  
  cell_sizes <- cell_sizes %>%
    mutate(row_proto = row_number()) %>%
    gather(col_proto, cell_size, -row_proto)
  
  quants <- round(quantile(cell_sizes$cell_size, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 0)
  
  subtitl <- paste0("Min = ", quants[1], ", ",
                    "Q1 = ", quants[2], ", ",
                    "Mean = ", round(mean(cell_sizes$cell_size, na.rm = TRUE), 0), ", ",
                    "Median = ", quants[3], ", ",
                    "Q3 = ", quants[4], ", ",
                    "Max = ", quants[5])
  
  gg <- cell_sizes %>%
    ggplot(aes(x = col_proto, y = row_proto, fill = cell_size)) +
    geom_tile(colour = "grey30", size = 0.05) + 
    scale_fill_gradientn(colours = rev(rainbow(250, start = 0, end = 0.7)),
                         na.value = "white", trans = "log10") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = "Column Prototype Index", y = "Row Prototype Index", 
         subtitle = subtitl, fill = "Cell Size")
  
  return(gg)
}
