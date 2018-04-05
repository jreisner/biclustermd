#' Make a heatmap of cell MSEs
#'
#' @param mse_obj A data frame returned by `cell_mse`
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradientn theme_bw labs
#' @importFrom stats pnorm quantile
#' @importFrom grDevices rainbow
#' @return A ggplot object.

mse_heatmap <- function(mse_obj) {

  gg <- mse_obj %>%
    mutate(trans_mse = pnorm(CellMSE / 50)) %>%
    mutate(CellMSE_plus1 = CellMSE + 1) %>%
    ggplot(aes(x = ColProto, y = RowProto, fill = CellMSE)) +
    geom_tile(colour = "gray35", size = 0.000001) +
    scale_fill_gradientn(colours = rev(rainbow(250, start = 0, end = 0.7)),
                         na.value = "white", trans = "log10") +
    theme_bw()

  return(gg)
}
