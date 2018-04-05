#' Make a heatmap of sparse biclustering results
#'
#' @param bc_object A bicluster object.
#' @param data The raw data that was biclustered.
#' @param transform_colors If equals `TRUE` (default) then the data is scaled by
#' `c` and run through a standard normal cdf before plotting. If `FALSE`, raw data
#' values are used in the heat map.
#' @param c Value to scale the data by before running it through a standard normal CDF. Default is 2/15.
#' @export
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_tile geom_hline geom_vline scale_fill_gradientn theme_bw theme
#' @importFrom grDevices rainbow
#' @return An object of class ggplot.

gg_bicluster <- function(bc_object, data, transform_colors = TRUE, c = 2/15) {

  bc <- bc_object

  P <- bc$P
  Q <- bc$Q

  cols <- as.integer(part_matrix_to_vector(P))
  rows <- as.integer(part_matrix_to_vector(Q))

  p_list <- lapply(1:ncol(P), function(x) which(cols == x))
  p_list <- p_list[sapply(p_list, length) != 0]

  q_list <- lapply(1:ncol(Q), function(x) which(rows == x))
  q_list <- q_list[sapply(q_list, length) != 0]

  col_ord <- unlist(p_list)
  row_ord <- unlist(q_list)

  ord_dat <- data[row_ord, col_ord]

  melted <- data.frame(cbind(rownames(ord_dat), ord_dat))
  colnames(melted)[1] <- "rows"


  melted <- melted %>%
    gather(cols, value, -rows) %>%
    mutate(value = as.numeric(value))

  vline_coords <- cumsum(sapply(p_list, length)) + 0.5
  vline_coords <- data.frame(v = vline_coords)

  hline_coords <- cumsum(sapply(q_list, length)) + 0.5
  hline_coords <- data.frame(h = hline_coords)

  melted$plot_data <- pnorm(c * melted$value)

  res_list <- list(data = melted,
                   vlines = vline_coords,
                   hlines = hline_coords)

  if(transform_colors == TRUE) {
    gg <- ggplot() +
      geom_tile(data = res_list$data, aes(y = rows, x = cols, fill = plot_data)) +
      geom_vline(data = res_list$vlines, aes(xintercept = v), size = 0.1, colour = "black") +
      geom_hline(data = res_list$hlines, aes(yintercept = h), size = 0.1, colour = "black")  +
      scale_fill_gradientn(colours = rev(rainbow(250, start = 0, end = 0.7)),
                           na.value = "white") +
      theme_bw() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank())
  } else if(transform_colors == FALSE) {
    gg <- ggplot() +
      geom_tile(data = res_list$data, aes(y = rows, x = cols, fill = value)) +
      geom_vline(data = res_list$vlines, aes(xintercept = v),
                 size = 0.1, colour = "black") +
      geom_hline(data = res_list$hlines, aes(yintercept = h),
                 size = 0.1, colour = "black")  +
      scale_fill_gradientn(colours = rev(rainbow(250, start = 0, end = 0.7)),
                           na.value = "white") +
      theme_bw() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank())
  }

  return(gg)

}

