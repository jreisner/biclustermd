#' Make a heatmap of sparse biclustering results
#'
#' @param bc_object A bicluster object.
#' @param data The raw data that was biclustered.
#' @param col_clusts A vector of column cluster indices to display. If NULL (default), all are displayed.
#' @param row_clusts A vector of row cluster indices to display. If NULL (default), all are displayed.
#' @param transform_colors If equals `TRUE` then the data is scaled by
#'     `c` and run through a standard normal cdf before plotting. If `FALSE` (default), raw data
#'     values are used in the heat map.
#' @param c Value to scale the data by before running it through a standard normal CDF.
#'     Default is 1/6.
#' @param ... Arguments to be passed to `geom_vline()` and `geom_hline()`.
#' @export
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_tile geom_hline geom_vline scale_fill_gradientn theme_bw theme
#' @importFrom grDevices rainbow
#' @return An object of class ggplot.

gg_bicluster <- function(bc_object, data, col_clusts = NULL, row_clusts = NULL,
                         transform_colors = FALSE, c = 1/6, ...) {

  bc <- bc_object
  P <- bc$P
  Q <- bc$Q

  if(is.null(col_clusts)) {
    col_clusts <- c(1:ncol(P))
  }

  if(is.null(row_clusts)) {
    row_clusts <- c(1:ncol(Q))
  }

  cols <- as.integer(part_matrix_to_vector(P))
  rows <- as.integer(part_matrix_to_vector(Q))

  p_list <- lapply(1:ncol(P), function(x) which(cols == x))
  p_list <- p_list[sapply(p_list, length) != 0]
  p_list <- p_list[col_clusts]

  q_list <- lapply(1:ncol(Q), function(x) which(rows == x))
  q_list <- q_list[sapply(q_list, length) != 0]
  q_list <- q_list[row_clusts]

  col_ord <- unlist(p_list)
  row_ord <- unlist(q_list)

  ord_dat <- data[row_ord, col_ord]

  ord_dat <- as.data.frame(ord_dat)
  ord_dat$rows <- rownames(ord_dat)

  melted <- ord_dat %>% gather(cols, value, -rows)
  melted$rows <- factor(melted$rows, levels = unique(melted$rows))
  melted$cols <- factor(melted$cols, levels = unique(melted$cols))

  vline_coords <- cumsum(sapply(p_list, length)) + 0.5
  vline_coords <- data.frame(v = vline_coords[-length(vline_coords)])

  hline_coords <- cumsum(sapply(q_list, length)) + 0.5
  hline_coords <- data.frame(h = hline_coords[-length(hline_coords)])

  melted$plot_data <- pnorm(c * melted$value)

  res_list <- list(data = melted,
                   vlines = vline_coords,
                   hlines = hline_coords)

  if(transform_colors == TRUE) {
    gg <- ggplot() +
      geom_tile(data = res_list$data, aes(y = rows, x = cols, fill = plot_data)) +
      geom_vline(data = res_list$vlines, aes(xintercept = v), ...) +
      geom_hline(data = res_list$hlines, aes(yintercept = h), ...)  +
      scale_fill_gradientn(colours = rev(rainbow(250, start = 0, end = 0.7)),
                           na.value = "white") +
      theme_bw() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

  } else if(transform_colors == FALSE) {
    gg <- ggplot() +
      geom_tile(data = res_list$data, aes(y = rows, x = cols, fill = value)) +
      geom_vline(data = res_list$vlines, aes(xintercept = v), ...) +
      geom_hline(data = res_list$hlines, aes(yintercept = h), ...)  +
      scale_fill_gradientn(colours = rev(rainbow(250, start = 0, end = 0.7)),
                           na.value = "white") +
      theme_bw() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  }

  return(gg)

}

