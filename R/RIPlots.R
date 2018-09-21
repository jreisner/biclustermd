#' Plot each iteration's Rand Indices
#'
#' @param x A bicluster object.
#' @param ... Arguments to be passed to base R line and point graphics.
#' @export
#' @importFrom graphics points
#' @return A plot

RI_plots <- function(x, ...) {
  model_df <- data.frame(PRI = as.vector(na.omit(x$RIs[, 1])),
                         QRI = as.vector(na.omit(x$RIs[, 2])))
  model_df$iteration <- seq_along(model_df$PRI)

  y_min <- floor(min(model_df) * 100) / 100

  plot(model_df$iteration, model_df$PRI, ylim = c(y_min, 1),
       ylab = "Rand Index", xlab = "Iteration", col = 2, ...)
  lines(model_df$iteration, model_df$PRI, col = 2, ...)
  points(model_df$iteration, model_df$QRI, col = 4, ...)
  lines(model_df$iteration, model_df$QRI, col = 4, ...)

  legend("bottomright", c("P Rand Index", "Q Rand Index"), lty = rep(1, 2),
         pch = rep(20, 2), col = c(2, 4), lwd = rep(2, 2), cex = 0.8)
}
