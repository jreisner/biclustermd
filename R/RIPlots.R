#' Plot each iteration's Rand Indices
#' 
#' @param bicluster_obj A bicluster object.
#' @param pch Point shape. This is equivalent to the `pch` argument for base graphics.
#' @param cex Scale of the points in the plot.
#' @param lwd Width of the smoothing spline line.
#' @export
#' @importFrom graphics points
#' @return A plot

RI_plots <- function(bicluster_obj, pch = 20, cex = 0.5, lwd = 1) {
  model_df <- data.frame(PRI = as.vector(na.omit(bicluster_obj$RIs[, 1])),
                         QRI = as.vector(na.omit(bicluster_obj$RIs[, 2])))
  model_df$iteration <- seq_along(model_df$PRI)
  
  plot(model_df$iteration, model_df$PRI, pch = pch, cex = cex,
       ylab = "Rand Index", xlab = "Iteration", ylim = c(0, 1), col = 2)
  lines(model_df$iteration, model_df$PRI, col = 2, lwd = lwd)
  points(model_df$iteration, model_df$QRI, col = 4, pch = pch, cex = cex)
  lines(model_df$iteration, model_df$QRI, col = 4, lwd = lwd)
  
  legend("bottomright", c("P Rand Index", "Q Rand Index"), lty = rep(1, 2), 
         pch = rep(20, 2), col = c(2, 4), lwd = rep(2, 2), cex = 0.8)
}
