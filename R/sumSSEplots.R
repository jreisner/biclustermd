#' Plot each iteration's sum of SSEs and a smoothing spline fit to the data.
#'
#' @param bicluster_obj A bicluster object.
#' @param linear Logical. If `TRUE`, a linear fit is plotted.
#' @param pch Point shape. This is equivalent to the `pch` argument for base graphics.
#' @param cex Scale of the points in the plot.
#' @param lwd Width of lines.
#' @export
#' @importFrom gam gam
#' @importFrom graphics plot lines legend
#' @importFrom stats coef lm na.omit
#' @return A plot

sumSSE_plots <- function(bicluster_obj, linear = FALSE, pch = 20, cex = 1, lwd = 3) {

  model_df <- data.frame(sumSSE = as.vector(na.omit(bicluster_obj$SSE)))
  model_df$iteration <- seq_along(model_df$sumSSE)

  lin_m <- lm(sumSSE ~ iteration, data = model_df)
  model_df$linear <- lin_m$fitted.values

  smoothing <- gam(model_df$sumSSE ~ s(model_df$iteration))
  model_df$smooth <- smoothing$fitted.values

  slope <- round(unname(coef(lin_m))[2], 3)
  p_value <- round(unname(summary(lin_m)$coefficients[, 4][2]), 3)

  plot(model_df$iteration, model_df$sumSSE, pch = pch, cex = cex,
       ylab = "SSE", xlab = "Iteration",
       main = paste0("Slope = ", slope, ", p-value = ", p_value))

  lines(model_df$iteration, model_df$smooth, col = 2, lwd = lwd)
  legend("topright", c("Smoothing Spline"), lty = 1,
         col = 2, lwd = 2, cex = 0.8)

  if(linear == TRUE) {
    lines(model_df$iteration, model_df$linear, col = 3, lwd = lwd)

    legend("topright", c("Smoothing Spline", "Linear Model"), lty = rep(1, 2),
           col = 2:3, lwd = rep(2, 2), cex = 0.8)
  }


}

