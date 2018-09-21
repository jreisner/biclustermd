#' Make a data frame containing the MSE for each bicluster cell
#'
#' @param x A bicluster object.
#' @param data Data the bicluster object was generated from
#' @export
#' @return A data frame

cell_mse <- function(x, data) {
  data <- as.matrix(data)

  mse_df <- expand.grid(row_cluster = 1:ncol(x$Q),
                        col_cluster = 1:ncol(x$P))

  nr <- nrow(mse_df)

  row_cluster_count <- colSums(x$Q, na.rm = TRUE)
  col_cluster_count <- colSums(x$P, na.rm = TRUE)
  mse_df$row_cluster_count <- unlist(lapply(1:nr, function(x) row_cluster_count[mse_df$row_cluster[x]]))
  mse_df$col_cluster_count <- unlist(lapply(1:nr, function(x) col_cluster_count[mse_df$col_cluster[x]]))

  mse_df$CellMean <- unlist(lapply(1:nr, function(x) {

    x$A[mse_df$row_cluster[x], mse_df$col_cluster[x]]

  }), use.names = FALSE)

  cell_mses <- function(row) {
    cell_mean <- mse_df$CellMean[row]

    rows <- which(x$Q[, mse_df$row_cluster[row]] == 1)
    cols <- which(x$P[, mse_df$col_cluster[row]] == 1)

    cell_values <- na.omit(as.vector(data[rows, cols]))

    mean((cell_mean - cell_values) ^ 2)
  }

  mse_df$CellMSE <- unlist(lapply(1:nr, cell_mses), use.names = FALSE)

  return(mse_df)
}
