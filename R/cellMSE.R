#' Make a data frame containing the MSE for each bicluster cell
#'
#' @param bc_object A bicluster object.
#' @param data Data the bicluster object was generated from
#' @export
#' @return A data frame

cell_mse <- function(bc_object, data) {
  data <- as.matrix(data)

  mse_df <- expand.grid(row_cluster = 1:ncol(bc_object$Q),
                        col_cluster = 1:ncol(bc_object$P))

  nr <- nrow(mse_df)

  row_cluster_count <- colSums(bc_object$Q, na.rm = TRUE)
  col_cluster_count <- colSums(bc_object$P, na.rm = TRUE)
  mse_df$row_cluster_count <- unlist(lapply(1:nr, function(x) row_cluster_count[mse_df$row_cluster[x]]))
  mse_df$col_cluster_count <- unlist(lapply(1:nr, function(x) col_cluster_count[mse_df$col_cluster[x]]))

  mse_df$CellMean <- unlist(lapply(1:nr, function(x) {

    bc_object$A[mse_df$row_cluster[x], mse_df$col_cluster[x]]

  }), use.names = FALSE)

  cell_mses <- function(row) {
    cell_mean <- mse_df$CellMean[row]

    rows <- which(bc_object$Q[, mse_df$row_cluster[row]] == 1)
    cols <- which(bc_object$P[, mse_df$col_cluster[row]] == 1)

    cell_values <- na.omit(as.vector(data[rows, cols]))

    mean((cell_mean - cell_values) ^ 2)
  }

  mse_df$CellMSE <- unlist(lapply(1:nr, cell_mses), use.names = FALSE)

  return(mse_df)
}
