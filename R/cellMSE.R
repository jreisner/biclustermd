#' Make a data frame containing the MSE for each bicluster cell
#'
#' @param bc_object A bicluster object.
#' @param data Data the bicluster object was generated from
#' @export
#' @return A data frame

cell_mse <- function(bc_object, data) {
  data <- as.matrix(data)

  mse_df <- expand.grid(RowProto = 1:ncol(bc_object$Q),
                        ColProto = 1:ncol(bc_object$P))

  nr <- nrow(mse_df)

  RowProtoCount <- colSums(bc_object$Q, na.rm = TRUE)
  ColProtoCount <- colSums(bc_object$P, na.rm = TRUE)
  mse_df$RowProtoCount <- unlist(lapply(1:nr, function(x) RowProtoCount[mse_df$RowProto[x]]))
  mse_df$ColProtoCount <- unlist(lapply(1:nr, function(x) ColProtoCount[mse_df$ColProto[x]]))

  mse_df$CellMean <- unlist(lapply(1:nr, function(x) {

    bc_object$A[mse_df$RowProto[x], mse_df$ColProto[x]]

  }), use.names = FALSE)

  cell_mses <- function(row) {
    cell_mean <- mse_df$CellMean[row]

    rows <- which(bc_object$Q[, mse_df$RowProto[row]] == 1)
    cols <- which(bc_object$P[, mse_df$ColProto[row]] == 1)

    cell_values <- na.omit(as.vector(data[rows, cols]))

    mean((cell_mean - cell_values) ^ 2)
  }

  mse_df$CellMSE <- unlist(lapply(1:nr, cell_mses), use.names = FALSE)

  return(mse_df)
}
