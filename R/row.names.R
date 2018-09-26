#' Get data matrix row names and their corresponding row cluster membership
#' 
#' @param x and object of class \code{biclustermd}
#' 
#' @return a data frame with row names of the shuffled matrix and corresponding row cluster names.
#' 
#' @export

row.names.biclustermd <- function (x) {
  row_clust <- data.frame(row_cluster = part_matrix_to_vector(x$Q))
  row_clust$name <- rownames(x$data)
  row_clust <- row_clust %>% arrange(row_cluster)
  row_clust
}