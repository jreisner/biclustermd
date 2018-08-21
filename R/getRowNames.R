#' Get row names in each row cluster
#'
#' @param bc_object Biclustering object to extract row cluster designation from
#' @param data Data that contains the row names
#'
#' @export
#' @return A data frame with two columns: \code{cluster} corresponds to the row
#'   cluster and \code{name} gives the row names in each cluster.
#'
row_cluster_names <- function(bc_object, data) {
  row_clust <- data.frame(row_cluster = part_matrix_to_vector(bc_object$Q))

  row_clust$name <- rownames(data)

  row_clust %>% arrange(row_cluster) %>% return()
}
#' @examples
#' data("synthetic")
#' P01 <- partition_gen(12, 3)
#' Q01 <- partition_gen(6, 2)
#'
#' bc <- bicluster(synthetic, P01, Q01, miss_val = mean(synthetic, na.rm = TRUE),
#'                 miss_val_sd = sd(synthetic, na.rm = TRUE),
#'                 col_min_num = 2, row_min_num = 2,
#'                 col_num_to_move = 1, row_num_to_move = 1,
#'                 max.iter = 10)
#' row_cluster_names(bc, synthetic)
#' col_cluster_names(bc, synthetic)
