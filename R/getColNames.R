#' Get column names in each column cluster
#'
#' @param bc_object Biclustering object to extract column cluster designation from
#' @param data Data that contains the column names
#'
#' @export
#' @return A data frame with two columns: \code{cluster} corresponds to the column
#'   cluster and \code{name} gives the column names in each cluster.
#'
#' @examples
#' data("synthetic")
#' rownames(dat) <- letters[1:nrow(dat)]
#' colnames(dat) <- letters[1:col(dat)]
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
col_cluster_names <- function(bc_object, data) {
  col_clust <- data.frame(col_cluster = part_matrix_to_vector(bc_object$P))

  col_clust$name <- colnames(data)

  col_clust %>% arrange(col_cluster) %>% return()
}

