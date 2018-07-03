#' Get column names in each column cluster
#' 
#' @param bc_object Biclustering object to extract column cluster designation from
#' @param data Data that contains the column names
#' 
#' @export
#' @return A data frame with two columns: \code{cluster} corresponds to the column
#'   cluster and \code{name} gives the column names in each cluster.
#'
col_cluster_names <- function(bc_object, data) {
  col_clust <- data.frame(cluster = part_matrix_to_vector(bc_object$P))
  
  col_clust$name <- colnames(data)
  
  col_clust %>% arrange(cluster) %>% return()
}