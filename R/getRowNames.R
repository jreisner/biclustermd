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
  row_clust <- data.frame(cluster = part_matrix_to_vector(bc_object$Q))
  
  row_clust$name <- rownames(data)
  
  row_clust %>% arrange(cluster) %>% return()
}