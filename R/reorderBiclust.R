#' Reorder a bicluster object for making a heat map
#' 
#' @param bc_object A bicluster object.
#' @export
#' @return A list containing the two partition matrices used by gg_bicluster.

reorder_biclust <- function(bc_object) {
  bc <- bc_object
  
  bc_p <- bc$P[, order(-colMeans(bc$A))]
  bc_q <- bc$Q[, order(rowMeans(bc$A))]
  
  return(list(P = bc_p, Q = bc_q))
}