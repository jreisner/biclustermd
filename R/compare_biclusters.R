#' Compare two biclusterings or a pair of partition matrices
#' 
#' @param bc1 the first biclustering or partition matrix. Must be either of class 
#'   \code{biclustermd} or \code{matrix}.
#' @param bc2 the second biclustering or partition matrix. Must be either of class 
#'   \code{biclustermd} or \code{matrix}.
#'   
#' @export
#' 
#' @importFrom clues adjustedRand
#' 
#' @return If comparing a pair of biclusterings, a list containing the column 
#'   similarity indices and the row similarity indices, in that order. If a pair of matrices,
#'   a vector of similarity indices.
#' 
#' @examples 
#' data("synthetic")
#' bc <- biclustermd(synthetic, col_clusters = 3, row_clusters = 2)
#' bc2 <- biclustermd(synthetic, col_clusters = 3, row_clusters = 2)
#' 
#' # compare the two biclusterings
#' compare_biclusters(bc, bc2)
#' 
#' # determine the similarity between initial and final row clusterings
#' compare_biclusters(bc$Q0, bc$Q)
#' 
compare_biclusters <- function(bc1, bc2) {
  
  if(("biclustermd" %in% class(bc1)) & ("biclustermd" %in% class(bc2))) {
    
    P1 <- part_matrix_to_vector(bc1$P)
    P2 <- part_matrix_to_vector(bc2$P)
    
    Q1 <- part_matrix_to_vector(bc1$Q)
    Q2 <- part_matrix_to_vector(bc2$Q)
    
    list(
      "P Similarity" = adjustedRand(P1, P2, randMethod = c("Rand", "HA", "Jaccard")),
      "Q Similarity" = adjustedRand(Q1, Q2, randMethod = c("Rand", "HA", "Jaccard"))
    )
    
  } else if(("matrix" %in% class(bc1)) & ("matrix" %in% class(bc2))) {
    
    adjustedRand(
      part_matrix_to_vector(bc1),
      part_matrix_to_vector(bc2),
      randMethod = c("Rand", "HA", "Jaccard")
    )
    
  }
  
}