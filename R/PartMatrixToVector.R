#' Convert a partition matrix to a vector
#' 
#' @param P0 A partition matrix
#' @return An integer vector
#' @description For each row in a partition matrix, this function gets the column index for which the row is equal to one.
#'   That is, for row i, this function returns the index of the row entry that is equal to one.

# get a vector of the row/col prototype each row/col is in (essentially, for 
# each row of Q/P, get the column of Q/P that is nonzero (has a 1 in it).)
# pmat2vec.f
part_matrix_to_vector <- function(P0){
  P <- as.matrix(P0)
  
  a <- nrow(P)
  b <- ncol(P)
  
  vec <- numeric(a)
  
  Q <- diag(1:b)
  for (i in 1:a){
    vec[i] <- sum(Q %*% P[i,])
  }
  return(vec)
}