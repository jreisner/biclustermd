#' Bicluster data
#'
#' @param data Dataset to bicluster. Must to be a data matrix with only numbers and missing values in the data set. It should have row names and column names.
#' @param col_clusters The number of clusters to partition the columns into.
#' @param row_clusters The number of clusters to partition the rows into.
#' @param miss_val Value or function to put in empty cells of the prototype matrix.
#'     If a value, a random normal variable with sd = `miss_val_sd` is used each iteration.
#' @param miss_val_sd Standard deviation of the normal distribution `miss_val` follows
#'     if `miss_val` is a number. By default this equals 1.
#' @param similarity The metric used to compare two successive clusterings. Can be
#'     "Rand" (default), "HA" for the Hubert and Arabie adjusted Rand index or "Jaccard".
#'     See \link[clues]{adjustedRand} for details.
#' @param col_min_num Minimum column prototype size in order to be eligible to be chosen when filling an empty row prototype. Default is 5.
#' @param row_min_num Minimum row prototype size in order to be eligible to be chosen when filling an empty row prototype. Default is 5.
#' @param col_num_to_move Number of columns to remove from the sampled prototype to put in the empty column prototype. Default is 1.
#' @param row_num_to_move Number of rows to remove from the sampled prototype to put in the empty row prototype. Default is 1.
#' @param row_shuffles Number of times to shuffle rows in each iteration. Default is 1.
#' @param col_shuffles Number of times to shuffle columns in each iteration. Default is 1.
#' @param max.iter Maximum number of iterations to let the algorithm run for.
#' @param verbose Logical. If TRUE, will report progress.
#' @export
#' @importFrom clues adjustedRand
#' @importFrom stats rnorm
#' @return A list containing all arguments passed to the function, final matrices
#'     for column and row partitions, the SSE of the original partitioning,
#'     the SSE for each iteration, the similarity scores for row and column
#'     prototypes, the number of iterations the algorithm ran for, and the final
#'     prototype matrix.
#' @examples
#' data("synthetic")
#'
#' bc <- bicluster(synthetic, col_clusters = 3, row_clusters = 2,
#'                 miss_val = mean(synthetic, na.rm = TRUE),
#'                 miss_val_sd = sd(synthetic, na.rm = TRUE),
#'                 col_min_num = 2, row_min_num = 2,
#'                 col_num_to_move = 1, row_num_to_move = 1,
#'                 max.iter = 10)
#' bc


bicluster <- function(data, col_clusters, row_clusters, miss_val,
                      miss_val_sd = 1, similarity = "Rand",
                      row_min_num = 5, col_min_num = 5,
                      row_num_to_move = 1, col_num_to_move = 1,
                      row_shuffles = 1, col_shuffles = 1,
                      max.iter = 100, verbose = TRUE) {

  if(length(similarity) > 1) {
    warning(
      paste0("Only one similarity metric can be used as a stopping condition. Using the first supplied, ", similarity[1])
    )
    similarity <- similarity[1]
  }

  if(!(class(data) %in% c("matrix", "data.frame"))) {
    stop("`data` must be a matrix or a data.frame.")
  } #else if(is.null(rownames(data)) & is.null(colnames(data))) {
    # warning("`data` does not have row or column names. For interpretation of results, it is advised to have row and column names.")
  # }

  if(is.expression(miss_val)) {
    condition_call <- substitute(miss_val)
    if(is.na(eval(condition_call))) {
      stop("`miss_val` is NA")
    }
  }


  data <- as.matrix(data)
  m_d <- nrow(data)
  n_d <- ncol(data)

  P0 <- partition_gen(n_d, col_clusters)
  Q0 <- partition_gen(m_d, row_clusters)

  P <- P0
  Q <- Q0

  result_list <- vector("list", 11)
  names(result_list) <- c("params", "data", "P0", "Q0", "InitialSSE", "P", "Q", "SSE", "Similarities", "iteration", "A")
  result_list$params <- mget(names(formals()),sys.frame(sys.nframe()))
  result_list$P0 <- P0
  result_list$Q0 <- Q0

  InitialSSE <- cluster_iteration_sum_sse(data, P, Q)

  SSE <- matrix(nrow = max.iter, ncol = 2)
  colnames(SSE) <- c("SSE", "Iteration")

  # RIs <- matrix(nrow = max.iter, ncol = 3)
  # colnames(RIs) <- c("P_sim", "Q_sim", "Iteration")

  Similarities <- matrix(nrow = max.iter, ncol = 7)
  colnames(Similarities) <- c("P_rand", "P_ha", "P_jaccard", "Q_rand", "Q_ha", "Q_jaccard", "Iteration")

  n_p <- ncol(P)
  n_q <- ncol(Q)

  s <- 0

  A <- matrix(nrow = n_q, ncol = n_p)
  p1 <- numeric(n_d)
  q1 <- numeric(m_d)

  dat2 <- data
  for (i in 1:m_d){
    for (j in 1:n_d){
      if (!is.na(data[i, j])) {
        dat2[i, j] <- 1
      }
    }
  }

  while(s < max.iter) {
    if(verbose) {
      if(s %% 10 == 0) {
        cat("Iteration ", s, "\n")
      }
    }


    P_old <- P
    Q_old <- Q


    for(k in 1:row_shuffles) {
      for (j in 1:n_q){
        q_ind <- which(Q[, j] == 1)

        for (i in 1:n_p){
          p_ind <- which(P[, i] == 1)

          x_ij_prime <- data[q_ind, p_ind]

          A[j, i] <- sum(x_ij_prime, na.rm = TRUE) /
            (length(x_ij_prime) - sum(is.na(x_ij_prime)))

          if(is.na(A[j, i])) {
            if(is.numeric(miss_val)) {
              A[j, i] <- rnorm(1, miss_val, miss_val_sd)
            } else {
              condition_call <- substitute(miss_val)
              A[j, i] <- eval(condition_call)
            }
          }
        }
      }


      col_cluster_mean <- matrix(0, nrow = m_d, ncol = n_p)  # is M_{in}^R in paper
      distq <- matrix(nrow = m_d, ncol = n_q)  # is d_{im}^R in paper
      dq <- matrix(0, nrow = n_q, ncol = n_p)  # is value in parens in d_{im}^R in paper
      for(i in 1:m_d) {
        col_cluster_cnt <- apply(dat2[i,] * P, 2, sum, na.rm = T)
        col_cluster_mean[i,] <- apply(data[i,] * P, 2, sum, na.rm = T) / col_cluster_cnt
        col_cluster_mean[i, which(col_cluster_mean[i,] == 0 | is.na(col_cluster_mean[i,]))] <- miss_val
        for(j in 1:n_q) {
          dq[j,] <- A[j,] - col_cluster_mean[i,]
          distq[i, j] <- sum((dq[j,] ^ 2) * col_cluster_cnt, na.rm = TRUE)
        }
      }

      q1 <- unlist(lapply(1:m_d, function(x) which.min(distq[x,])), use.names = FALSE)

      Q <- partition_gen_by_p(m_d, n_q, q1)

      Q <- random_assign_unassigned_fill_empties_Q(data, Q, row_min_num, row_num_to_move)
    }

    for(k in 1:col_shuffles) {
      for (j in 1:n_q) {
        q_ind <- which(Q[, j] == 1)
        for (i in 1:n_p) {
          p_ind <- which(P[, i] == 1)

          x_ij_prime <- data[q_ind, p_ind]

          A[j, i] <- sum(x_ij_prime, na.rm = TRUE) /
            (length(x_ij_prime) - sum(is.na(x_ij_prime)))

          if(is.na(A[j, i])) {
            if(is.numeric(miss_val)) {
              A[j, i] <- rnorm(1, miss_val, miss_val_sd)
            } else {
              condition_call <- substitute(miss_val)
              A[j, i] <- eval(condition_call)
            }
          }
        }
      }

      row_cluster_mean <- matrix(0, nrow = n_d, ncol = n_q)  # is M_{jm}^R in paper
      distp <- matrix(nrow = n_d, ncol = n_p)  # is d_{jn}^R in paper
      dp <- matrix(0, nrow = n_p, ncol = n_q)  # is value in parens in d_{jn}^R in paper
      for (i in 1:n_d){
        row_cluster_cnt <- apply(dat2[, i] * Q, 2, sum, na.rm = T)
        row_cluster_mean[i, ] <- apply(data[, i] * Q, 2, sum, na.rm = T) / row_cluster_cnt
        row_cluster_mean[i, which(row_cluster_mean[i, ] == 0 | is.na(row_cluster_mean[i, ]))] <- miss_val
        for (j in 1:n_p){
          dp[j,] <- A[, j] - row_cluster_mean[i, ]
          distp[i, j] <- sum((dp[j, ] ^ 2) * row_cluster_cnt)
        }
      }

      p1 <- unlist(lapply(1:n_d, function(x) which.min(distp[x,])), use.names = FALSE)

      P <- partition_gen_by_p(n_d, n_p, p1)

      P <- random_assign_unassigned_fill_empties_P(data, P, col_min_num, col_num_to_move)
    }

    s <- s + 1

    SSE[s, 1] <- cluster_iteration_sum_sse(data, P, Q)
    SSE[s, 2] <- s - 1

    P_old_vec <- part_matrix_to_vector(P_old) + 1
    P_new_vec <- part_matrix_to_vector(P) + 1
    Q_old_vec <- part_matrix_to_vector(Q_old) + 1
    Q_new_vec <- part_matrix_to_vector(Q) + 1

    # PRI <- RRand(P_old_vec, P_new_vec)[[1]]
    P_sim <- adjustedRand(P_old_vec, P_new_vec, randMethod = similarity)
    # QRI <- RRand(Q_old_vec, Q_new_vec)[[1]]
    Q_sim <- adjustedRand(Q_old_vec, Q_new_vec, randMethod = similarity)

    # RIs[s, 1] <- P_sim
    # RIs[s, 2] <- Q_sim
    # RIs[s, 3] <- s - 1
    Similarities[s, 1:3] <- adjustedRand(P_old_vec, P_new_vec, randMethod = c("Rand", "HA", "Jaccard"))
    Similarities[s, 4:6] <- adjustedRand(Q_old_vec, Q_new_vec, randMethod = c("Rand", "HA", "Jaccard"))
    Similarities[s, 7] <- s - 1

    if((P_sim == 1) && (Q_sim == 1)) {
      result_list$data <- data
      result_list$P <- P
      result_list$Q <- Q
      result_list$InitialSSE <- InitialSSE
      result_list$SSE <- SSE
      result_list$Similarities <- data.frame(Similarities)
      result_list$iteration <- s
      result_list$A <- A

      result_list$SSE <- result_list$SSE[1:s,]
      result_list$Similarities <- result_list$Similarities[1:s,]

      class(result_list) <- c("biclustermd", "list")

      result_list
      break
    }


  }

  result_list$data <- data
  result_list$P <- P
  result_list$Q <- Q
  result_list$InitialSSE <- InitialSSE
  result_list$SSE <- SSE
  result_list$Similarities <- data.frame(Similarities)

  result_list$SSE <- result_list$SSE[1:s,]
  result_list$Similarities <- result_list$Similarities[1:s,]

  result_list$iteration <- s
  result_list$A <- A

  class(result_list) <- c("biclustermd", "list")

  result_list

}



