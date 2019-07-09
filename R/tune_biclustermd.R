#' Bicluster data over a grid of tuning parameters
#' 
#' @param data Dataset to bicluster. Must to be a data matrix with only numbers and missing values in the data set. It should have row names and column names.
#' @param nrep The number of times to repeat the biclustering for each set of parameters. Default 10.
#' @param parallel Logical indicating if the user would like to utilize the 
#'     \code{foreach} parallel backend. Default is FALSE.
#' @param ncores The number of cores to use if parallel computing. Default 2.
#' @param tune_grid A data frame of parameters to tune over. The column names of 
#'     this must match the arguments passed to \code{biclustermd()}. This must
#'     contain at least \code{col_clusters} and \code{row_clusters}, as there is
#'     no default in \code{biclustermd}.
#'     
#' @export
#' 
#' @importFrom stats sd
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange filter select row_number
#' @importFrom foreach %dopar% foreach
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster
#' 
#' @return A list of:
#'     \item{best_combn }{The best combination of parameters,}
#'     \item{best_bc }{The minimum SSE biclustering using the parameters in 
#'     \code{best_combn},} \item{grid }{\code{tune_grid} with columns giving the 
#'     minimum, mean, and standard deviation of the final SSE for each parameter 
#'     combination, and} \item{runtime }{CPU runtime & elapsed time.}
#'     
#' @examples
#' library(tidyverse)
#' data("synthetic")
#' tg <- expand.grid(
#' miss_val = fivenum(synthetic),
#' similarity = c("Rand", "HA", "Jaccard"),
#' col_min_num = 2,
#' row_min_num = 2,
#' col_clusters = 3:5,
#' row_clusters = 3
#' )
#' tg
#' tbc <- tune_biclustermd(synthetic, nrep = 10, tune_grid = tg)
#' tbc
#' 
#' tbc$grid %>% 
#'   group_by(miss_val, col_clusters) %>% 
#'   summarise(avg_sd = mean(sd_sse)) %>% 
#'   ggplot(aes(miss_val, avg_sd, color = col_clusters, group = col_clusters)) +
#'   geom_line() + 
#'   geom_point()
#' 
#' 
#' 
#' # in parallel: two cores:
#' tbc <- tune_biclustermd(synthetic, nrep = 20, parallel = TRUE, ncores = 2, tune_grid = tg)
#' tbc
#' 
#' boxplot(tbc$grid$mean_sse ~ tbc$grid$similarity)
#' boxplot(tbc$grid$sd_sse ~ tbc$grid$similarity)

tune_biclustermd <- function(data, nrep = 10, parallel = FALSE, ncores = 2, tune_grid = NULL) {
  
  if(is.null(tune_grid)) {
    stop("tune_grid is NULL")
  }
  
  defaults <- as.list(args(rep_biclustermd))
  defaults <- defaults[-length(defaults)]
  defaults$data <- data
  defaults$nrep <- nrep
  
  defaults$miss_val <- mean(data, na.rm = TRUE)
  
  nparams <- ncol(tune_grid)
  tune_params <- names(tune_grid)
  if("similarity" %in% tune_params) {
    tune_grid$similarity <- as.character(tune_grid$similarity)
  }
  
  if(!parallel) {
    
    tune_grid$min_sse <- NA
    tune_grid$mean_sse <- NA
    tune_grid$sd_sse <- NA
    
    st <- proc.time()
    grid_n <- nrow(tune_grid)
    best_sse <- .Machine$double.xmax
    for(i in 1:grid_n) {
      
      defaults[tune_params] <- tune_grid[i, tune_params]
      
      bc <- do.call(rep_biclustermd, defaults)
      tune_grid$min_sse[i] <- min(bc$rep_sse)
      tune_grid$mean_sse[i] <- mean(bc$rep_sse)
      tune_grid$sd_sse[i] <- sd(bc$rep_sse)
      
    }
    
    tune_grid <- tune_grid %>% 
      mutate(orig_order = row_number()) %>% 
      arrange(min_sse, sd_sse) %>% 
      mutate(best_combn = ifelse(row_number() == 1, '*         ', '')) %>% 
      arrange(orig_order) %>% 
      select(-orig_order)
    
    best_combn <- tune_grid %>% 
      arrange(min_sse, sd_sse) %>% 
      filter(row_number() == 1) %>% 
      select(1:nparams)
    
    defaults[names(best_combn)] <- best_combn[1,]
    
    best_bc <- do.call(rep_biclustermd, defaults)$best_bc
    et <- proc.time()
    
    list(
      best_combn = best_combn,
      best_bc = bc$best_bc,
      grid = tune_grid,
      runtime = et - st
    )
    
  } else if(parallel) {
    
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    
    st <- proc.time()
    grid_n <- nrow(tune_grid)
    results <- try(foreach(i = 1:grid_n, .packages = 'biclustermd') %dopar% {
      
      defaults[tune_params] <- tune_grid[i,]
      
      do.call(rep_biclustermd, defaults)
      
    })
    
    stopCluster(cl)
    
    if("try-error" %in% class(results)) {
      return(list(msg = 'foreach failed', results = results))
    }
    
    tune_grid$min_sse <- sapply(results, function(z) min(z$rep_sse))
    tune_grid$mean_sse <- sapply(results, function(z) mean(z$rep_sse))
    tune_grid$sd_sse <- sapply(results, function(z) sd(z$rep_sse))
    
    tune_grid <- tune_grid %>% 
      mutate(orig_order = row_number()) %>% 
      arrange(min_sse, sd_sse) %>% 
      mutate(best_combn = ifelse(row_number() == 1, '*         ', '')) %>% 
      arrange(orig_order) %>% 
      select(-orig_order)
    
    best_combn <- tune_grid %>% 
      arrange(min_sse, sd_sse) %>% 
      filter(row_number() == 1) %>% 
      select(1:nparams)
    
    grid_vars <- 1:nparams
    
    defaults[tune_params] <- best_combn[1, grid_vars]
    
    best_bc <- do.call(rep_biclustermd, defaults)$best_bc
    et <- proc.time()
    
    list(
      best_combn = best_combn,
      best_bc = best_bc,
      grid = tune_grid,
      runtime = et - st
    )
    
  }
  
}