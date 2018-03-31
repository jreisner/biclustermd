#' Make a data frame containing information about what cluster a row-column pair is in
#' 
#' @param data Data that was fed into the bicluster function.
#' @param bicluster_obj A bicluster object.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @importFrom tidyr gather
#' @return A data frame
make_train_set <- function(data, bicluster_obj) {
  data <- as.data.frame(data)
  
  bc <- bicluster_obj
  q <- bc$Q
  p <- bc$P
  
  rownames(q) <- rownames(data)
  rownames(p) <- colnames(data)
  
  rnames <- rownames(data)
  
  mod_data <- data[, -1]
  mod_data$row_name_var <- rnames
  
  mod_data <- mod_data[, c(ncol(mod_data), 1:(ncol(mod_data) - 1))]
  colnames(mod_data)[1] <- "row_name_var"
  
  train_set <- mod_data %>%
    gather(col_name_var, val, -row_name_var)
 
  train_set$row_proto <- unlist(lapply(1:nrow(train_set), function (x) {
    which(q[rownames(q) == train_set$row_name_var[x],] == 1)
  }
  ))
  
  train_set$col_proto <- unlist(lapply(1:nrow(train_set), function (x) {
    which(p[rownames(p) == train_set$col_name_var[x],] == 1)
  }
  ))
  
  train_set <- train_set %>%
    arrange(row_proto, col_proto)
  
  return(train_set)
}




