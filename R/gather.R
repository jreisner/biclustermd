#' Gather a biclustermd object
#' 
#' @param x a \code{biclustermd} object to gather.
#' 
#' @return A data frame containing the row names and column names of both the 
#'   two-way table of data biclustered and the cell-average matrix.
#' 
#' @importFrom dplyr arrange select
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' 
#' @export
#' 
gather.biclustermd <- function (x) {
  data <- as.data.frame(x$data)
  bc <- x[-1]
  q <- bc$Q
  p <- bc$P
  rownames(q) <- rownames(data)
  rownames(p) <- colnames(data)
  rnames <- rownames(data)
  
  data$row_name <- rnames
  data <- data[, c(ncol(data), 1:(ncol(data) - 1))]
  colnames(data)[1] <- "row_name"
  
  data <- data %>% 
    gather(col_name, val, -row_name)
  data$row_group <- unlist(
    lapply(1:nrow(data), function(n) {
      which(q[rownames(q) == data$row_name[n], ] == 1)
    })
  )
  data$col_group <- unlist(
    lapply(1:nrow(data), function(n) {
      which(p[rownames(p) == data$col_name[n], ] == 1)
    })
  )
  data <- data %>% 
    arrange(row_group, col_group) %>%
    select(row_name, col_name, row_group, col_group, val)
  data
}