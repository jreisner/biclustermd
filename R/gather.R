#' Gather a biclustermd object
#' 
#' @param data a \code{biclustermd} object to gather.
#' @param key unused; included for consistency with \code{tidyr} generic
#' @param value unused; included for consistency with \code{tidyr} generic
#' @param ... unused; included for consistency with \code{tidyr} generic
#' @param na.rm unused; included for consistency with \code{tidyr} generic
#' @param convert unused; included for consistency with \code{tidyr} generic
#' @param factor_key unused; included for consistency with \code{tidyr} generic
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
gather.biclustermd <- function(data, key = NULL, value = NULL, ..., na.rm = FALSE,
                               convert = FALSE, factor_key = FALSE) {
  dat <- as.data.frame(data$data)
  bc <- data[-1]
  q <- bc$Q
  p <- bc$P
  rownames(q) <- rownames(dat)
  rownames(p) <- colnames(dat)
  rnames <- rownames(dat)
  
  dat$row_name <- rnames
  dat <- dat[, c(ncol(dat), 1:(ncol(dat) - 1))]
  colnames(dat)[1] <- "row_name"
  
  dat <- dat %>% 
    gather(col_name, val, -row_name)
  dat$row_group <- unlist(
    lapply(1:nrow(dat), function(n) {
      which(q[rownames(q) == dat$row_name[n], ] == 1)
    })
  )
  dat$col_group <- unlist(
    lapply(1:nrow(dat), function(n) {
      which(p[rownames(p) == dat$col_name[n], ] == 1)
    })
  )
  dat <- dat %>% 
    arrange(row_group, col_group) %>%
    select(row_name, col_name, row_group, col_group, val)
  dat
}