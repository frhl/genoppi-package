#' @title Normalize data
#' @description Normalize a table of label or label free data. 
#' The function will calculate the \code{type} e.g. median of
#' all numeric columns and subtract the median from the 
#' intensity value.
#' @param table a data.frame/table.
#' @param type what type of normalization? Can be any function
#' that processes numeric vectors. Default is median.
#' @author Frederik
#' @family statistics
#' @export

normalize <- function(table = NULL, type = 'median'){
  
  ## normalize numeric columns only
  columns_numeric <- sapply(table, is.numeric)
  table_numeric <- table[, columns_numeric]
  values_type <- as.data.frame(t(sapply(table_numeric, function(x) do.call(type, list(na.omit(x))))))
  values_type <- values_type[rep(seq_len(nrow(values_type)), each = nrow(table)), ]
  table_normalized <- table_numeric - values_type
  table[, columns_numeric] <- table_normalized
  return(table)
  
}












