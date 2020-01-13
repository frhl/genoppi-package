#' @title misc tools
#' @rdname misc
#' @export

#' @description length of unique items.
#' @param x a vector or list of items.
#' @export
lun <- function(x) length(unique(as.vector(x)))

#' @title Not in
#' @description returns true for x not in y
#' @param x value x
#' @param y value y
#' @export
'%nin%' <- function(x, y) !(x %in% y)

#' @title Not in
#' @description returns true for x not in y
#' @param x value x
#' @param y value y
#' @export
'%!in%' <- function(x,y) return(!('%in%'(x,y)))

#' @title omit nulls from list
#' @description remove NULLs in list
#' @export
null.omit <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' @title warnings to stderr
#' @description sends a message to stderr
#' @param msg the message.
#' @param file write logs to file as well as stderr.
#' @export
warn <- function(msg, file = NULL){
  write(msg,stderr())
  if (!is.null(file)) write(msg, file)
}


