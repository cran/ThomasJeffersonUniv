
#' @title checkCount
#' 
#' @description
#' ..
#' 
#' @param x \link[base]{logical} \link[base]{vector}
#' 
#' @returns
#' Function [checkCount] returns a \link[base]{character} scalar.
#' 
#' @export
checkCount <- function(x) {
  if (!is.logical(x)) stop('input needs to be `logical`')
  x0 <- x[!is.na(x)]
  if (!(n <- length(x0))) return('')
  y <- sum(x0)
  if (y == 0L) return('')
  return(sprintf(fmt = '%d/%d, %.1f%%', y, n, 1e2*y/n))
}