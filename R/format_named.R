

#' @title format_named
#' 
#' @param x \link[base]{character} \link[base]{vector}, 
#' or a \link[base]{list} of \link[base]{character} object.
#' Input `x` must be named
#' 
#' @param sep \link[base]{character} scalar, see \link[base]{paste}
#' 
#' @param colored \link[base]{logical} scalar, whether use two different color
#' to separate each element, default `TRUE`
#' 
#' @returns
#' Function [format_named] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' x1 = c(a = 1, bc = '2\n3')
#' cat(format_named(x1), sep = '\n')
#' 
#' x2 = list(a = '1\n2', b = character(), cd = '3\n4', efg = '5\n6\n7')
#' cat(format_named(x2, colored = FALSE), sep = '\n')
#' cat(format_named(x2), sep = '\n')
#' 
#' x3 = c(a = '1\n2')
#' cat(format_named(x3), sep = '\n')
#' 
#' @keywords internal
#' @export
format_named <- function(x, sep = ': ', colored = TRUE) {
  
  x0 <- trimws(vapply(x, FUN = paste, collapse = ' ', FUN.VALUE = ''))
  x1 <- x0[nzchar(x0)]
  if (!length(nm <- names(x1))) stop('input must be named')
  if (!all(nzchar(nm))) stop('do not allow empty name!')
  
  x2 <- strsplit(x1, split = '\n')
  if (all((nx <- lengths(x2)) == 1L)) { 
    colour_times <- rep(1, times = length(x1))
  } else { # some element(s) contains '\n'
    x1 <- unlist(x2, use.names = FALSE)
    tmp <- character(length = length(x1))
    c_nx <- cumsum(nx)
    tmp[c_nx] <- nm
    nm <- tmp # now allow zchar in `nm`
    colour_times <- c(c_nx[1L], diff(c_nx))
  } 
  
  ret <- paste(format.default(nm, justify = 'right'), x1, sep = sep)
  if (!colored) return(ret)
  
  colour_head <- if (length(colour_times) == 1L) {
    rep('\033[32m', times = colour_times)
  } else suppressWarnings(mapply(rep, c('\033[32m', '\033[36m'), times = colour_times))
  # base::suppressWarnings on length not integer times haha
  
  return(paste0(
    unlist(colour_head, use.names = FALSE),
    ret,
    rep('\033[0m', times = length(ret))
  ))
  
}


