
cat_named <- function(X, sep = ': ') {
  # do not want to include my [trimws2]
  X0 <- trimws(vapply(X, FUN = paste, collapse = ' ', FUN.VALUE = ''))
  X1 <- X0[nzchar(X0)]
  if (!length(nm <- names(X1))) stop('must be named')
  if (!all(nzchar(nm))) stop('do not allow empty name!')
  X2 <- strsplit(X1, split = '\n')
  if (!all((nx <- lengths(X2)) == 1L)) { # some element(s) contains '\n'
    X1 <- unlist(X2, use.names = FALSE)
    tmp <- character(length = length(X1))
    tmp[cumsum(nx)] <- nm
    nm <- tmp # now allow zchar in `nm`
  }
  .mapply(FUN = cat, dots = list(
    #.bold(.violet(format(nm, width = max(nchar(nm)) + 1L, justify = 'right'))), # ?stats:::print.power.htest
    format(nm, width = max(nchar(nm)) + 1L, justify = 'right'), # ?stats:::print.power.htest
    sep, 
    X1, # .cyan(X1), 
    '\n'
  ), MoreArgs = list(sep = ''))
  cat('\n')
  return(invisible())
}


