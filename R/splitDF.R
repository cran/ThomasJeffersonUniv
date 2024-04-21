


#' @title Split \link[base]{data.frame} by Row
#' 
#' @description
#' \link[base]{split.data.frame} into individual rows.
#' 
#' @param x \link[base]{data.frame}
#' 
#' @note
#' We use \link[base]{split.data.frame} with argument `f` being `attr(x, which = 'row.names', exact = TRUE)` instead of
#' `seq_len(.row_names_info(x, type = 2L))`,
#' not only because the former is faster, but also \link[base]{.rowNamesDF<-} enforces 
#' that \link[base]{row.names.data.frame} must be unique.
#' 
#' @returns
#' Function [splitDF] returns a \link[base]{list} of \link[base]{nrow}-1 \link[base]{data.frame}s.
#' 
#' @examples
#' splitDF(head(mtcars)) # data.frame with rownames
#' splitDF(head(warpbreaks)) # data.frame without rownames
#' splitDF(data.frame()) # exception
#' @export
splitDF <- function(x) {
  split.data.frame(x, f = attr(x, which = 'row.names', exact = TRUE))
}






#' @title Match Rows of \link[base]{data.frame}s
#' 
#' @description
#' To \link[base]{match} the rows of one \link[base]{data.frame}
#' to the rows of another \link[base]{data.frame}.
#' 
#' @param x \link[base]{data.frame}, the rows of which to be matched.
#' 
#' @param table \link[base]{data.frame}, the rows of which to be matched *against*.
#' 
#' @param by \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param by.x,by.table \link[base]{character} scalar or \link[base]{vector}
#' 
#' @returns 
#' Function [matchDF] returns a \link[base]{integer} \link[base]{vector}
#' 
#' 
#' @examples
#' DF <- swiss[sample(nrow(swiss), size = 100, replace = TRUE), ]
#' matchDF(DF)
#' 
#' @export
matchDF <- function(
    x, table = unique.data.frame(x),
    by = names(x), by.x = character(), by.table = character()
) {
  
  if (!is.data.frame(x)) stop('`x` must be data.frame')
  
  tab <- table; table <- NULL # dont want to confuse with ?base::table
  if (!is.data.frame(tab)) stop('`table` must be data.frame')
  
  nm.x <- names(x)
  nm.tab <- names(tab)
  
  by.x <- unique.default(c(by, by.x))
  by.tab <- unique.default(c(by, by.table))
  if (any(id <- is.na(match(by.x, table = nm.x)))) stop('Colnames ', paste(sQuote(by.x[id]), collapse = ','), ' absent from `x`')
  if (any(id <- is.na(match(by.tab, table = nm.tab)))) stop('Colnames ', paste(sQuote(by.tab[id]), collapse = ','), ' absent from `table`')
  
  nby <- length(by.x)
  if (nby != length(by.tab)) stop('`by.x` and `by.table` must be same length')
  
  nm_x <- setdiff(nm.x, by.x)
  nm_table <- setdiff(nm.tab, by.tab)
  if (length(nm_ <- intersect(nm_x, nm_table))) stop('do not allow same colnames ', paste(sQuote(nm_), collapse = ','), ' in `x` and `table` (except for `by`)')
  
  x0 <- x[by.x]; .rowNamesDF(x0) <- NULL
  tab0 <- tab[by.tab]; names(tab0) <- by.x; .rowNamesDF(tab0) <- NULL
  # otherwise, if `!identical(by.x, by.tab)`, ?base::match wont work
  if (anyDuplicated.data.frame(tab0)) stop('do not allow duplicated ', sQuote(paste0(by.tab, collapse = '+')), ' in `table`')
  
  id <- match(x = splitDF(x0), table = splitDF(tab0), nomatch = NA_integer_)
  
  if (any(na1 <- is.na(id))) {
    x_ <- x0[na1, , drop = FALSE]
    x_uid <- !duplicated.data.frame(x_)
    x_u <- x_[x_uid, , drop = FALSE]
    
    message('\u2756 ', sum(na1), ' (', sum(x_uid), ' unique) rows (', paste(by.x, collapse = ','), 
            ') has no match (', paste(by.tab, collapse = ','), ')')
    
    for (i in rev.default(seq_len(nby - 1L))) { # (i = nby - 1L)
      iseq <- seq_len(i)
      idx <- match(x = splitDF(unique.data.frame(x_u[iseq])), 
                   table = splitDF(unique.data.frame(tab0[iseq])), 
                   nomatch = NA_integer_)
      idok <- !is.na(idx)
      message('\u2756 Matched ', sum(idok), '/', length(idx), ' by ', 
              sQuote(paste(by.x[iseq], collapse = ',')), ' and ',
              sQuote(paste(by.tab[iseq], collapse = ',')))
      if (all(idok)) break
    }
  }
  
  attr(id, which = 'by.x') <- by.x
  attr(id, which = 'by.table') <- by.tab
  return(id)
  
}











#' @title An Alternative Merge Operation
#' 
#' @description
#' ..
#' 
#' @param e1 \link[base]{data.frame}, on which new columns will be added.
#' All rows of `e1` will be retained in the returned object, *in their original order*.
#' 
#' @param e2 \link[base]{data.frame}, columns of which will be added to `e1`.
#' Not all rows of `e2` will be included in the returned object
#' 
#' @param by \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param by1,by2 \link[base]{character} scalar or \link[base]{vector}
#' 
#' @note
#' We avoid \link[base]{merge.data.frame} as much as possible,
#' because it's slow and 
#' even `sort = FALSE` may not completely retain the original order of input `x`.
#' 
#' @returns 
#' Function [mergeDF] returns a \link[base]{data.frame}.
#' 
#' @examples
#' # examples inspired by ?merge.data.frame 
#' 
#' (authors = data.frame(
#'  surname = c('Tukey', 'Venables', 'Tierney', 'Ripley', 'McNeil'),
#'  nationality = c('US', 'Australia', 'US', 'UK', 'Australia'),
#'  deceased = c('yes', rep('no', 4))))
#' (books = data.frame(
#'  name = c('Tukey', 'Venables', 'Tierney', 'Ripley', 
#'   'Ripley', 'McNeil', 'R Core', 'Diggle'),
#'  title = c(
#'   'Exploratory Data Analysis',
#'   'Modern Applied Statistics',
#'   'LISP-STAT', 'Spatial Statistics', 'Stochastic Simulation',
#'   'Interactive Data Analysis', 'An Introduction to R',
#'   'Analysis of Longitudinal Data'),
#'  other.author = c(
#'   NA, 'Ripley', NA, NA, NA, NA, 'Venables & Smith',
#'   'Heagerty & Liang & Scott Zeger')))
#' 
#' (m = mergeDF(books, authors, by1 = 'name', by2 = 'surname'))
#' attr(m, 'nomatch')
#' 
#' @export
mergeDF <- function(
    e1, e2, 
    by = character(), by1 = character(), by2 = character()
) {
  
  id <- matchDF(x = e1, table = e2, by = by, by.x = by1, by.table = by2)
  
  data_nomatch <- if (anyNA(id)) {
    tmp <- e1[is.na(id), , drop = FALSE]
    by1 <- attr(id, which = 'by.x', exact = TRUE)
    tmp_uid <- !duplicated.data.frame(tmp[by1])
    tmp_u <- tmp[tmp_uid, , drop = FALSE]
    tmp_u[do.call(order, args = as.list.data.frame(tmp_u[by1])), , drop = FALSE]
  } # else NULL
  
  by2 <- attr(id, which = 'by.table', exact = TRUE)
  ret <- data.frame(e1, e2[id, setdiff(names(e2), by2), drop = FALSE])
  rownames(ret) <- rownames(e1) # otherwise be overriden by rownames(e2[...])
  attr(ret, which = 'nomatch') <- data_nomatch
  return(ret)
  
}