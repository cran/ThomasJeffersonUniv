

#' @title checkDuplicated
#' 
#' @description
#' ..
#' 
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param f \link[stats]{formula}
#' 
#' @param file_duplicated \link[base]{character} scalar
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [checkDuplicated] returns a \link[base]{data.frame}.
#' 
#' @examples
#' (d1 = data.frame(A = c(1, 1), B = c(NA_character_, 'text')))
#' 
#' 
#' (d2 = data.frame(A = c(1, 2), B = c(NA_character_, 'text')))
#' 
#' @importFrom stats model.frame.default
#' @export
checkDuplicated <- function(
    data, f,
    file_duplicated = tempfile(fileext = '.txt'),
    ...
) {
  
  dup_txt <- sQuote(deparse1(f))
  
  d0 <- model.frame.default(formula = f, data = data)
  f0 <- do.call(what = interaction, args = as.list.data.frame(d0))
  if (nlevels(f0) == length(f0)) {
    message(sprintf(fmt = '\u2714 No duplicated %s\n', dup_txt))
    return(invisible(data))
  }
  
  #ds <- split.data.frame(x = data, f = f) # too slow with big `data`
  f_ <- .formula2varlist(formula = f, data = data)
  rid <- split(x = seq_len(nrow(data)), f = f_) # split row-indices
  rid_n <- lengths(rid, use.names = FALSE)
  
  # rows of `data` without duplication
  r0 <- sort.int(unlist(rid[rid_n == 1L], use.names = FALSE))
  
  rid_dup <- rid[rid_n > 1L]
  nm_dup <- format(sQuote(names(rid_dup)), justify = 'left')
  ds_dup <- mapply(FUN = function(i, nm) {
    message('\rCreating subset ', nm, appendLF = FALSE)
    data[i, , drop = FALSE]
  }, i = rid_dup, nm = sprintf(fmt = '%s - %d of %d', nm_dup, seq_along(nm_dup), length(nm_dup)), SIMPLIFY = FALSE)
  
  ds_coalesce <- lapply(ds_dup, FUN = function(d) {
    # can `d` be colume-wise coalesced ?
    tryCatch(expr = as.data.frame.list(
      x = lapply(d, FUN = unique_), 
      check.names = FALSE
    ), error = identity)
  })
  
  id_truedup <- vapply(ds_coalesce, FUN = inherits, what = 'error', FUN.VALUE = NA)
  
  if (any(id_truedup)) {
    
    rid_truedup <- rid_dup[id_truedup]
    nm_truedup <- nm_dup[id_truedup]
    ds_truedup <- ds_dup[id_truedup]
    tmp <- mapply(FUN = function(d, nm) {
      message('\rFinding duplicated columns ', nm, appendLF = FALSE)
      not_unique_(d)
    }, d = ds_truedup, nm = sprintf(fmt = '%s - %d of %d', nm_truedup, seq_along(nm_truedup), length(nm_truedup)), SIMPLIFY = FALSE)
    sink(file = file_duplicated)
    print(tmp)
    sink()
    message(sprintf(fmt = '\r\u261e %s: %d %s with substantial duplicates', sQuote(basename(file_duplicated)), length(rid_truedup), dup_txt))
    system(paste0('open ', dirname(file_duplicated)))
    
    # quite slow!!
    d_dup_row1 <- do.call(rbind.data.frame, args = c(lapply(ds_truedup, FUN = function(x) x[1L, , drop = FALSE]), list(make.row.names = FALSE)))
    
  } else d_dup_row1 <- NULL
  
  ret <- rbind.data.frame(
    data[r0, , drop = FALSE],
    if (any(!id_truedup)) do.call(what = rbind.data.frame, args = c(ds_coalesce[!id_truedup], list(make.row.names = FALSE))),
    d_dup_row1
  )
  message(sprintf('\u21ac %d rows: duplicated (substantial or trivial) %s removed\n', nrow(ret), dup_txt))
  return(ret)
  
}



unique_ <- function(x) {
  x0 <- x[!is.na(x)]
  if (!length(x0)) return(NA)
  # not using my [unique_allequal]
  if (length(x1 <- unique(x0)) != 1L) stop('non-unique entries!')
  return(x1)
}



not_unique_ <- function(data) {
  unique_id <- vapply(data, FUN = function(x) {
    #inherits(tryCatch(unique_(x), error = identity), what = 'error')
    # base::tryCatch too slow
    x0 <- x[!is.na(x)]
    if (!length(x0)) return(TRUE)
    x1 <- unique(x0) # not using my [unique_allequal]
    return(length(x1) == 1L)
  }, FUN.VALUE = NA)
  data[!unique_id]
}


