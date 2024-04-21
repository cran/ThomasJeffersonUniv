


#' @title Award & Effort from Cayuse 
#' 
#' @description
#' Print out grant and effort from Cayuse.
#' 
#' @param path \link[base]{character} scalar, directory of downloaded award `.csv` file.
#' Default is the download directory `'~/Downloads'`
#' 
#' @param fiscal.year \link[base]{integer} scalar
#' 
#' @returns ..
#' 
#' @details 
#' \itemize{
#' \item {go to `https://jefferson.cayuse424.com/sp/index.cfm`}
#' \item {My Proposals -> Submitted Proposals. 
#'   Lower-right corner of screen, 'Export to CSV'.
#'   Downloaded file has name pattern `'^proposals_.*\\.csv'`}
#' \item {My Awards -> Awards (*not* 'Active Projects').
#'   Lower-right corner of screen, 'View All', then 'Export to CSV'.
#'   Downloaded file has name pattern `'^Awards_.*\\.csv'`}
#' \item {My Awards -> Awards.  Click into each project, under 'People' tab to find my 
#'   'Sponsored Effort'}
#' }
#' 
#' Function [aggregateAwards()] aggregates grant over different period 
#' (e.g. from Axx-xx-001, Axx-xx-002, Axx-xx-003 to Axx-xx).
#' Then we need to manually added in our 'Sponsored Effort' in the returned `.csv` file.
#' 
#' @examples 
#' if (FALSE) {
#' aggregateAwards()
#' viewAward()
#' viewProposal()
#' award2LaTeX()
#' }
#' 
#' @name TJU_Cayuse
#' @importFrom lubridate year
#' @importFrom utils read.csv write.table
#' @export
aggregateAwards <- function(
    path = '~/Downloads', 
    fiscal.year = year(Sys.Date())
) {
  
  awards_csv <- list.files(path = path, pattern = '^Awards_.*\\.csv$', full.names = TRUE)
  nA <- length(awards_csv)
  if (!nA) stop('Awards file not available')
  if (nA > 1L) stop('Multiple awards .csv files available')
  
  dim(awards <- read.csv(awards_csv))
  # subset(awards, nzchar(Flags)) # manually inspect
  awards <- within.data.frame(awards, expr = {
    Admin.Unit <- Account.Numbers <- NULL
    Status <- Flags <- NULL # hard to consolidate with a series of extensions
    Lead.PI <- gsub(' AOI$', replacement = '', x = Lead.PI)
    Award.Amount <- as.double(gsub('^\\$|,', replacement = '', x = Award.Amount))
    Award.No. <- vapply(strsplit(Award.No., split = '-'), FUN = function(i) paste(i[1:2], collapse = '-'), FUN.VALUE = '')
    Award.Notice.Received <- as.Date.character(Award.Notice.Received, format = '%m/%d/%Y')
    Award.Begin.Date <- as.Date.character(Award.Begin.Date, format = '%m/%d/%Y')
    Award.End.Date <- as.Date.character(Award.End.Date, format = '%m/%d/%Y')
  })
  
  length(ys <- split.data.frame(awards, f = ~ Award.No. + Sponsor))
  length(ys <- ys[vapply(ys, FUN = .row_names_info, type = 2L, FUN.VALUE = 0L) > 0L])
  #length(split.data.frame(Awards1, f = ~ Award.No. + Sponsor, envir = Awards1))
  
  y1 <- do.call(rbind.data.frame, args = c(lapply(ys, FUN = function(y) { # (y = ys[[1L]])
    if (!all(duplicated(y$Project.Title)[-1L])) stop('`Project.Title` not same')
    first_begin <- min(y$Award.Begin.Date, na.rm = TRUE)
    last_end <- max(y$Award.End.Date, na.rm = TRUE)
    Award.Period <- with(y, expr = paste0(
      Award.Begin.Date, ' ~ ', Award.End.Date, ' (', 
      format.difftime(asDifftime(Award.End.Date - Award.Begin.Date, units = 'years'), digits = 1L), ', ',
      '$', formatC(y$Award.Amount, big.mark = ',', format = 'f', digits = 2L),
      ')', collapse = '\n'
    ))
    data.frame(
      Award.No. = y$Award.No.[1L], 
      Project.Title = trimws(y$Project.Title[1L]), 
      Lead.PI = paste(unique(y$Lead.PI), collapse = ', '), 
      Sponsor = y$Sponsor[1L],
      Award.Amount = paste0('$', formatC(sum(y$Award.Amount, na.rm = TRUE), big.mark = ',', format = 'f', digits = 2L)),
      #Award.Notice.Received = min(Award.Notice.Received, na.rm = TRUE),
      TimePeriod = paste(first_begin, '~', last_end),
      Award.End.Date = last_end,
      Award.Period = Award.Period
    )
  }), list(make.row.names = FALSE)))
  
  y2 <- within(y1, expr = {
    Status <- .bincode(as.double(Award.End.Date), breaks = c(-Inf, as.double(TJU_Fiscal_Year(fiscal.year)), Inf), right = TRUE, include.lowest = TRUE)
    Status <- structure(Status, levels = c(sprintf('Ends before FY%d', fiscal.year), sprintf('Ends in FY%d', fiscal.year), 'Ongoing'), class = 'factor')
    Award.End.Date <- NULL
  })
  
  y3 <- subset(y2, !is.na(Status))
  y3$Effort <- '' # to be manually filled in

  y4 <- y3[order(y3$Award.No.), ]
  aggAwards_csv <- file.path(path, 'aggregatedAwards.csv')
  write.table(y4, file = aggAwards_csv, sep = ',', row.names = FALSE)
  system(paste('open', aggAwards_csv))
  
  cat('Fill in `Effort` by clicking into each project under \'Active Projects\'\n')
  return(invisible(y4))
  
}




#' @rdname TJU_Cayuse
#' @export
viewProposal <- function(path = '~/Downloads', fiscal.year = year(Sys.Date())) {
  proposal_csv <- list.files(path = path, pattern = '^proposals_.*\\.csv$', full.names = TRUE)
  np <- length(proposal_csv)
  if (!np) stop('Proposal file not available')
  if (np > 1L) stop('Multiple proposal files available')
  
  dim(proposal0 <- read.csv(file = proposal_csv))
  
  proposal1 <- within.data.frame(data = proposal0, expr = {
    Prop.No <- trimws(Prop.No)
    Lead.PI <- gsub(' AOI$', replacement = '', x = Lead.PI)
    Submitted.Date <- as.Date.character(Submitted.Date, format = '%m/%d/%Y')
    Submitted_FY <- .bincode(Submitted.Date, breaks = TJU_Fiscal_Year(fiscal.year)) # NA, 1, NA
    Submitted_Term <- TJU_SchoolTerm(Submitted.Date)
  })
  
  # status_rm <- c('Not Funded', 'Funded', 'Abandoned', 'Withdrawn', 'TJU Signing Official')
  status_rm <- c('Funded', 'Abandoned', 'Withdrawn', 'TJU Signing Official')
  dim(proposal <- eval(quote(subset(x = proposal1, subset = !is.na(Submitted_FY) & !(Status %in% status_rm)))))
  
  n <- .row_names_info(proposal, type = 2L)
  
  if (FALSE) { #manually inspect
    freqs(proposal$Submitted.Date)
    freqs(proposal$Status)
    length(unique(proposal$Lead.PI))
  }
  
  #################################
  # copy to Interfolio
  
  .mapply(FUN = function(x, nm) {
    cat(c('Proposal', sQuote(nm)), '\n')
    cat_named(x)
  }, dots = list(
    x = split.data.frame(proposal[c('Submitted_Term', 'Project.Name', 'Sponsor', 'Prop.No', 'My.Role', 'Lead.PI', 'Status')], f = seq_len(n)), 
    nm = seq_len(n)
  ), MoreArgs = NULL)
  
  return(invisible(proposal))
  
}




#' @rdname TJU_Cayuse
#' @export
viewAward <- function(path = '~/Downloads') {
  
  awards <- read.csv(file = file.path(path, 'aggregatedAwards.csv'))
  n <- .row_names_info(awards, type = 2L)
  #awards <- within(awards0, expr = {
  #  Award.No. <- paste0(Award.No., '; ', Lead.PI)
  #  Lead.PI <- Award.Amount <- TimePeriod <- NULL
  #})
  
  #################################
  # copy to Interfolio
  
  .mapply(FUN = function(x, nm) {
    message(c('Award ', sQuote(nm)))
    cat_named(x)
  }, dots = list(
    x = split.data.frame(awards[c(
      'Award.No.', 'Project.Title', 'Lead.PI', 'Sponsor', 'Award.Period', 'Status', 'Effort'
    )], f = seq_len(n)), 
    nm = seq_len(n)
  ), MoreArgs = NULL)
  
  return(invisible())
  
}



#' @rdname TJU_Cayuse
#' @export
award2LaTeX <- function(path = '~/Downloads') {
  awards <- read.csv(file.path(path, 'aggregatedAwards.csv'))
  NoOut = lapply(seq_len(.row_names_info(awards, 2L)), FUN = function(i) { 
    # copy to LaTeX resume
    tmp <- awards[i, ]
    cat('\\textmd{', tmp$Role, '.}\n', sep = '')
    cat('\\textit{', gsub('\\&', '\\\\&', tmp$Project.Title), '}.\n', sep = '')
    if (!is.na(tmp$SponsorAward)) cat(gsub('\\&', '\\\\&', tmp$Sponsor), ', ', tmp$SponsorAward, '\n', sep = '') else cat(gsub('\\&', '\\\\&', tmp$Sponsor), '.\n', sep = '')
    cat('awarded to ', tmp$Lead.PI, ', ', gsub('\\~', '-', tmp$TimePeriod), ', \\', tmp$Award.Amount, '\n', sep = '')
    cat('\n\n')
  })
  return(invisible())
}




if (FALSE) {
  # only inspect
  dim(proposalFunded <- subset(pr1, Status == 'Funded')) # to be compared to `awards`
  # `proposalFunded` is not reliable
  proposalFunded[c('Lead.PI', 'Project.Name')]
  subset(awards, OnGoing, select = c('Lead.PI', 'Project.Title'))
}







