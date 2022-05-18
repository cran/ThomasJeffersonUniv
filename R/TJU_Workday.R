

#' @title Thomas Jefferson University Workdays
#' 
#' @description
#' To summarize the number of workdays, weekends, holidays and vacations in a given time-span 
#' (e.g., a month or a quarter of a year).
#' 
#' @param x \link[base]{character} scalar or vector (e.g.,
#' \code{'2021-01'} for January 2021,
#' \code{'2021 Q1'} for 2021 Q1 (January to March)), or
#' \link[base]{integer} scalar or vector (e.g., \code{2021L} for year 2021);
#' The time-span to be summarized.
#' Objects of classes \link[zoo]{yearqtr} and \link[zoo]{yearmon} are also accepted.
#' 
#' @param vacations \link[base:as.Date]{Date} vector, vacation days
#' 
#' @details 
#' 
#' \link{TJU_Workday} summarizes the workdays, weekends,
#' Jefferson paid holidays 
#' (New Year’s Day, Martin Luther King, Jr. Day, Memorial Day, Fourth of July, Labor Day, Thanksgiving and Christmas)
#' and your vacation (e.g., sick, personal, etc.) days (if any),
#' in a given time-span.
#' 
#' Per Jefferson policy (source needed), if a holiday is on Saturday, then the preceding Friday is considered Weekend.
#' If a holiday is on Sunday, then the following Monday is considered Weekend.
#' 
#' 
#' @return 
#' \link{TJU_Workday} returns a \link[base]{factor}.
#' 
#' @references 
#' \url{https://hr.jefferson.edu/benefits-compensation/paid-time-off.html}
#' 
#' @examples
#' table(TJU_Workday(c('2021-01', '2021-02')))
#' tryCatch(TJU_Workday(c('2019-10', '2019-12')), error = identity)
#' table(TJU_Workday('2022 Q1'))
#' table(TJU_Workday('2022 Q1', vacations = seq.Date(
#'  from = as.Date('2022-03-14'), to = as.Date('2022-03-18'), by = 1)))
#' table(TJU_Workday('2022 Q2', vacations = as.Date(c(
#'  '2022-05-22', '2022-05-30', '2022-06-01', '2022-07-04'))))
#' table(TJU_Workday(2021L))
#' 
#' @export
TJU_Workday <- function(x, vacations) {
  x_dt_orig <- allDates(x) # use S3
  if (!all(diff.default(x_dt_orig) == 1L)) stop('algorithm only handles weekday&holiday issue for consecutive time period')
  
  # add 1-day before and after, to deal with 'weekend & holiday' situation
  x_dt <- c(x_dt_orig[1L] - 1L, x_dt_orig, x_dt_orig[length(x_dt_orig)] + 1L) 
  nx <- length(x_dt)
  
  x_wkd <- format.Date(x_dt, format = '%a') # ?base::weekdays.Date
  id_holiday <- x_dt %in% as.Date.timeDate(holiday(year = unique.default(year(x_dt)), Holiday = c(
    'USNewYearsDay', 'USMLKingsBirthday', 'USMemorialDay', 'USIndependenceDay', 'USLaborDay', 'USThanksgivingDay', 'USChristmasDay')))
  id_weekend <- x_wkd %in% c('Sat', 'Sun')
  
  if (any(id_he <- (id_holiday & id_weekend))) { # {h}oliday on week{e}nd; Jefferson makes the closest weekday as weekend
    if (length(wch_Sat <- setdiff(which(id_holiday & (x_wkd == 'Sat')), y = 1L))) {
      id_weekend[wch_Sat] <- FALSE # original Saturday no longer considered as weekend; consider as holiday
      id_weekend[wch_Sat - 1L] <- TRUE # previous (auxiliary) Friday considered as weekend
      # dont care when first day is Sunday and before-auxilary Saturday is holicay or not
      # Takes care when last day is Friday and after-auxiliary Saturday is a holiday
    }
    if (length(wch_Sun <- setdiff(which(id_holiday & (x_wkd == 'Sun')), y = nx))) {
      id_weekend[wch_Sun] <- FALSE # original Sunday no longer considered as weekend; consider as holiday
      id_weekend[wch_Sun + 1L] <- TRUE # next (auxiliary) Monday considered as weekend
      # dont care when last day is Saturday and after-auxiliary Sunday is holiday or not
      # Takes care when first day is Monday and before-auxiliary Sunday is a holiday
    }
  }
  
  if (any(id_holiday & id_weekend)) stop('should have been removed')
  
  out <- rep(1L, times = nx) # default: weekday
  out[id_weekend] <- 2L # weekend
  out[id_holiday] <- 3L # holiday
  
  if (!missing(vacations)) {
    if (!inherits(vacations, what = 'Date')) stop('`vacations` must be Date object')
    if (any(id_holiday_vacation <- vacations %in% x_dt[id_holiday])) {
      message('Vacation day(s) ', sQuote(vacations[id_holiday_vacation]), ' are holiday(s).')
      vacations <- vacations[!id_holiday_vacation]
    }
    if (any(id_weekend_vacation <- vacations %in% x_dt[id_weekend])) {
      message('Vacation day(s) ', sQuote(vacations[id_weekend_vacation]), ' are weekend(s).')
      vacations <- vacations[!id_weekend_vacation]
    }
    if (any(id_out_vacation <- !(vacations %in% x_dt[-c(1L, nx)]))) {
      message('Vacation day(s) ', sQuote(vacations[id_out_vacation]), ' are out of the timespan.')
      vacations <- vacations[!id_out_vacation]
    }
    out[x_dt %in% vacations] <- 4L # vacation
  } # else do nothing
  
  ret <- structure(out[-c(1L, nx)], # remove auxiliary day-begin and day-end
            class = 'factor',
            levels = c('Workday', 'Weekend', 'Holiday', 'Vacation'))
  factor(ret) # remove zero-count
}




# \link{allDates} returns all \link[base:as.Date]{Date} in a given time span.

# Objects \link[zoo:as.yearqtr]{yearqtr} and \link[zoo:as.yearmon]{yearmon} are type-double.

# dont forget
# base::month.abb
# base::month.name

allDates <- function(x) {
  if (!length(x)) return(invisible())
  if (inherits(x, what = 'Date')) return(x)
  if (anyNA(x)) stop('does not allow NA input')
  x <- unique(x) # ?base::unique.default ?zoo:::unique.yearmon ?zoo:::unique.yearqtr
  UseMethod('allDates')
}


allDates.integer <- function(x) { # `x` considered as year!
  do.call(c, args = lapply(x, FUN = \(i) {
    i1 <- as.Date.character(paste0(i, c('-01-01', '-12-31')), format = '%Y-%m-%d')
    seq.Date(from = i1[1L], to = i1[2L], by = 1L)
  }))
}


allDates.character <- function(x) {
  if (!anyNA(x0 <- as.yearmon(x))) return(allDates.yearmon(x0)) # ?zoo:::as.yearmon.character, exception is NA (not error)
  if (!anyNA(x0 <- as.yearqtr(x))) return(allDates.yearqtr(x0)) # ?zoo:::as.yearqtr.character
  stop('Cannot be converted to Date: ', sQuote(x))
}


allDates.yearmon <- function(x) {
  do.call(c, args = lapply(x, FUN = \(i) {
    seq.Date(from = as.Date.yearmon(i), to = as.Date.yearmon(i + 1/12) - 1L, by = 1L)
  }))
}


allDates.yearqtr <- function(x) {
  do.call(c, args = lapply(x, FUN = \(i) {
    seq.Date(from = as.Date.yearqtr(i), to = as.Date.yearqtr(i + 1/4) - 1L, by = 1L)
  }))
}






