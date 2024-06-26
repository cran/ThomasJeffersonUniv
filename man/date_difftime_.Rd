% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_time.R
\name{date_difftime_}
\alias{date_difftime_}
\title{Concatenate a \link[base]{Date} and a \link[base]{difftime} Object}
\usage{
date_difftime_(date_, difftime_, tz = "UTC", tol = sqrt(.Machine$double.eps))
}
\arguments{
\item{date_}{an R object containing \link[base]{Date} information}

\item{difftime_}{a \link[base]{difftime} object}

\item{tz}{\link[base]{character} scalar, time zone,
see \link[base]{as.POSIXlt.Date} and \link[base]{ISOdatetime}}

\item{tol}{\link[base]{numeric} scalar, tolerance in finding second.
Default \code{sqrt(.Machine$double.eps)} as in \link[base]{all.equal.numeric}}
}
\value{
Function \link{date_difftime_} returns a \link[base]{POSIXct} object.
}
\description{
..
}
\note{
For now, I do not know how to force function \code{readxl::read_excel} to read a column
as \link[base]{POSIXt}.
By default, such column will be read as \link[base]{difftime}.

See \code{lubridate:::date.default} for the handling of year and month!
}
\examples{
(x = as.Date(c('2022-09-10', '2023-01-01', NA, '2022-12-31')))
y = as.difftime(c(47580.3, NA, 48060, 30660), units = 'secs')
units(y) = 'hours'
y
date_difftime_(x, y)
}
