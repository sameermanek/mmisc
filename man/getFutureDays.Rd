% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates.R
\name{getFutureDays}
\alias{getFutureDays}
\title{Get a sequence of future days, inclusive}
\usage{
getFutureDays(chr.start.date, int.length)
}
\arguments{
\item{chr.start.date}{The start date (only one)}

\item{int.length}{The number of days}
}
\value{
A character vector of int.length, with the future series of days
}
\description{
Get a sequence of future days, inclusive
}
\examples{
getFutureDays('2017-04-01', 12)
}
