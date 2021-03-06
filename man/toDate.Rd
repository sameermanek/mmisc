% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates.R
\name{toDate}
\alias{toDate}
\title{Convert strings to date}
\usage{
toDate(chr.date.raw, lst.date.formats = NULL, log.stop.on.invalid = TRUE)
}
\arguments{
\item{chr.date.raw}{The input date vector, as characters}

\item{lst.date.formats}{Date formats, in the form of a list, with associated validation. For example,
\code{
lst.date.formats = list(c(validation = '^[0-9]{2}[a-zA-Z]{3}20[0-9]{2}$', format = '\%d\%b\%Y'))
}. 
You can include as many formats as you'd like within the lst.date.formats}

\item{log.stop.on.invalid}{Should this function stop if any (non-NA) element of \code{chr.date.raw} 
doesn't validate across any of the validation criteria?}
}
\value{
The character vector (same ordering) in format YYYY-MM-DD
}
\description{
\code{toDate} converts a raw date format (e.g., YY-MM-DD, MM-DD-YYYY, etc) into a normalized
date format (YYYY-MM-DD) as a character vector. This is less clumsy than casting using 
\code{as.character(as.Date(..., format = ....))} assuming your goal is to create the 
normalized character output. While not intuitive, it will also perform basic validation
and is much, much quicker (through hashing, allowing multiple formats, and short-circuiting) 
than alternatives (inat least for my use case where I have many values, many of which are repeated)
}
\examples{
toDate(c('03/15/2013','03MAR2015','2015-03-12'))
}
