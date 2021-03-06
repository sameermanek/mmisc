% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coalesce.R
\name{coalesce}
\alias{coalesce}
\title{Return the first non-NA value for each element in a vector}
\usage{
coalesce(...)
}
\arguments{
\item{...}{a sequence of vectors. Will recycle as necessary}
}
\value{
a vector of the same length as the first vector in ...
}
\description{
Return the first non-NA value for each element in a vector
}
\examples{
coalesce(c(1,2,NA,NA,NA), c(1,NA,3,NA,NA), 0)
}
