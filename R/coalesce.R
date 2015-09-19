#' Return the first non-NA value for each element in a vector
#' 
#' @param ... a sequence of vectors. Will recycle as necessary
#' @return a vector of the same length as the first vector in ...
#' @export
#' @examples
#' coalesce(c(1,2,NA,NA,NA), c(1,NA,3,NA,NA), 0)
coalesce <- function(...) {
  l <- list(...)
  l <- l[!sapply(l, is.null)]
  if(length(l) == 0) return(NULL)
  if(length(l[[1]]) == 0) return(l[[1]])
  if.na <- function(x,y) {
    ifelse(is.na(x), y, x)
  }
  Reduce(if.na, l)
}