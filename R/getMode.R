#' Returns the statistical mode of a vector
#' Will return the min mode in the case of ties.
#' 
#' @param x a vector (numeric or character) 
#' @return a one element vector of the same type as x
#' @export
getMode <- function(x) {
  log.numeric <- is.numeric(x)
  freq <- table(x)
  m <- names(freq)[which.max(freq)]
  if(log.numeric) m <- as.numeric(m)
  return(m)
}