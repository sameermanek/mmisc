#' Expand data over missing value
#' This is particularly useful for graphing (where you wnant NAs to indicate missing data)
#' This can also be useful when calculating rolling means, etc
#' 
#' @param df is a data.frame of data
#' @param mxt.key.cols is a character vector that specifies the columns for which you want all combinations of
#' OR mxt.key.cols is a named list, where the values of the list are additional levels to include
#' @export
#' @examples
#' df <- data.frame(a = 1:4, b = c('A','B','C','D'), c = runif(4), stringsAsFactors=FALSE)
#' expandData(df, c('a','b'))
#' expandData(df, list(a = c(1:10), b=LETTERS))
expandData <- function(df, mxt.key.cols) {
  if(!is.data.frame(df)) {
    stop('df needs to be a data.frame')
  }
  if((!is.list(mxt.key.cols) || is.null(names(mxt.key.cols))) && !is.character(mxt.key.cols)) {
    stop('mxt.key.cols needs to be a named list or a character vector')
  }
  if(is.list(mxt.key.cols)) {
    lst.cols <- mxt.key.cols
  } else {
    lst.cols <- lapply(mxt.key.cols, function(x) return(NULL))
    names(lst.cols) <- mxt.key.cols
  }
  df.grid <- do.call(expand.grid, c(stringsAsFactors=FALSE, 
                                   lapply(names(lst.cols), function(x) {
                                     unique(c(df[[x]], lst.cols[[x]]))
                                   })
                                  )
              )
  names(df.grid) <- names(lst.cols)
  df <- merge(data.table::data.table(df), 
              data.table::data.table(df.grid),
              by=names(lst.cols),
              all=TRUE)
  return(df)
}