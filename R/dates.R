#' Convert strings to date
#' 
#' \code{toDate} converts a raw date format (e.g., YY-MM-DD, MM-DD-YYYY, etc) into a normalized
#' date format (YYYY-MM-DD) as a character vector. This is less clumsy than casting using 
#' \code{as.character(as.Date(..., format = ....))} assuming your goal is to create the 
#' normalized character output. While not intuitive, it will also perform basic validation
#' and is much, much quicker (through hashing, allowing multiple formats, and short-circuiting) 
#' than alternatives (inat least for my use case where I have many values, many of which are repeated)
#' 
#' @param chr.date.raw The input date vector, as characters
#' @param lst.date.formats Date formats, in the form of a list, with associated validation. For example,
#' \code{
#' lst.date.formats = list(c(validation = '^[0-9]{2}[a-zA-Z]{3}20[0-9]{2}$', format = '\%d\%b\%Y'))
#' }. 
#' You can include as many formats as you'd like within the lst.date.formats
#' @param log.stop.on.invalid Should this function stop if any (non-NA) element of \code{chr.date.raw} 
#' doesn't validate across any of the validation criteria?
#' @return The character vector (same ordering) in format YYYY-MM-DD
#' @examples 
#' toDate(c('03/15/2013','03MAR2015','2015-03-12'))
#' @export
toDate <- function(chr.date.raw, lst.date.formats = NULL, log.stop.on.invalid = TRUE) {
  #date validation
  if(is.null(lst.date.formats)) {
    lst.date.formats <- list(
      c(validation = '^[0-1]?[0-9]/[0-3]?[0-9]/[0-9]{2}$', format = '%m/%d/%y'),
      c(validation = '^[0-1]?[0-9]/[0-3]?[0-9]/[0-9]{4}$', format = '%m/%d/%Y'),
      c(validation = '^[0-9]{4}-[0-1]?[0-9]-[0-3]?[0-9]$', format = '%Y-%m-%d'),
      c(validation = '^[0-9]{2}-[0-1]?[0-9]-[0-3]?[0-9]$', format = '%y-%m-%d'),
      c(validation = '^[01]?[0-9]/[0-3]?[0-9]/[0-9]{4}', format = '%m/%d/%Y'),
      c(validation = '^[0-9]{2}[a-zA-Z]{3}[0-9]{4}$', format = '%d%b%Y')
    )
  }
  # this seems to handle lack of leading zeros okay.
  # 1) Move the hashing to pre-validation, 2) do the short circuiting (should be trivial if I change things to a 'while' loop)
  chr.keys <- unique(chr.date.raw)
  mat.date <- matrix(data = NA_character_, nrow = length(chr.keys), ncol = length(lst.date.formats))
  
  for(i in seq_along(lst.date.formats)) {
    chr.validation <- lst.date.formats[[i]]['validation']
    log.valid <- stringr::str_detect(chr.keys, chr.validation)
    if(any(log.valid)) {
      chr.format <- lst.date.formats[[i]]['format']
      mat.date[log.valid, i] <- as.character(as.Date(chr.keys[log.valid], format = chr.format))
    }
    if(all(apply(mat.date, 1, function(x) any(!is.na(x))))) {
      break;
    }
  }
  # now perform the lookups.
  chr.values <- do.call(coalesce, lapply(1:ncol(mat.date), function(x) mat.date[,x,drop=TRUE ]))
  chr.date <- chr.values[match(chr.date.raw, chr.keys)]
  
  if(any(is.na(chr.date) & !is.na(chr.date.raw))) {
    chr.message <- paste0('Invalid date format detected ', paste0(chr.date.raw[which(is.na(chr.date) & !is.na(chr.date.raw))], collapse = '\n') )
    if(log.stop.on.invalid) stop(chr.message) else warning(chr.message)
  }
  return(chr.date)
} 

getDiffDate <- function(chr.start.date = '1900-01-01', chr.end.date = '2100-12-31') {
  chr.dates <- as.character(seq(as.Date(chr.start.date), as.Date(chr.end.date), 'day'))
  diffDate <- function(chr.end.date, chr.start.date) {
    match(chr.end.date,chr.dates) - match(chr.start.date, chr.dates)
  }
  diffDate
}
#' Calculate the difference between dates (vectorized)
#' @param chr.end.date End dates (in the same format), same length as chr.start.date
#' @param chr.start.date Start dates (in format YYYY-MM-DD). 
#' Strongly recommend using toDate to get this format
#' @return Number of days (as numeric) between end date and start date
#' @examples  
#' chr.start <- toDate(c('2017-01-01','2016-03-01','2015-01-01'))
#' chr.end <- toDate(c('2013-03-01', '2016-03-16', '2016-09-09'))
#' diffDate(chr.end, chr.start)
#' @export
diffDate <- getDiffDate()



getTrailingDaysFunction <- function(chr.start = '1900-01-01', chr.end = '2100-12-31') {
  chr.dates <- as.character(seq(as.Date(chr.start), as.Date(chr.end), 'day'))
  getTrailingDays <- function(chr.end.date, int.length) {
    # Should do rigorous checks? No, would slow down too much
    chr.ret <- chr.dates[match(chr.end.date, chr.dates) + (-(int.length-1):0)]
    if(any(is.na(chr.ret))) stop('Requested dates outside available window. Please rebuild getTrailingDays function')
    return(chr.ret)
  }
  return(getTrailingDays)
}

#' Get a sequence of trailing days, inclusive
#' 
#' @param chr.end.date The end date (only one)
#' @param int.length The number of days
#' @return A character vector of int.length, with the trailing series of days
#' @examples 
#' getTrailingDays('2017-04-01', 12)
#' @export
getTrailingDays <- getTrailingDaysFunction()


getFutureDaysFunction <- function(chr.start = '1900-01-01', chr.end = '2100-12-31') {
  chr.dates <- as.character(seq(as.Date(chr.start), as.Date(chr.end), 'day'))
  getFutureDays <- function(chr.start.date, int.length) {
    chr.ret <- chr.dates[match(chr.start.date, chr.dates) + 0:(int.length - 1)]
    if(any(is.na(chr.ret))) stop('Requested dates outside available window. Please rebuild getFutureDays function')
    return(chr.ret)
  }
}
#' Get a sequence of future days, inclusive
#' 
#' @param chr.start.date The start date (only one)
#' @param int.length The number of days
#' @return A character vector of int.length, with the future series of days
#' @examples
#' getFutureDays('2017-04-01', 12)
#' @export
getFutureDays <- getFutureDaysFunction()

getDateSequenceFunction <- function(chr.start = '1900-01-01', chr.end = '2100-12-31') {
  chr.dates <- as.character(seq(as.Date(chr.start), as.Date(chr.end), 'day'))
  getDateSequence <- function(chr.start.date, chr.end.date, int.by = 1) {
    int.s <- data.table::chmatch(chr.start.date,chr.dates)
    int.e <- data.table::chmatch(chr.end.date, chr.dates)
    return(chr.dates[seq(int.s, int.e,by = int.by)])
    
  }
  return(getDateSequence)
}
#' Get a sequence of dates, inclusive
#' 
#' @param chr.start.date The start date (only one)
#' @param chr.end.date The end date (only one)
#' @param int.by The interval by which dates should be given
#' @return A character vector with the intervening days
#' @examples 
#' getDateSequence('2017-04-01', '2018-01-01', 7)
#' getDateSequence('2016-02-01','2016-03-01', 1)
#' @export
getDateSequence <- getDateSequenceFunction()



getGetMonth <- function(chr.start.date = '1900-01-01', chr.end.date = '2100-12-31') {
  chr.dates <- getDateSequence(chr.start.date,chr.end.date)
  chr.month <- paste0(substr(chr.dates, 1, 8), '01')
  
  getMonth <- function(chr.date) {
    return(chr.month[data.table::chmatch(chr.date, chr.dates)])
  }
  return(getMonth)
}
#' Get the first day of a month for a vector of dates (as character strings)
#' Much easier and faster than using substr+paste
#' 
#' @param chr.date A character vector of dates
#' @return The months in a date (character) format. E.g., 2016-01-01
#' @examples 
#' getMonth(c('2013-03-12','2013-09-02'))
#' @export
getMonth <- getGetMonth()


# Some quick and easy date terms
getYearFunction <- function(chr.start = '1900-01-01', chr.end = '2100-12-31') {
  chr.dates <- seq(as.Date(chr.start), as.Date(chr.end), by = 'day')
  num.years <- data.table::year(chr.dates)
  chr.dates <- as.character(chr.dates)
  getYear <- function(chr.date) {
    num.years[match(chr.date, chr.dates)]
  }
  return(getYear)
}

#' Get the associated years (as numeric integers) from a series of dates. 
#' Much faster than using \code{data.table::year} or \code{as.numeric(substr(...))}
#' 
#' @param chr.date A character vector of dates
#' @return The (numeric) year
#' @examples
#' getYear(c('2013-03-01','2014-02-01'))
#' @export
getYear <- getYearFunction()


getSeasonFunction <- function(chr.start = '1900-01-01', chr.end = '2100-12-31') {
  chr.dates <- seq(as.Date(chr.start), as.Date(chr.end), by = 'day')
  chr.seasons <- c('Winter','Spring','Summer','Fall')[(floor(data.table::month(chr.dates) / 3) %% 4 + 1)]
  chr.dates <- as.character(chr.dates)
  getSeason <- function(chr.date) {
    chr.seasons[match(chr.date, chr.dates)]
  }
  return(getSeason)
}

#' Get the associated seasons (as a character) from a series of dates. 
#' Uses the monthly season definition (Dec-Feb is Winter, Mar-May is spring, etc)
#' Faster and easier than calculating otherwise
#' 
#' @param chr.date A character vector of dates
#' @return The (character) seasons
#' @examples 
#' getSeason(c('2013-03-01','2014-02-01'))
#' @export
getSeason <- getSeasonFunction()


getMonthNameFunction <- function(chr.start = '1900-01-01', chr.end = '2100-12-31') {
  chr.dates <- seq(as.Date(chr.start), as.Date(chr.end), by = 'day')
  chr.months <- format(chr.dates, '%b')
  chr.dates <- as.character(chr.dates)
  getMonthName <- function(chr.date) {
    chr.months[match(chr.date, chr.dates)]
  }
  return(getMonthName)
}
#' Get the associated month (as a character) from a series of dates. 
#' Faster and easier than getting otherwise
#' 
#' @param chr.date A character vector of dates
#' @return The (character) months
#' @examples 
#' getMonthName(c('2013-03-01','2014-02-01'))
#' @export
getMonthName <- getMonthNameFunction()

