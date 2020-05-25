#' @title Define month-year categories from timeseries
#' @description This function defines month-year categories from timeseries data (i.e., \code{\link[base]{Date}} and \code{\link[base]{DateTimeClasses}} objects).
#'
#' @param x An vector of class \code{\link[base]{Date}} or \code{\link[base]{DateTimeClasses}}.
#' @param levels A logical input which defines whether or not to return a factor with chronologically ordered levels (i.e., levels ordered by year then month, for the inputted \code{x}) or a character (\code{levels = FALSE}).
#'
#' @return The function returns a vector which specifies the month and year of each observation in \code{x} as mm-yyyy. By default (i.e., when \code{levels = TRUE}), this is a factor with chronologically ordered levels (i.e., levels ordered by year then month, for the inputted \code{x}). If \code{levels = FALSE}, a character vector is returned.
#'
#' @examples
#' #### Example (1): Extract the month-year category from Dates:
#' mmyy(as.Date("2016-01-01"))
#' mmyy(seq.Date(as.Date("2016-01-01"), as.Date("2016-12-01"), 10))
#'
#' #### Example (2): Extract the month-year category from POSIXct objects:
#' mmyy(as.POSIXct("2016-01-01"))
#' mmyy(seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-12-01"), "20 days"))
#'
#' #### Example (3): By default, mmyy() returns an ordered factor
#' # ... with levels ordered chronologically:
#' mmyy(c(as.POSIXct(c("2017-01-02", "2016-01-02", "2016-03-01", "2017-02-03"))))
#'
#' #### Example (4): Ordered levels can be suppressed with levels = FALSE,
#' # ... in which case a character vector is returned:
#' mmyy(as.Date("2016-01-01"), levels = FALSE)
#' mmyy(seq.Date(as.Date("2016-01-01"), as.Date("2016-12-01"), 10), levels = FALSE)
#' mmyy(as.POSIXct("2016-01-01"), levels = FALSE)
#' mmyy(seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-12-01"), "20 days"), levels = FALSE)
#'
#' @author Edward Lavender
#' @export
#'

mmyy <- function(x, levels = TRUE){
  #### Define months, adjusting those with only one digit to be 01
  mm <- as.character(lubridate::month(x))
  pos1  <- which(nchar(mm) == 1)
  if(length(pos1) > 0) mm[pos1] <- paste0(0, mm[pos1])
  #### Define years
  yy <- lubridate::year(x)
  #### Define month-year categories
  mmyy <- paste0(mm, "-", yy)
  #### Assign ordered factor levels, if requested.
  if(levels){
    dl <- data.frame(mm = mm, yy = yy, mmyy = mmyy)
    dl <- dl[order(dl$yy, dl$mm), ]
    mmyy <- factor(mmyy, levels = unique(dl$mmyy))
  }
  return(mmyy)
}



