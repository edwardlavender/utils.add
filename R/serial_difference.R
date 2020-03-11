#' @title Calculate the difference between sequential observations
#' @description This function provides a quick method to calculate the duration between sequential observations. The function is based on \code{\link[utils.add]{difference}} which can calculate the difference between numeric objects or timestamps (i.e. \code{\link[base]{Dates}} or \code{\link[base]{DateTimeClasses}}).
#'
#' @param x A vector of numbers or timestamps (i.e. \code{\link[base]{Dates}} or \code{\link[base]{DateTimeClasses}} supported by \code{\link[base]{difftime}}).
#' @param na.rm A logical input defining whether or not to remove NAs from the output vector. If \code{FALSE}, the last value is \code{NA} because a difference cannot be calculated between the final observation and the 'next' observation. If \code{TRUE}, this \code{NA} (and any other NAs) are removed.
#' @param ... parameters passed to \code{\link[utils.add]{difference}}. These include \code{units} (a character input specifying the units in which to return time differences) and \code{f}, a function to process differences.
#'
#' @return A vector of differences.
#'
#' @examples
#'
#' #### Define some timeseries data
#' d1 <- c(seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-01-29"), by = "2 mins"),
#'         seq.POSIXt(as.POSIXct("2016-01-30"), as.POSIXct("2016-02-03"), by = "20 mins"),
#'         seq.POSIXt(as.POSIXct("2016-04-01"), as.POSIXct("2016-05-01"), by = "days")
#'         )
#' length(d1)
#'
#' #### Example (1): Calculate the duration between observations
#' duration <- serial_difference(d1)
#' table(duration)
#' head(duration)
#' tail(duration) # The last value is NA
#'
#' #### Example (2): Calculate the duration between observations in user-specified units
#' # ... by supplying the units argument, which is passed to difference() to and then to difftime()
#' duration <- serial_difference(d1, units = "mins")
#'
#' #### Example (3): Implement post-processing by supplying f to difference()
#' serial_difference(d1, units = "days", f = function(x){round(as.numeric(x), 3)})
#'
#' #### Example (4): Remove any NAs
#' duration <- serial_difference(d1, units = "days", na.rm = TRUE)
#' # The last value is no longer NA:
#' tail(duration)
#' # The sequence is one unit shorter than the input sequence due to the removal of the final NA:
#' length(d1) - length(duration)
#'
#' #### Example (5) Numeric example
#' x <- c(seq(0, 10, by = 1), seq(11, 20, by = 0.1))
#' serial_difference(x)
#' serial_difference(x, na.rm = TRUE)
#'
#' @author Edward Lavender
#' @export
#'

#############################################
#############################################
#### serial_difference()

serial_difference <-
  function(
    x,
    na.rm = FALSE,...
    ){

  #### Updated: numeric and timestamp objects now supported.
  #### check format of timestamps:
  # if(!(class(x)[1] %in% c("POSIXct", "POSIXt", "Date"))){
  #   warning("Check the class of inputted timestamps. This should be a date-time or date object supported by difftime().")
  # }

  #### Define differences
  # dur <- as.numeric(difftime(dplyr::lead(x), x, units = units))
  dur <- utils.add::difference(dplyr::lead(x), x,...)

  #### Process differences (update: implemented via difference() instead)
  # if(!is.null(fp)){
  #   dur <- fp(dur)
  # }

  #### Remove NA in output vector
  if(na.rm){
    posNA <- which(is.na(dur))
    dur <- dur[-c(posNA)]
  }

  #### Return differences
  return(dur)
  }


#### End of code.
#############################################
#############################################
