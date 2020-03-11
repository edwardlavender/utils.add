#' @title Calculate the difference between two numbers or times
#' @description Calculate a difference between two numbers or times. For numeric objects, the difference is calculated as \code{x2 - x1}. For time objects (i.e., \code{\link[base]{DateTimeClasses}} or \code{\link[base]{Dates}}), the difference is calculated using \code{\link[base]{difftime}}. A function can be supplied to process outputs; e.g. \code{\link[base]{abs}}, or \code{\link[base]{numeric}} for \code{\link[base]{difftime}} objects.
#'
#' @param x2 A number, \code{\link[base]{Dates}} or \code{\link[base]{DateTimeClasses}} object.
#' @param x1 A number, \code{\link[base]{Dates}} or \code{\link[base]{DateTimeClasses}} object.
#' @param ... Other arguments passed to \code{\link[base]{difftime}} e.g. \code{units = "secs"}.
#' @param f A function to process outputs.
#'
#' @return A difference.
#'
#' @examples
#'
#' #### Example (1) Numeric difference
#' difference(1, 2)
#'
#' #### Example (2) Numeric difference with post processing
#' difference(1, 2, f = abs)
#'
#' #### Example (3) Difference with times
#' difference(as.POSIXct("2016-01-05"), as.POSIXct("2016-01-01"))
#'
#' #### Example (4) Difference with times and units specified
#' difference(as.POSIXct("2016-01-05"), as.POSIXct("2016-01-01"), units = "mins")
#'
#' #### Example (5) Difference with times, specified units and post-processing
#' difference(as.POSIXct("2016-01-05"), as.POSIXct("2016-01-01"), units = "mins", f = as.numeric)
#'
#' @author Edward Lavender
#' @export
#'

########################################
########################################
#### difference()

difference <- function(x2, x1, f = NULL,...){
  if(class(x2)[1] %in% c("numeric", "integer")){
    d <- x2 - x1
  } else if(class(x2)[1] %in% c("POSIXct", "POSIXlt", "Date")){
    d <- difftime(x2, x1,...)
  }

  if(!is.null(f)){
    d <- as.numeric(d)
  }

  return(d)
}


#### End of code.
########################################
########################################
