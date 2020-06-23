#####################################
#####################################
#### round_range()

#' @title Round a range
#' @description This rounds the lower and upper values of a range down and up, respectively, by a user-specified amount. This can be useful, for example, in the creation of plots with suitable axis limits. The function uses \code{\link[plyr]{round_any}}.
#'
#' @param x A vector of two numbers to be round down and up respectively.
#' @param digits The precision of rounding (e.g. to the nearest 0.1). "auto" is allowed which attempts to define suitable accuracy automatically.
#' @return A number.
#'
#' @examples
#' round_range(c(0.987, 10.982), "auto")
#'
#' @author Edward Lavender
#' @export
#'

round_range <-

  #### Define inputs
  function(
    # Define x, a vector of two numbers (i.e. some range) to round down/up
    x,
    # Define the accuracy of rounding e.g. to the nearest 0.1
    digits = "auto"){

    #### Check the inputted range contains only two values:
    if(length(x) != 2){
      warning("Inputted 'range' (x) does not contain 2 numbers: only first and last number adjusted.")
    }

    #### If digits are to be automatically chosen...
    if(digits == "auto"){
      # Determine the order of magnitude by which the two numbers differ
      digits <- 1 * 10^(floor(log(max(x, na.rm = T) - min(x, na.rm = T), base = 10)))
      if(digits == 0){
        digits = 1*10-5
      }
    }

    #### Implement rounding
    # Round down first value and round up second value
    x[1] <- plyr::round_any(x[1], accuracy = digits, f = floor)
    x[2] <- plyr::round_any(x[2], accuracy = digits, f = ceiling)

    #### Return updated vector
    return(x)

    #### Close function
  }


##########################################
##########################################
#### seq_range()

#' @title Define a sequence between a range of numbers
#' @description This function is an implementation of \code{\link[base]{seq}} in which the user can simply input a range (i.e., a vector of two numbers).
#'
#' @param range A vector of two numbers representing \code{from} and \code{to} in \code{seq()}.
#' @param ... Other arguments passed to \code{\link[base]{seq}}.
#'
#' @examples
#'
#' #### Define a vector
#' x <- runif(10, 1, 5)
#'
#' #### Example (1) A sequence of specified length
#' seq_range(range(x), length.out = 10)
#'
#' #### Example (2) A sequence with a specified increment
#' seq_range(range(x), by = 0.001)
#'
#' @author Edward Lavender
#' @export
#'

seq_range <- function(range,...){
  first <- range[1]
  last <- range[2]
  seq_range <- seq(first, last, ...)
}


#####################################
#####################################
#### clip_within_range

#' @title Clip a vector to lie within a range
#' @description This function clips a vector to lie within a range.
#' @param x A vector, to be clipped.
#' @param range A vector of two numbers, used to clip \code{x}.
#' @return A vector, as inputted, solely comprising values within the range specified by \code{range}, inclusive.
#'
#' @examples
#' clip_within_range(1:10, c(5, 6))
#' clip_within_range(stats::runif(10, -10, 10), c(-9, 9))
#'
#' @author Edward Lavender
#' @export
#'

clip_within_range <-
  function(x, range){
    stopifnot(length(range) == 2)
    pos2rem <- which(x < range[1])
    if(length(pos2rem) > 0) x <- x[-c(pos2rem)]
    pos2rem <- which(x > range[2])
    if(length(pos2rem) > 0) x <- x[-c(pos2rem)]
    return(x)
  }


#### End of code
#####################################
#####################################
