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

######################################################
######################################################
#### round_range()

#### round_range:
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


#### End of function.
######################################################
######################################################
