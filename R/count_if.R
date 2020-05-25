#' @title Count the number of observations that meet a condition
#' @description This function returns the number of observations that meet a condition. This is a simple wrapper function for \code{\link[base]{which}} and \code{\link[base]{length}} designed to increase code readability and reduce code length.
#'
#' @param condition A logical vector.
#' @param ... Additional arguments (none currently implemented).
#'
#' @return A number which defines the number of elements in a vector which meet the condition specified.
#'
#' @examples
#' x <- c(1, 2, 4, 4, 3)
#' count_if(x < 2)
#'
#' @author Edward Lavender
#' @export

count_if <- function(condition,...){
  count <- length(which(condition))
  return(count)
}
