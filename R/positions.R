#' @title Extract the first position of each unique element in a vector
#' @description This function extracts the position of the first appearance of each unique element in a vector. This is a simple wrapper for \code{\link[base]{which}} and \code{\link[base]{duplicated}} designed to improve code readability and reduce code length.
#'
#' @param x A vector.
#'
#' @return An integer vector of the positions in a vector at which each unique element appears for the first time.
#'
#' @examples
#' pos_first_unique(c(1, 1, 1, 2, 4, 1, 2, 4, 9, 10, 1, 2))
#'
#' @author Edward Lavender
#' @export
#'

pos_first_unique <- function(x){
  return(which(!duplicated(x)))
}
