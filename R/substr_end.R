#' @title Extract the last n letters of a character
#' @description This is a wrapper for \code{\link[base]{substr}} for the extraction of the last n letters of a character/word.
#'
#' @param x A character or character vector.
#' @param n A number which defines the number of letters from the end of the word to be retained.
#'
#' @return A character or character vector, as inputted, but including only the last n letters of each element.
#'
#' @examples
#'
#' substr_end(c("edward", "john"), 2)
#' substr_end(as.character(seq.Date(as.Date("2016-01-01"), as.Date("2016-01-31"), 1)), 5)
#'
#' @author Edward Lavender
#' @export

substr_end <- function(x, n){
  x <- as.character(x)
  x <- substr(x, nchar(x)-n+1, nchar(x))
  return(x)
}

