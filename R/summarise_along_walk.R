#####################################################
#####################################################
#### summarise_along_walk()

#' @title Summarise every n numbers in a vector
#' @description This function summarises every n numbers in a vector.
#' @param vec A numeric vector.
#' @param every An integer that defines the step length of the walk over the vector.
#' @param summarise A function that summarises the numbers in each step.
#' @param na.rm A logical value that defines whether or not to remove NAs.
#' @param ... Additional arguments passed to \code{summarise}.
#'
#' @return The function returns a numeric vector.
#'
#' @examples
#' x <- c(rep(1, 10), rep(2, 10))
#' summarise_along_walk(x, every = 10)
#' x <- c(mean(10, 5), mean(100, 5))
#' summarise_along_walk(x, every = 10, summarise = mean)
#' x <- c(x, NA)
#' summarise_along_walk(x, every = 10, summarise = mean, na.rm = TRUE)
#'
#' @author Edward Lavender
#' @source This function is a slight modification of the code provided here: https://stackoverflow.com/questions/43635846/calculating-mean-for-every-n-values-from-a-vector.
#'
#' @export

summarise_along_walk <- function(vec, every, summarise = sum, na.rm = FALSE,...) {
  n <- length(vec)
  x <- .colSums(vec, every, n %/% every, na.rm = na.rm)
  r <- n %% every
  if (r) x <- c(x, summarise(vec[(n - r + 1):n], na.rm = na.rm,...))
  return(x)
}
