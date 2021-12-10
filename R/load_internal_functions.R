#' @title Load internal functions from a package
#' @description This function loads internal functions from a package into the global environment. This is designed to support package testing.
#'
#' @param pkg A character that defines the name of the package.
#'
#' @section Warning:
#' This function modifies the global environment.
#'
#' @source The function was modified from code provided by Tyler Rinker (https://www.py4u.net/discuss/871790).
#'
#' @export

load_internal_functions <- function(pkg){
  stopifnot(inherits(pkg, "character"))
  if(!requireNamespace("pacman", quietly = TRUE))
    stop("This function requires the 'pacman' package.", call. = FALSE)
  hidden <-
    setdiff(pacman::p_funs(pkg, all = TRUE, character.only = TRUE),
            pacman::p_funs(pkg, all = FALSE, character.only = TRUE))
  lapply(hidden, function(x) {
    a <- strtrim(x, 1) == "%"
    b <- substring(x, nchar(x)) == "%"
    if (a && b) {
      x2 <- paste0("`", x, "`")
    } else {
      x2 <- x
    }
    assign(x, eval(parse(text = paste0(pkg, ":::", x2))),
           envir = .GlobalEnv)
  })
  return(invisible())
}
