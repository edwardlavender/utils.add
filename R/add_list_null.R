#' @title Add \code{list(NULL)} to an existing list in place of absent elements
#' @description This function adds \code{list(NULL)} to a list at all the elements in a user-supplied vector that are \code{NULL}.
#'
#' @param l A list.
#' @param elm A character vector which defines list element names at which the function should check whether those elements exist and, if not, replace them with \code{list(NULL)}.
#'
#' @return A list, exactly as inputted, but in which \code{NULL} elements have been replaced by \code{list(NULL)}.
#'
#' @examples
#'
#' l <- list(a = 10, b = 5, c = 4, d = NULL)
#' add_list_null(l, "d")
#' add_list_null(l, "e")
#'
#' @author Edward Lavender
#' @export

##########################################
##########################################
#### add_list_null()

add_list_null <- function(l, elm){
  for(i in elm){
    if(is.null(l[[i]])){
      l[[i]] <- list(NULL)
    }
  }
  return(l)
}

#### End of function.
##########################################
##########################################
