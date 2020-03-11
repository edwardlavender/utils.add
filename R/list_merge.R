#' @title Merge lists accounting for empty lists
#' @description This function is a wrapper for \code{\link[rlist]{list.merge}}. The difference is that this function first screens out any empty lists, which cause errors for \code{\link[rlist]{list.merge}}. If there is only one non-empty list, this is returned. Otherwise, \code{\link[rlist]{list.merge}} is used to merge lists in an iterative process. For large lists, this approach will be slower than calling \code{\link[rlist]{list.merge}} directly if there are no empty lists. Both \code{\link[rlist]{list.merge}} and \code{list_merge()} require named lists.
#'
#' @param ... named lists
#'
#' @examples
#'
#' #### (1) rlist::list.merge() returns an error for empty lists
#' \dontrun{
#'   rlist::list.merge(list(a = 1), list(b = 2, c = 5, a = list(d = 1)), list())
#'   }
#'
#' #### (2) rlist::list.merge() requires non empty lists:
#' rlist::list.merge(list(a = 1), list(b = 2, c = 5, a = list(d = 1)))
#'
#' #### (3) list_merge() ignores empty lists, returning output identical to rlist::list.merge():
#' list_merge(list(a = 1), list(b = 2, c = 5, a = list(d = 1)), list())
#'
#' #### (4) rlist::list.merge() and list_merge() require named lists,
#' # ... so the following code returns an error in both cases:
#' \dontrun{
#'   rlist::list.merge(list(a = 1), list(b = 2, c = 5, a = list(d = 1)), list(1))
#'   list_merge(list(a = 1), list(b = 2, c = 5, a = list(d = 1)), list(1))
#' }
#'
#' @author Edward Lavender
#' @export
#'

########################################
########################################
#### list_merge()

list_merge <- function(...){
  # Define overall list
  lists <- list(...)
  # Identify empty lists
  pos_empty <- which(sapply(lists, function(x) length(x) == 0))
  # Remove any empty lists
  if(length(pos_empty) > 0){
    lists[pos_empty] <- NULL
  }
  # If there is only one list left, simply return that list
  if(length(lists) == 1){
    return(unlist(lists, recursive = FALSE))

    # Otherwise, use rlist::list.merge() to join lists
  } else{
    # Define the first list
    l <- lists[[1]]
    # Iteractively add to this list
    for(i in 2:length(lists)){
      l <- rlist::list.merge(l, lists[[i]])
    }
    return(l)
  }
}

#### End of code.
########################################
########################################
