######################################
######################################
#### list_depth()

#' @title Find the maximum depth of a list
#' @description Descend a list and find the maximum number of levels in a list.
#' @param x A list.
#' @details A possibly nested list of lists is descended to determine the maximum number of levels.
#' @return The maximum number of levels in the list.
#' @source This function and the documentation are derived from \code{plotrix::listDepth()}.
#' @export
#'

list_depth <- function(x) {
  if(is.list(x)) {
    maxdepth<-1
    for(lindex in seq_along(x)) {
      newdepth<-list_depth(x[[lindex]])+1
      if(newdepth > maxdepth) maxdepth<-newdepth
    }
  }
  else maxdepth<-0
  return(maxdepth)
}
