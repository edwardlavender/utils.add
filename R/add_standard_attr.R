#' @title Add informative attributes to an object
#' @description This function adds informative attributes to an object, including the source of the script that produced the object, a title, description and details of any parameters (if requested) and the date/time of file creation. These attributes help to maintain the links between the scripts in which objects are produced and any saved objects.
#'
#' @param x An object.
#' @param title A string providing an informative title for the object.
#' @param description A string providing an informative description of the object.
#' @param param An object (e.g., a string) containing further details about an object.
#'
#' @examples
#' \dontrun{
#' x <- runif(10, 0, 1)
#' out <- add_standard_attr(x)
#' attributes(out)
#' out <- add_standard_attr(x, title = "A numeric vector")
#' attributes(out)
#' }
#'
#' @return The function returns the object, \code{x}, as inputted with standard attributes.
#'
#' @details This function requires rstudioapi.
#'
#' @author Edward Lavender
#' @export
#'

add_standard_attr <- function(x,
                              title = deparse(substitute(x)),
                              description = NULL,
                              param = NULL){
  if(!requireNamespace("rstudioapi", quietly = TRUE)){
    stop("This function requires 'rstudioapi'.")
  }
  attributes(x)$source <- rstudioapi::getSourceEditorContext()$path
  if(!is.null(title)) attributes(x)$title <- title
  if(!is.null(description)) attributes(x)$description <- description
  if(!is.null(param)) attributes(x)$param <- param
  attributes(x)$date_creation <- Sys.time()
  return(x)
}
