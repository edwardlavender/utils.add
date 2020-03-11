#' @title The names of \code{mgcv} smooth terms
#' @description This function defines the names of \code{mgcv} smooth terms based on the type of smooth and the variable name(s). This is useful, for instance, for extracting term-specific predictions from a \code{\link[mgcv]{predict.gam}} or \code{\link[mgcv]{predict.bam}} object.
#'
#' @param s A character vector specifying the type of smooth term (e.g. "s", "te", "ti", "crs", "cr" etc.)
#' @param vars A character vector or list specifying variable names.
#'
#' @examples
#' # Supplying variables as a list:
#' sterm(s = "s", vars = list("th_interp"))
#' sterm(s = "ti", vars = list("hourofday", "julian_day"))
#' # Supplying variables as character vectors:
#' sterm(s = "s", vars = "th_interp")
#' sterm(s = "s", vars = c("hourofday", "julian_day"))
#'
#' @author Edward Lavender
#' @export
#'

##########################################
##########################################
#### sterm()

sterm <- function(s = "ti", vars = c("hourofday", "julian_day")){
  # If the variables have been supplied as a list...
  if(class(vars) == "list"){
    # Create smooth term name:
    sterm <- paste0(s, "(", stringi::stri_paste_list(vars, sep = "", collapse = ","), ")")
    # If the variable is simply supplied as a character or is a vector of characters...
  } else if(class(vars) == "vector" | (class(vars) == "character")){
    # Create smooth term name:
    sterm <- paste0(s, "(", paste0(vars, collapse = ","), ")")
  }
  # Return the smooth term
  return(sterm)
} # close function


#### End of code.
##########################################
##########################################
