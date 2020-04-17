#' @title Define available values
#' @description The opposite of \code{\link[base]{NA}}.
#' @param ... arguments passed to \code{\link[base]{NA}}.
#' @export
"isnt.na" <- Negate("is.na")
