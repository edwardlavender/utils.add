#' @title Define available values
#' @description The opposite of \code{\link[base]{NA}}.
#' @param ... arguments passed to \code{\link[base]{NA}}.
#' @export
"isnt.na" <- Negate("is.na")

#' @title Identify which elements are not in a vector
#' @description The opposite of \code{\link[base]{`%in%``}}
#' @export
"%ni%" <- Negate(`%in%`)
