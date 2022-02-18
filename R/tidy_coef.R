#' @title Tidy a table of coefficients
#' @description This function tidies a table of coefficients. Values can be processed (e.g., by rounding) and row and column names adjusted. A text file can be saved that can be imported into other programmes (e.g., Microsoft Word).
#' @param coef A matrix of model coefficients, usually from \code{\link[stats]{coef}}. The matrix is expected to have row names (coefficient names) and numeric coefficients.
#' @param coef_names (optional) A character vector of 'tidy' coefficient names.
#' @param col_names (optional) A character vector of 'tidy' column names. The first column name should be the name of the column containing the coefficient names.
#' @param f (optional) A function to be applied to specified columns to tidy numeric outputs.
#' @param f_index An integer vector of the indices of the columns in \code{coef} to which the function \code{f} is applied. By default, if \code{f} is supplied, it is applied to all columns.
#' @param file (optional) A character which defines the name of the file to be saved. This is passed to \code{\link[utils]{write.table}}.
#' @param quote A logical value which defines whether or not to surround character or factor columns with quotes. This is passed to \code{\link[utils]{write.table}}.
#' @param sep The field separator string. This is passed to \code{\link[utils]{write.table}}.
#' @param row.names A logical value which defines whether or not row names are to be saved (or a character vector or the row names to be written). This is passed to \code{\link[utils]{write.table}}.
#' @param ... Other arguments passed to \code{\link[utils]{write.table}}.
#' @return The function returns/saves a tidier table of coefficients.
#' @examples
#' n <- 100
#' x <- runif(n, 0, 1)
#' y <- rnorm(n, 5*x, 10)
#' mod <- lm(y ~ x)
#' tidy_coef(coef(summary(mod)))
#' @author Edward Lavender
#' @export
#'

tidy_coef <- function(coef,
                      coef_names = rownames(coef),
                      col_names = c("Coefficient", "Estimate", "SE", "z-value", "p-value")[1:(ncol(coef)+1)],
                      f = function(x) prettyGraphics::add_lagging_point_zero(round(x, digits = 3), n = 3),
                      f_index = 1:ncol(coef),
                      file = NULL,
                      quote = FALSE,
                      sep = ",",
                      row.names = FALSE
                      ,...){

  #### Process selected columns in coef object
  coefp <- coef
  if(!is.null(f)){
    for(j in f_index){
      coefp[, j] <- f(as.numeric(coefp[, j]))
    }
  }

  #### Define tidy dataframe
  if(is.null(col_names)) col_names <- c("Coefficient", colnames(coefp))
  dat_coef <- data.frame(coefficient = rownames(coefp), coefp)
  colnames(dat_coef) <- col_names
  rownames(dat_coef) <- NULL
  if(!is.null(coef_names)) dat_coef[, 1] <- coef_names

  #### Return processed coefficients to console or to file
  if(!is.null(file)) utils::write.table(dat_coef, file = file, quote = quote, sep = sep, row.names = row.names,...)
  return(dat_coef)
}
