#' @title Find the quantile bin in which an observation belongs
#' @description This function identifies the quantile interval (or 'bin') in which an observation belongs, given an empirical distribution of values (\code{x}). For observations that lie beyond the range of \code{x}, the original distribution can be 'padded' by adding an extreme minimum and/or maximum value to increase the range of the distribution (see Details). Sample quantiles are calculated for \code{x} using user-specified probabilities (\code{probs}) via \code{\link[stats]{quantile}}. The quantile bins in which each observation (\code{y}) falls is then determined and returned in a dataframe.
#' @param x A numeric vector.
#' @param y A numeric vector of observations for which to determine the quantile bin in which they belong.
#' @param probs A numeric vector of probabilities, passed to \code{\link[stats]{quantile}}.
#' @param pad_min A number which is added to \code{x} to extend the left-hand side of the distribution of values encompassed.
#' @param pad_max A number which is added to \code{x} to extend the right-hand side of the distribution of values encompassed.
#' @param ... Additional arguments passed to \code{\link[stats]{quantile}}.
#'
#' @details Padding is implemented by this function to express observations that are more extreme than the original distribution of observations as quantiles of that distribution. This has a small effect on the lowest/highest quantiles of the distribution but this is often unnoticeable if the quantile bins to which an observation is assigned are sufficiently large (e.g., 2 %).
#'
#' @return The function returns a dataframe with the index of the quantile bin in which each observation belongs ('bin') and the corresponding probability ('prob').
#'
#' @examples
#' set.seed(0)
#' x <- runif(100, 0, 250)
#' y <- c(10, 100, 200)
#' find_quantile_bin(x, y)
#' find_quantile_bin(x, y, pad_min = 0, pad_max = 1e5)
#'
#' @author Edward Lavender
#' @export
#'

find_quantile_bin <- function(x,
                              y,
                              probs = seq(0, 1, by = 0.02),
                              pad_min = NULL,
                              pad_max = NULL,...){
  # Pad the distribution, if requested
  if(!is.null(pad_min)) x <- c(pad_min, x)
  if(!is.null(pad_max)) x <- c(x, pad_max)
  if(any(y < min(x, na.rm = TRUE)) | any(y > max(x, na.rm = TRUE))){
    stop("'y' value(s) lie beyond the range of 'x'.")
  }
  # Define quantiles
  qx <- stats::quantile(x = x, probs = probs,...)
  # Define the index of the bin in which y falls
  qx_int  <- findInterval(y, qx)
  # Define the corresponding probability and express between 0 and 1
  qx_prob <- qx[qx_int]
  qx_prob <- names(qx_prob)
  qx_prob <- as.numeric(substr(qx_prob, 1, nchar(qx_prob)-1))
  qx_prob <- qx_prob/100
  # Return a dataframe
  out <- data.frame(bin = qx_int, prob = qx_prob)
  return(out)
}
