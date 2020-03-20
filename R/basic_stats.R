#' @title Calculate basic statistics for a numeric vector
#' @description Calculate basic statistics (e.g. \code{mean}, \code{min}, \code{max}, \code{sd}, \code{IQR}) from a numeric vector.
#'
#' @param x A numeric vector for which to calculate statistics.
#' @param f A named list of functions which are used to calculate statistics.
#' @param p A function for processing summary statistics. The default is a function which rounds each output to the nearest two decimal places.
#' @param output A character specifying the format in which summary statistics should be returned. The currently implemented options are: (1) \code{"vec"} a numeric vector; (2) \code{"ldf"} a long-format dataframe in long format; and (3) \code{"wdf"}, a wide-format dataframe.
#' @param ... Other arguments that apply to all functions in \code{f}, such as \code{na.rm = TRUE}.
#'
#' @return Basic statistics for a vector of numbers, either in the format of a numeric vector, a long-format dataframe or a wide-format dataframe depending on the input to \code{output} (see above).
#'
#' @examples
#'
#' #### Define a vector:
#' x <- runif(10, 0, 10)
#'
#' #### Example (1): Numeric vector output
#' basic_stats(x, output = "vec")
#'
#' #### Example (2): Long-format dataframe output
#' basic_stats(x, output = "ldf")
#'
#' #### Example (3): Wide-format dataframe output
#' basic_stats(x, output = "wdf")
#'
#' #### Example (4): Functions can be adjusted in f argument by supplying a named list:
#' basic_stats(x,
#'             f = list(median = median,
#'                      quantile25 = function(x) { quantile(x, prob = 0.25) }),
#'             output = "ldf"
#'             )
#'
#' #### Example (5) Save typing by passing any arguments that apply to all functions via ...
#' basic_stats(c(x, NA), f = list(mean = mean, min = min), na.rm = TRUE)
#'
#' @author Edward Lavender
#' @export
#'

##############################################
##############################################
#### basic_stats()

basic_stats <-
  function(x,
           f = list(mean = mean, min = min, max = max, sd = stats::sd, IQR = stats::IQR),
           p = function(x){ round(x, digits = 2) },
           output = "ldf",...){
  # Apply each function to create a numeric vector with each summary statistic
  vec <- sapply(f, function(foo){ foo (x,...) })
  # Apply processing function
  vec <- p(vec)
  # long-format dataframe
  ldf <- data.frame(value = vec)
  ldf$statistic <- rownames(ldf)
  # wide-format dataframe
  wdf <- tidyr::spread(ldf, "statistic", "value")
  # return desired output
  return(eval(parse(text = output)))
  }

#### End of function.
##############################################
##############################################
