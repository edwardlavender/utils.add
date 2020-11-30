####################################
####################################
#### basic_stats()

#' @title Calculate basic statistics for a numeric vector
#' @description Calculate basic statistics (e.g. \code{mean}, \code{min}, \code{max}, \code{sd}, \code{IQR}) from a numeric vector.
#'
#' @param x A numeric vector for which to calculate statistics.
#' @param f A named list of functions which are used to calculate statistics.
#' @param p A function for processing summary statistics. The default is a function which rounds each output to the nearest two decimal places. \code{p = NULL} leaves summary statistics unchanged.
#' @param output A character specifying the format in which summary statistics should be returned. The currently implemented options are: (1) \code{"vec"} a numeric vector; (2) \code{"ldf"} a long-format dataframe in long format; and (3) \code{"wdf"}, a wide-format dataframe. The default option is \code{"wdf"}.
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
#' #### Example (6) Unsupported outputs return a warning and the function defaults to 'ldf'
#' \dontrun{
#' basic_stats(1:10, output = "df")
#' }
#'
#' @author Edward Lavender
#' @export
#'

basic_stats <-
  function(x,
           f = list(min = min,
                    mean = mean,
                    median = stats::median,
                    max = max,
                    sd = stats::sd,
                    IQR = stats::IQR,
                    MAD = stats::mad),
           p = function(x) round(x, digits = 2) ,
           output = "wdf",...){
  # Check f is named list
  check_named_list(input = f,
                   ignore_empty = FALSE)
  # Apply each function to create a numeric vector with each summary statistic
  vec <- sapply(f, function(foo){ foo (x,...) })
  # Apply processing function
  if(!is.null(p)) vec <- p(vec)
  # long-format dataframe
  ldf <- data.frame(value = vec)
  ldf$statistic <- factor(names(f), levels = names(f))
  rownames(ldf) <- 1:nrow(ldf)
  # wide-format dataframe
  wdf <- tidyr::spread(ldf, "statistic", "value")
  wdf <- wdf[, names(f)]
  rownames(wdf) <- 1:nrow(wdf)
  # return desired output
  if(!(output %in% c("vec", "ldf", "wdf"))){
    warning(paste0("output = '", output, "' not supported; defaulting to output = 'wdf'."))
    output <- "wdf"
  }
  return(eval(parse(text = output)))
  }


####################################
####################################
#### est_prop()

#' @title Estimate proportions and standard errors
#' @description This function estimates proportions and their associated standard errors/confidence intervals from binary/binomial observations. These can be supplied as a binary vector of successes and failures or as integers that define the number of successes and failures. The function calculates the observed and expected proportion of successes, along with their standard errors and confidence intervals, and returns these estimates in a dataframe.
#' @param x A binary vector of successes and failures or an integer that defines the number of successes.
#' @param y (optional) An integer that defines the number of failures.
#' @param accuracy A number that defines the accuracy to which estimates are rounded. This can be suppressed with \code{accuracy = NULL}.
#' @param ... Additional arguments (none implemented).
#' @details If \eqn{n} is the number of trials, and \eqn{n_1} is the number of successes, the estimated proportion is given by \eqn{\hat{p} = n_1/n} or \eqn{\hat{p} = (n_1 + 2)/(n + 4)} if \eqn{n_1 \leq 5} or \eqn{n - n_1 \leq 5}. The latter is a 'correction' for small sample sizes.  Corresponding standard errors are given by \eqn{SE = \sqrt{\hat{p}(1-\hat{p})/n}} or \eqn{SE = \sqrt{\hat{p}(1-\hat{p})/(n + 4)}} and 95 percent confidence intervals by \eqn{\hat{p} \pm t_{0.975,n-1}SE} (Gelman et al. 2021).
#' @return The function returns a dataframe with the following columns: 'n', the total number of samples; 'n_success', the total number of successes; 'n_failure', the total number of failures; 'p_obs', the empirical probability of success; 'p_hat', the estimated probability of success; 'se', the standard error; 'lower_ci', the lower confidence bound; 'upper_ci', the upper confidence bound; 'ci', the confidence interval; 'truncate_lower' and 'truncate_upper', boolean variables which define whether or not the lower/upper confidence interval has been truncated at zero/one; and 'correction', a boolean variable which defines whether or not the estimates have been calculated using the correction for small samples (see Details). The dataframe also contains an 'se_prob_param' attribute which is a list of the arguments supplied to the function.
#' @examples
#' # Estimate proportions from a vector of successes and failures
#' est_prop(sample(c(0, 1), 10, replace = TRUE))
#' # Compare estimates proportions from a larger sample size
#' est_prop(sample(c(0, 1), 1e4, replace = TRUE))
#' # Manipulate the simulated probability of success and failure
#' x <- sample(c(0, 1), 1e4, replace = TRUE, prob = c(0.2, 0.8))
#' est_prop(x)
#' # Estimate proportions from a count of the number of successes and failures
#' est_prop(sum(x == 1), sum(x == 0))
#' # Adjust the accuracy of the results
#' est_prop(x, accuracy = NULL)
#' est_prop(x,accuracy = 0.01)
#' @references Gelman, A. et al. (2021) Regression and Other Stories. Cambridge, Cambridge University Press, pages 51-53.
#' @author Edward Lavender
#' @export
#'

est_prop <- function(x, y = NULL, accuracy = 0.001,...){

  #### Define the number of successes and failures
  if(is.null(y)){
    x <- as.integer(x)
    if(!all(unique(x) %in% c(0, 1))) stop("If y = NULL, 'x' should comprise a vector of 0s and 1s.")
    n_success <- length(which(x == 1))
    n_failure <- length(which(x == 0))
  } else{
    stopifnot(length(x) == 1)
    stopifnot(length(y) == 1)
    n_success <- x
    n_failure <- y
  }

  #### Define observed probability of success
  n <- n_success + n_failure
  if(n <= 1) stop('Proportions cannot be calculated for n <= 1.')
  p_obs <- n_success/n

  ##### Estimate the probability of success
  ## large 'n' implementation
  if(n_success >= 5 & n - n_success >= 5){
    p_hat <- n_success/n
    se <- sqrt(p_hat * (1 - p_hat)/n)
    correction <- FALSE
  } else{
    ## small 'n' implementation
    p_hat <- (n_success + 2)/(n + 4)
    se <- sqrt(p_hat * (1 - p_hat)/(n + 4))
    correction <- TRUE
  }

  #### Define confidence intervals
  ## Calculate intervals
  # lower_ci <- p_hat - 2*se
  # upper_ci <- p_hat + 2*se
  lower_ci <- p_hat + stats::qt(0.025, n-1)*se
  upper_ci <- p_hat + stats::qt(0.975, n-1)*se
  ## Truncate intervals between 0 and 1
  if(lower_ci < 0){
    lower_ci <- 0
    truncate_lower <- TRUE
  } else truncate_lower <- FALSE
  if(upper_ci > 1){
    upper_ci <- 1
    truncate_upper <- TRUE
  } else truncate_upper <- FALSE

  ##### Process outputs to desired accuracy
  if(!is.null(accuracy)){
    p_obs    <- plyr::round_any(p_obs, accuracy = accuracy, f = round)
    p_hat    <- plyr::round_any(p_hat, accuracy = accuracy, f = round)
    se       <- plyr::round_any(se, accuracy = accuracy, f = round)
    lower_ci <- plyr::round_any(lower_ci, accuracy = accuracy, f = round)
    upper_ci <- plyr::round_any(upper_ci, accuracy = accuracy, f = round)
  }

  ##### Define a dataframe to return
  # Define confidence intervals
  ci <- paste0("[", lower_ci, ",", upper_ci, "]")
  # Define dataframe
  out <- data.frame(n = n,
                    n_success = n_success,
                    n_failure = n_failure,
                    p_obs = p_obs,
                    p_hat = p_hat,
                    se = se,
                    lower_ci = lower_ci,
                    upper_ci = upper_ci,
                    ci = ci,
                    truncate_lower = truncate_lower,
                    truncate_upper = truncate_upper,
                    correction = correction
                    )
  attributes(out)$se_prob_param <- list(x = x, y = y, accuracy = accuracy)
  return(out)
}

####################################
####################################
#### dev_expl()

#' @title Calculate the percent deviance explained by a GLM or GAM
#' @description This function calculates the percent deviance explained by a model, such as a generalised linear model (GLM) or generalised additive model (GAM).
#' @param model A model object.
#' @details Percent deviance explained is given by \code{(model$null.deviance - model$deviance)/model$null.deviance) * 100}.
#' @return The function returns the percent deviance explained as a number.
#' @examples
#' n <- 100
#' x <- stats::runif(n, 0, 10)
#' y <- stats::rpois(n, x*2)
#' m1 <- stats::glm(y ~ x)
#' dev_expl(m1)
#' m2 <- mgcv::gam(y ~ s(x))
#' dev_expl(m2)
#'
#' @author Edward Lavender
#' @export
#'

dev_expl <- function(model){
  if(is.null(model$null.deviance) | is.null(model$deviance)){
    stop("model$null.deviance or model$deviance is NULL.")
  }
  return((model$null.deviance - model$deviance)/model$null.deviance * 100)
}




