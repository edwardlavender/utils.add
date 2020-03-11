#' @title Find the position in one dataframe that is closest in time to a value in another dataframe
#' @description This function is like \code{\link[base]{match}}, but the aim is to find the position in one dataframe that is the closest in time (\code{lookup}) to the time in another dataframe (\code{times}). The function is vectorised over the time in the first dataframe (\code{times}). This means that for every time inputted, the function finds the row in another dataframe which is closest in time to that time. For speed, the function assumes that the dataframe used for matching is arranged by timestamp.
#'
#' @param times A vector of timestamps for which you want to identify the position of the closest timestamp in another vector (\code{lookup}).
#' @param lookup A vector of timestamps for which you will determine the position of the closest timestamp to each time in \code{times}.
#'
#' @return The function returns a vector of positions which link times in one dataframe to observations closest in time in another dataframe.
#'
#' @examples
#' #### Example (1)
#' # Define dataframe to which we want to add information
#' d1 <- data.frame(t = seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-01-02"), by = "hours"))
#' # Define dataframe in which information is contained
#' d2 <- data.frame(t = seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-01-02"), by = "mins"))
#' d2$vals <- runif(nrow(d2), 0, 50)
#' # Use match_closest to add information to the first dataframe based on second dataframe
#' d1$position_in_d2 <- match_closest(times = d1$t, lookup = d2$t)
#' d1$vals <- d2$vals[d1$position_in_d2]
#' # Examine
#' head(cbind(d1, d2[d1$position_in_d2, ]))
#'
#' @author Edward Lavender
#' @export
#'

############################################
############################################
#### match_closest()

match_closest <-
  function(
    times,
    lookup
  ){
    dat <- data.frame(lookup = lookup)
    dat$after <- (lookup >= times)
    pos <- which.max(dat$after)
    return(pos)
  }

#### Vectorise function
match_closest <- Vectorize(match_closest, "times")


#### End of code.
############################################
############################################
