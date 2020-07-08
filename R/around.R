####################################
####################################
#### around()

#' @title Return the rows around a specified position in a dataframe
#' @description This is a function like \code{\link[utils]{head}} or \code{\link[utils]{tail}} for dataframes which displays the values around a specified position.
#'
#' @param dataframe A dataframe.
#' @param position A position/numeric vector of positions around which you want to examine the rows of a dataframe.
#' @param n The number of adjacent positions around each position in \code{position} that you want to examine.
#' @param return A character specifying \code{"data.frame"} or \code{"list"}. If \code{"list"}, then all the rows around each \code{position} are returned in an element of a list; if \code{"data.frame"}, these are all returned in a single dataframe.
#' @param verbose A logical input that defines whether or not to print a helpful message describing the rows that are being examined.
#'
#' @return The function returns a dataframe or a list depending on the input to \code{return}.
#'
#' @examples
#'
#' #### Example (1): A single position
#' # Define data
#' set.seed(1)
#' nrows <- 50
#' x <- runif(nrows, 1, 50)
#' y <- rnorm(nrows, x * 10 - 5, 60)
#' dat <- data.frame(x = x, y = y)
#' around(dat, 10, 3, "data.frame")
#'
#' #### Example (2): Multiple positions, returned as a list
#' around(dat, c(10, 30, 39), 3, "list")
#'
#' #### Example (3): Multiple positions returned as a dataframe
#' around(dat, c(10, 30, 39), 3, "data.frame")
#'
#' #### Example (4): The function accounts for position at the start/end of the dataframe
#' around(dat, c(1, 10, nrow(dat)), 3)
#'
#'
#' @author Edward Lavender
#' @export
#'

around <-
  function(dataframe, position, n = 3, return = "list", verbose = TRUE){
    # Define message, displayed to the user
    # This is helpful if the user inputs a function for position,
    # ... and hasn't check what position that is!
    if(verbose){
      msg <- paste("Returning dataframe at position", position, "+/-", n, "positions...\n")
      # Print the message:
      cat(msg)
    }
    # Define dataframe
    rows <- 1:nrow(dataframe)
    dls <- lapply(position, function(i){
      sel <- (i-n):(i+n)
      sel <- sel[which(sel %in% rows)]
      d <- dataframe[sel, ]
      return(d)
    })
    # Return format
    if(return == "data.frame"){
      dout <- dplyr::bind_rows(dls)
    } else{
      dout <- dls
    }
    # Return the dataframe:
    return(dout)
  }


####################################
####################################
#### right()

#' @title Select the last \code{n} columns of a dataframe or matrix
#' @description This function returns the last \code{n} columns of a dataframe or matrix.
#' @param x A dataframe or matrix.
#' @param n The number of columns to return.
#' @return The function returns the last \code{n} columns of a dataframe or matrix.
#' @examples
#' right(x = data.frame(x = 1, y = 1, z = 1), n = 2L)
#' @author Edward Lavender
#' @export

right <- function(x, n = 6L){
  stopifnot(n > 0)
  if(n > ncol(x)){
    warning("n > ncol(x); simply returning x.")
    return(x)
  } else{
    return(x[, (ncol(x)-n+1):ncol(x)])
  }
}


####################################
####################################
#### left()

#' @title Select the first \code{n} columns of a dataframe or matrix
#' @description This function returns the first \code{n} columns of a dataframe or matrix.
#' @param x A dataframe or matrix.
#' @param n The number of columns to return.
#' @return The function returns the first \code{n} columns of a dataframe or matrix.
#' @author Edward Lavender
#' @examples
#' left(x = data.frame(x = 1, y = 1, z = 1), n = 4)
#' @export

left <- function(x, n = 6L){
  stopifnot(n > 0)
  if(n > ncol(x)){
    warning("n > ncol(x); simply returning x.")
    return(x)
  } else{
    return(x[, 1:n])
  }
}


#### End of code.
####################################
####################################
