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
#'
#' @author Edward Lavender
#' @export
#'

######################################################
######################################################
#### around()

# Define function:
around <-
  # Define the dataframe, the position around which we want to examine
  # ... and the number of rows (n) above and below this row which we'll examine.
  function(dataframe, position, n = 3, return = "list", verbose = TRUE){
    # Define message, displayed to the user
    # This is helpful if the user inputs a function for position,
    # ... and hasn't check what position that is!
    if(verbose){
      msg <- paste("Returning dataframe at position", position, "+/-", n, "positions...\'n")
      # Print the message:
      print(msg)
    }
    # Define dataframe
    dls <- lapply(position, function(i){
      d <- dataframe[(i-n):(i+n), ]
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



#### End of function.
######################################################
######################################################
