#' @title Convert a Julian day to a date/month/season
#' @description This function converts a Julian day to a date/month/season in any given year. This is useful, for instance, when making inferences from models of a response ~ Julian day, if you want to quickly draw inferences from models in terms of time units that are more familiar.
#'
#' @param yday A numeric input specifying the Julian day (the number of days since January 1st).
#' @param origin A character date (YYYY-MM-DD) that defines the first day of the year from which Julian day has been calculated.
#' @param verbose A logical input that defines whether or not to print the date, month and season.
#' @param return_list A logical input that defines whether or not to return the date, month  and year in a list.
#'
#' @examples
#' yday2date(16)
#' yday2date(16, return_list = FALSE)
#' yday2date(16, verbose = FALSE)
#'
#' @author Edward Lavender
#' @export



######################################################
######################################################
#### yday2date()

yday2date <-
  function(
    yday,
    origin = "2016-01-01",
    verbose = TRUE,
    return_list = FALSE){

    #### Define date, month and season
    # Define the date, based on a supplied origin:
    date <- as.Date(yday, origin = origin, tz = "UTC")
    # Define month
    month <- month.name[lubridate::month(date)]
    # Define season
    season <- lunar::terrestrial.season(date)

    #### Print output
    # Print the outputs if specified; normally this is sufficient
    # ... if yday2date is used to quickly gain an understanding of the
    # ... dates on plots with julian day
    if(verbose){
      print(paste("date:", date, "; month:", month, "; season:", season))
    }

    #### Return output
    # Return a list of the outputs if specified
    if(return_list){
      ls <- list(date = date, month = month, season = season)
      return(ls)
    } # close if(return_list){
  } # close function


#### End of function.
######################################################
######################################################
