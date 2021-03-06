% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{check_tz}
\alias{check_tz}
\title{Check the timezone of an object and force UTC if absent}
\usage{
check_tz(arg = deparse(substitute(input)), input)
}
\arguments{
\item{arg}{(optional) A character string which defines the argument of the parent function.}

\item{input}{An object.}
}
\value{
An object as inputted in which, if the object is of class Date or POSIXct and a time zone is absent, time zone "UTC" is forced.
}
\description{
This function checks the time zone of an inputted  object. If the object is of class Date or POSIXct and a time zone is absent, then "UTC" is forced. Otherwise, the object is returned unchanged.
}
\examples{
check_tz(input = as.POSIXct("2016-01-01"))
check_tz(arg = "t", input = as.POSIXct("2016-01-01"))
check_tz(arg = "t", input = as.POSIXct("2016-01-01", tz = "UTC"))

}
\author{
Edward Lavender
}
