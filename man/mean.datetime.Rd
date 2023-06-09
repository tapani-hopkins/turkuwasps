% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datetime.R
\name{mean.datetime}
\alias{mean.datetime}
\title{Get mean datetime}
\usage{
\method{mean}{datetime}(x, ...)
}
\arguments{
\item{x}{Vector of datetimes.}

\item{...}{Other arguments passed to \code{\link[=mean]{mean()}}.}
}
\value{
Datetime object (in timezone offset UTC+00:00).
}
\description{
Get the average of a vector of datetime objects.
}
