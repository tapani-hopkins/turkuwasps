% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datetime.R
\name{$<-.data.frame}
\alias{$<-.data.frame}
\title{Make data frames accept datetimes}
\usage{
\method{$}{data.frame}(x, name) <- value
}
\arguments{
\item{x}{Data frame into which a variable is being saved.}

\item{name}{Name to give to the column in the data frame.}

\item{value}{Variable being saved in the data frame.}
}
\value{
Data frame with new column added. If the new column is a datetime, it is converted to character.
}
\description{
Called by R whenever something is saved by name into a data frame (e.g. \code{x$name = value}). Not meant to be called by the user. Coverts any datetime object to character before continuing. Datetime objects are basically a list with two items (\code{d}=datetimes, \code{tz}=time zone offsets), not vectors, so they won't save properly into data frames if not converted.
}
