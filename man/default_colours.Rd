% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal_functions.R
\name{default_colours}
\alias{default_colours}
\title{Get default colours for taxa}
\usage{
default_colours(n)
}
\arguments{
\item{n}{Number of colours needed.}
}
\value{
Vector of colours in hexadecimal (e.g. "#00FFFF"), of length \code{n}.
}
\description{
Helper function used by e.g. \code{\link[=plot_time]{plot_time()}}. Gets the default colours used to plot different species. Gets the colours from a sequence of 30 colours (15 unique), which is recycled if more than 30 colours are needed.
}
\keyword{internal}
