% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datetime.R
\name{Ops.datetime}
\alias{Ops.datetime}
\title{Make basic operators work on datetimes}
\usage{
\method{Ops}{datetime}(e1, e2)
}
\arguments{
\item{e1}{Vector of datetimes (or numbers).}

\item{e2}{Vector of datetimes (or numbers).}
}
\value{
Result of operation. Varies by operator, see details.
}
\description{
Define methods for handling basic operations on datetime objects. (e.g. \code{+}, \code{-} \code{<} etc) This is called by R whenever e.g. a datetime is subtracted from another datetime; not meant to be called by the user.
}
\details{
Supported operators are:
\itemize{
\item \code{+} Add seconds to datetimes.
\item \code{-} Get the difference (in seconds) between two datetimes. Or if a number is subtracted from a datetime, subtract that number of seconds from it.
\item \code{==} Check if two datetimes occur at the same time.
\item \code{!=} Check if two datetimes do not occur at the same time.
\item \code{<} Check if a datetime is before another.
\item \code{<=} Check if a datetime is before or at same time as another.
\item \code{>} Check if a datetime is after another.
\item \code{>=} Check if a datetime is after or at same time as another.
All comparisons are done in UTC. What time zone the datetimes happen to be displayed in is irrelevant. For example, "2000-01-01 12:00:00 UTC+03:00" and "2000-01-01 09:00:00 UTC+00:00" are equal, because they occurred at the same time. And "2000-01-01 12:00:00 UTC+03:00" and "2000-01-01 12:00:00 UTC+00:00" are not equal, since one occurred three hours before the other.
}
}
\seealso{
Section "Ops" in \code{?groupGeneric}, which lists all the operators.
}
