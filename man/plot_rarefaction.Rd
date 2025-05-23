% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rarefaction.R
\name{plot_rarefaction}
\alias{plot_rarefaction}
\title{Plot rarefaction curves}
\usage{
plot_rarefaction(
  x,
  n = 10,
  n_ci = NULL,
  p = 0.84,
  add = FALSE,
  by = NULL,
  col = "black",
  lty = 1,
  pch = NA,
  pch_col = "black",
  ...
)
}
\arguments{
\item{x}{Data frame containing the wasp data. Must contain columns "taxon" and "sample". Each row is an individual wasp.}

\item{n}{Number of resamples. Default (10) is fast, but gives very jagged curves. Increase to e.g. 100 to get smooth averaged out curves.}

\item{n_ci}{Number of resamples for estimating confidence intervals (see Details). Default (NULL) is to only show the curve without confidence intervals. 10 is fast, but gives very jagged intervals; 100 already gives quite smooth intervals.}

\item{p}{How large confidence intervals to draw around the rarefaction curves. Number between 0 and 1. Default (0.84) is to draw 84\% intervals, i.e. ± 1.4 standard errors. Used to estimate if two rarefaction curves are significantly different (e.g. for a significance of 0.05, check if the confidence intervals of two 84\% curves overlap, see Details).}

\item{add}{If TRUE, the rarefaction curve(s) are added to an existing plot. Default is to create a new plot.}

\item{by}{Name of column in 'x' to split the data by. E.g. if \code{by}="forest_type", draws separate rarefaction curves for each forest type. Curves are drawn in the same order as the order of the factor levels of the column (change the levels with \code{\link[=factor]{factor()}} if you e.g. want the curves in different order in the legend drawn by \code{\link[=legend_rarefaction]{legend_rarefaction()}}). Default is to draw one curve containing all the wasps.}

\item{col}{Colour to be used for the curves. Typically a string if only one curve is drawn. If several curves are drawn (\code{by} is not NULL), should preferably be a named character vector giving the colour for each curve. But unnamed vectors or a string work too, see 'Details'.}

\item{lty}{Line type to be used for the curves. Typically an integer between 0:6 if only one curve is drawn, see \code{\link[=par]{par()}} for accepted values. If several curves are drawn (\code{by} is not NULL), should preferably be a named vector giving the line type for each curve. But unnamed vectors or an integer work too, see 'Details'.}

\item{pch}{What symbols to use on the curve. Typically an integer between 0:18 if only one curve is drawn, see \code{\link[=points]{points()}} for accepted values. If several curves are drawn (\code{by} is not NULL), should preferably be a named vector giving the symbol for each curve. But unnamed vectors or a single integer work too, see 'Details'. Default is for the curve to be drawn without symbols.}

\item{pch_col}{Colour to be used for the symbols. Typically a string if only one curve is drawn. If several curves are drawn (\code{by} is not NULL), should preferably be a named character vector giving the colour of the symbols for each curve. But unnamed vectors or a string work too, see 'Details'. Default is black.}

\item{...}{Graphical parameters passed to the two functions which draw the curves, \code{\link[=plot]{plot()}} and \code{\link[=lines]{lines()}}. These will override any default values such as colours. A few parameters (such as 'type' and 'pch') may not work as expected.}
}
\value{
List with the curve coordinates, number of wasps and parameters, returned silently. This can be passed to \code{\link[=legend_rarefaction]{legend_rarefaction()}} to draw a legend with the right colours etc. The list has 6 items:
\itemize{
\item \code{r} The curve coordinates and standard errors. List with the x coordinates (=number of wasps) of the curves, the average y coordinates (number of species), and the standard errors. If several curves are drawn, returns a list of each curves's coordinates.
\item \code{nwasps} Number of wasps in the curve. If several curves are drawn, returns a table (basically a named vector) of the number of wasps of each curve.
\item \code{col} Colour of the curve. If several curves are drawn, returns a named vector of the colours of each curve.
\item \code{lty} Line type of the curve. If several curves are drawn, returns a named vector of the line types of each curve.
\item \code{pch} Symbols drawn on the curve. If several curves are drawn, returns a named vector of the symbols of each curve.
\item \code{pch_col} Colour of the symbols drawn on each curve. If several curves are drawn, returns a named vector of the symbol colours of each curve.
}
}
\description{
Plot rarefaction curves showing how quickly species accumulated. Draws "sample-based" curves (see Details), and displays the number of wasps caught on the x axis and number of species on the y axis.
}
\details{
\subsection{Named vectors for graphical parameters}{

If drawing several curves, parameters such as the colour (\code{col}) should preferably be given as named vectors. The function will use the names to match the colours to the corresponding curve.

For example if drawing four curves for four forest types ("primary", "swamp", "disturbed", "clearcut"), the colours could be: \code{col = c(primary="darkgreen", swamp="blue", disturbed="green", clearcut="yellow")}. These will automatically be matched to the correct forest type, whatever order the colours were given in.

Unnamed vectors work too: \code{col = c("darkgreen", "blue", "green", "yellow")}. But then you have to be \emph{completely sure} that the colours are in the same order as the curves are drawn. Curves are drawn in the same order as the factor levels of the column that the wasp data is split by. You can check what the order is with \code{levels0(x[, by])}. Unnamed vectors will also need to have the same length as the factor levels of the column.

A single value for the colours and other parameters works but gives the same value to all curves.
}

\subsection{Sample-based curves}{

The curves are sample-based rarefaction curves. This means that the wasps are drawn randomly, one sample at a time, and the number of species versus number of wasps is added to the plot. Samples keep on being drawn until all the wasps have been added.

Several randomly drawn curves are made (default is 10). The returned curve is an averaged version of these: at each point of the x axis, we take the average number of species.

There are good reasons to prefer randomly drawing the wasps one \emph{sample} at a time, instead of one \emph{wasp} at a time (see e.g. Gotelli & Colwell 2011: Estimating species richness). For the wasp data, they boil down to rarefaction curves basically being a re-enactment. We're re-enacting what would happen if we went back and sampled the area again, several times. How many species for a given number of wasps caught would we expect to get? We'd still be collecting the wasps one sample at a time, so it makes sense to keep the wasps of each sample together.
}

\subsection{Confidence intervals}{

The confidence intervals are approximate, and should be interpreted with caution. They are drawn a set number of standard errors above and below the rarefaction curve (default is ± 1.4 SE).

Standard errors are estimated by bootstrapping. Samples are randomly picked \emph{with replacement}, and a rarefaction curve calculated. This is repeated \code{n_ci} times, to give a set of rarefaction curves. The standard error at any given point of the x axis is the standard deviation of these curves.

This, however, underestimates the standard error. A better estimate would randomly pick samples from all possible samples, including those that were not collected (and which may include uncollected species). Since we are only picking samples from those that were actually collected, the variation will inevitably be smaller. This should not be too big a problem if most of the common species have been collected (i.e if coverage is good).

To check if two curves are significantly different, I recommend using 84\% confidence intervals (\code{p=0.84}). These should be reasonably conservative: if the intervals of two curves don't touch, they're likely significantly different (see e.g. MacGregor-Fors and Payton 2013, \href{https://doi.org/10.1371/journal.pone.0056794}{https://doi.org/10.1371/journal.pone.0056794}). Another option would be 95\% intervals, which are guaranteed conservative (e.g. Colwell et al. 2012, \href{https://doi.org/10.1093/jpe/rtr044}{https://doi.org/10.1093/jpe/rtr044}). Whichever or whatever is used, it is worth treating the intervals as a rough guide; I would e.g. be cautious in my interpretations of curves whose confidence intervals almost touch.
}
}
\examples{

# get example wasp data
f = system.file("extdata", "wasps_example.csv", package = "turkuwasps", mustWork = TRUE)
wasps = read_wasps(f)

# plot a rarefaction curve with all wasps
plot_rarefaction(wasps, n=5)

# plot separate curves for each forest type
col = c(primary="darkgreen", swamp="blue", disturbed="green", clearcut="yellow", farm="orange")
r = plot_rarefaction(wasps, by="forest_type", col=col, pch=1:4)

# add a legend
legend_rarefaction(r=r)

}
\seealso{
Function \code{\link[=combine_columns]{combine_columns()}}, which makes it easier to split the data by several columns, e.g. to draw separate rarefaction curves for each forest type and collecting event.
}
