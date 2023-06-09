% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resample.R
\name{resample}
\alias{resample}
\title{Fit glm and analyse it}
\usage{
resample(model, x, m, pairwise = NULL, family = "negative.binomial", ...)
}
\arguments{
\item{model}{Character string giving the model to fit. Generally something like "offset(tdiff_log) + days + rain + forest_type + deadwood". The variables should be found as column names in \code{m}. Converted to \link{formula}, and will accept e.g. interaction terms in the same format as for formulae.}

\item{x}{Data frame with the wasp data. Must contain columns "sample" and "taxon".}

\item{m}{Data frame with the Malaise sample data. Must contain columns "name" and "event".}

\item{pairwise}{Character string giving the column in \code{m} for which pairwise p values should be calculated. E.g. \code{pairwise = "forest_type"} will check what forest types differ significantly from each other, not just if significant differences exist between forest types. Should only be used for categorical variables such as forest type, trap etc; not numeric such as rainfall. If NULL, only checks if significant differences exist, does not do pairwise checks.}

\item{family}{Probability distribution used to fit the model. Passed to \code{\link[=manyglm]{manyglm()}}. In general, this will be "negative.binomial" or "poisson".}

\item{...}{Other parameters passed to \code{\link[=anova.manyglm]{anova.manyglm()}}, and \code{\link[=summary.manyglm]{summary.manyglm()}}. These will override any default parameters. In general, there will be little need to adjust anything except \code{nBoot} (number of resamples). It is nine by default, to give a quick but approximate result. More accurate results will need e.g. \code{nBoot=499}, which can take hours to finish.}
}
\value{
List with items:
\itemize{
\item fit Fitted values for each sample and taxon.
\item coefficients Coefficients of the fitted model for each taxon and variable. See "Details" for how to interpret these.
\item p Vector of p values for each variable of the model. See "Details" for how to interpret these.
\item p_sp Matrix of p values for each variable of the model, separately for each taxon. See "Details" for how to interpret these.
\item p_pairwise Matrix of p values for pairwise differences between the levels of the model variable given by \code{pairwise}. E.g. for forest type, p values for primary versus disturbed, primary versus clearcut etc.
\item p_pairwise_sp List of matrixes of p values for pairwise differences between the levels of the model variable given by \code{pairwise}. One list item for each taxon. E.g. for forest type, p values for primary versus disturbed, primary versus clearcut etc.
\item mg Fitted model as a \code{\link[=manyglm]{manyglm()}} object. This is what \code{fit} and \code{coefficients} are extracted from.
\item anova Results of \code{\link[=anova.manyglm]{anova.manyglm()}}. This is what \code{p} is extracted from.
\item summaries List of results of \code{\link[=summary.manyglm]{summary.manyglm()}}, which is called multiple times to get pairwise comparisons. This is what \code{p_pairwise} and \code{p_pairwise_sp} are extracted from.
}
}
\description{
Fit generalised linear models to the wasp catches, and get the p values for whether forest type, rain etc affect the catch of each species. This is basically a wrapper for the package \link{mvabund}, functions \code{\link[=manyglm]{manyglm()}}, \code{\link[=anova.manyglm]{anova.manyglm()}}, and \code{\link[=summary.manyglm]{summary.manyglm()}}. The analyses take a long time to finish, anything from some minutes (\code{nBoot=9}) to hours (\code{nBoot=499})!
}
\details{
This function is in effect a translator for the \link{mvabund} package: it converts the wasp data to the format mvabund expects, does the analyses, then extracts the relevant results in a readable format. Although much easier to use than standard mvabund, the returned results are still a bit on the complex side.

The fitted model is returned as list item \code{coefficients}. For a model like "offset(tdiff_log) + rain + forest_type" (and default settings), you get the predicted number of wasps by counting: tdiff * \code{exp( c1 + c2 * rain + c3(forest_type) )}.

The basic p values are returned in list item \code{p}. If e.g. the p value of forest type is significant, there is some significant difference in the number of wasps caught between forest types. Look at \code{p_pairwise} to see which forest types differ, and \code{p_sp} to see which taxa have significant differences. (neither forest types nor taxa necessarily show anything, the basic p values are for everything pooled and are better at detecting differences)

In general, the p values should be treated with a pinch of salt. Any real differences will be visible in plots of the wasp catches, and/or in the fitted model. P values are an additional confirmation that the differences are likely true. For really rare species especially, the p values are likely to be nonsense (due to the nature of resampling). P values are a good tool for interpreting results, not the end goal!
}
\examples{
# get example wasp data
f = system.file("extdata", "wasps_example.csv", package = "turkuwasps", mustWork = TRUE)
wasps = read_wasps(f)

# remove damaged samples and their wasps
tmp = ecology_usable(wasps)
x = tmp$wasps
m = tmp$samples

\dontrun{

# fit model and get p values (only do three resamples to save time)
model = "offset(tdiff_log) + days + rain + forest_type + deadwood"
a = resample(model, x, m, pairwise="forest_type", nBoot=3)

# show coefficients of the fitted model
a$coefficients

# show which variables affected wasp catches
a$p

# show which forest types differed from each other in number of wasps caught
a$p_pairwise

}

}
