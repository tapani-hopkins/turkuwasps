#' Fit glm and analyse it
#'
#' Fit generalised linear models to the wasp catches, and get the p values for whether forest type, rain etc affect the catch of each species. This is basically a wrapper for the package [mvabund], functions [manyglm()], [anova.manyglm()], and [summary.manyglm()]. The analyses take a long time to finish, anything from some minutes (`nBoot=9`) to hours (`nBoot=499`)!
#'
#' @param model Character string giving the model to fit. Generally something like "offset(tdiff_log) + days + rain + forest_type + deadwood". The variables should be found as column names in `m`. Converted to [formula], and will accept e.g. interaction terms in the same format as for formulae.
#' @param x Data frame with the wasp data. Must contain columns "sample" and "taxon". 
#' @param m Data frame with the Malaise sample data. Must contain columns "name" and "event".
#' @param pairwise Character string giving the column in `m` for which pairwise p values should be calculated. E.g. `pairwise = "forest_type"` will check what forest types differ significantly from each other, not just if significant differences exist between forest types. Should only be used for categorical variables such as forest type, trap etc; not numeric such as rainfall. If NULL, only checks if significant differences exist, does not do pairwise checks. 
#' @param family Probability distribution used to fit the model. Passed to [manyglm()]. In general, this will be "negative.binomial" or "poisson".
#' @param ...  Other parameters passed to [anova.manyglm()], and [summary.manyglm()]. These will override any default parameters. In general, there will be little need to adjust anything except `nBoot` (number of resamples). It is nine by default, to give a quick but approximate result. More accurate results will need e.g. `nBoot=499`, which can take hours to finish. If you only want to fit the model, without calculating p values, set `nBoot=0`.
#' 
#' @details This function is in effect a translator for the [mvabund] package: it converts the wasp data to the format mvabund expects, does the analyses, then extracts the relevant results in a readable format. Although much easier to use than standard mvabund, the returned results are still a bit on the complex side.
#' @details The fitted model is returned as list item `coefficients`. For a model like "offset(tdiff_log) + rain + forest_type" (and default settings), you get the predicted number of wasps by counting: tdiff * `exp( c1 + c2 * rain + c3(forest_type) )`.
#' @details The basic p values are returned in list item `p`. If e.g. the p value of forest type is significant, there is some significant difference in the number of wasps caught between forest types. Look at `p_pairwise` to see which forest types differ, and `p_sp` to see which taxa have significant differences. (neither forest types nor taxa necessarily show anything, the basic p values are for everything pooled and are better at detecting differences)
#' @details In general, the p values should be treated with a pinch of salt. Any real differences will be visible in plots of the wasp catches, and/or in the fitted model. P values are an additional confirmation that the differences are likely true. For really rare species especially, the p values are likely to be nonsense (due to the nature of resampling). P values are a good tool for interpreting results, not the end goal!
#'
#' @return List with items:
#' * fit Fitted values for each sample and taxon.
#' * coefficients Coefficients of the fitted model for each taxon and variable. See "Details" for how to interpret these.
#' * p Vector of p values for each variable of the model. See "Details" for how to interpret these.
#' * p_sp Matrix of p values for each variable of the model, separately for each taxon. See "Details" for how to interpret these.
#' * p_pairwise Matrix of p values for pairwise differences between the levels of the model variable given by `pairwise`. E.g. for forest type, p values for primary versus disturbed, primary versus clearcut etc. 
#' * p_pairwise_sp List of matrixes of p values for pairwise differences between the levels of the model variable given by `pairwise`. One list item for each taxon. E.g. for forest type, p values for primary versus disturbed, primary versus clearcut etc. 
#' * mg Fitted model as a [manyglm()] object. This is what `fit` and `coefficients` are extracted from.
#' * anova Results of [anova.manyglm()]. This is what `p` is extracted from.
#' * summaries List of results of [summary.manyglm()], which is called multiple times to get pairwise comparisons. This is what `p_pairwise` and `p_pairwise_sp` are extracted from.
#' 
#' @examples
#' # get example wasp data
#' f = system.file("extdata", "wasps_example.csv", package = "turkuwasps", mustWork = TRUE)
#' wasps = read_wasps(f)
#' 
#' # remove damaged samples and their wasps
#' tmp = ecology_usable(wasps)
#' x = tmp$wasps
#' m = tmp$samples
#' 
#' \dontrun{
#'
#' # fit model and get p values (only do three resamples to save time)
#' model = "offset(tdiff_log) + days + rain + forest_type + deadwood"
#' a = resample(model, x, m, pairwise="forest_type", nBoot=3)
#' 
#' # show coefficients of the fitted model
#' a$coefficients
#'
#' # show which variables affected wasp catches
#' a$p
#'
#' # show which forest types differed from each other in number of wasps caught
#' a$p_pairwise
#'
#' }
#'
#' @export
resample = function(model, x, m, pairwise=NULL, family="negative.binomial", ...){
	
	# store various default arguments for the analysis
	analysis_args = list(
		nBoot = 9,
		p.uni="unadjusted", 
		resamp="pit.trap", 
		test = "LR"
	)
	
	# add the analysis arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	analysis_args[names(user_args)] = user_args
	
	# warn if not all variables are found in `m`
	model_terms = all.vars(stats::as.formula(paste("~", model)))
	if (! all(model_terms %in% colnames(m))){
		warning("Some of the variables of the model were not found in `m`. Resample may not work as expected.")
	}
	
	# store the model in the format expected by manyglm
	model = stats::as.formula(paste("mv ~", model))	
	
	# only use samples from the collecting event(s) the wasps come from
	m = filter_samples(x$sample, m)
	
	# make sure that all samples are counted (not just those that caught wasps)
	x$sample = factor(x$sample, levels=m$name)
	
	# count the number of wasps of each species caught in each sample
	mv = table(x$sample, x$taxon)

	# fit the model to the data
	fit = mvabund::manyglm(model, data=m, family=family)
	
	# add the fitted model to the analysis arguments
	analysis_args$object = fit
	
	# get p values unless nBoot is zero..
	if (! analysis_args$nBoot == 0){
		
		# test which variables had a significant effect on wasp catches
		a = do.call(mvabund::anova.manyglm, args=analysis_args)
	
		# extract the p values (both overall and for each taxon)
		p = a$table[, 4, drop=F]
		p_sp = a$uni.p
		attributes(p_sp)$title = NULL
	
		# if `pairwise` was given, test for differences between the levels of that variable..
		if (! is.null(pairwise)){
		
			# test which levels (e.g. forest types) differed significantly from each other
			summaries = get_summaries(m, pairwise, model, family, analysis_args)	
		
			# extract the p values (both overall and for each taxon)
			p_pairwise = get_p(summaries, pairwise, levels0(m[, pairwise]))
			if (ncol(mv) > 1){
				p_pairwise_sp = get_p_sp(summaries, pairwise, levels0(m[, pairwise]))
			} else {
				p_pairwise_sp = NULL
			}
		
		# ..if `pairwise` was not given, return blank variables for pairwise comparisons
		} else {
			p_pairwise <- p_pairwise_sp <- summaries <- NULL
		}
	
	# if nBoot is zero, save time by making the p values NULL
	} else {
		a <- p <- p_sp <- p_pairwise <- p_pairwise_sp <- summaries <- NULL
	}
	
	# fix a problem where manyglm drops sample names if there's just one species
	if (ncol(mv) == 1){
		
		# drop the samples from `mv` for which there is missing data in one or more of the model terms
		i = which(is.na(m[, model_terms]), arr.ind=TRUE)[, 1]
		mv2 = mv[-i, , drop=FALSE]
		
		# get the sample names and species name from mv2 (they *should* be in the same order..)
		dimnames(fit$fitted.values) = dimnames(mv2)
	}
	
	# get the fitted values, including for any samples which manyglm dropped
	fitted_values = include_na(fit$fitted.values, m)
	
	# return
	return(list(fit=fitted_values, coefficients=fit$coefficients, p=p, p_sp=p_sp, p_pairwise=p_pairwise, p_pairwise_sp=p_pairwise_sp, mg=fit, anova=a, summaries=summaries))
	
}

#' Include samples dropped by manyglm
#'
#' Helper function used by [resample()]. Finds out what samples, if any, were dropped by manyglm, and returns them to the manyglm results as NA values. Manyglm will drop any samples which return NA, for example samples for which there is no rain data if rain is included in the model, which will often mess up further analyses unless fixed.
#'
#' @param f Results returned by [manyglm()] when fitting a model to the data. A matrix with each sample on a separate row. Typically the fitted values.
#' @param m Data frame with the Malaise sample data. Must contain column "name" which gives the names of the samples.
#' 
#' @return Matrix 'f' with the samples dropped by manyglm added. Same number of rows as 'm', and with the samples in the same order.
#'
#' @keywords internal
#'
include_na = function(f, m){
	
	# get the samples which are not in the manyglm results
	i = which(! m$name %in% rownames(f))
	newnames = c(rownames(f), m$name[i])

	# add one row of NA for each missing sample to the manyglm results
	add = matrix(NA, nrow=length(i), ncol=ncol(f))
	f = rbind(f, add)
	rownames(f) = newnames
	
	# sort into the same order as 'm'
	o = match(m$name, rownames(f))
	f = f[o, , drop=FALSE]
	
	# return
	return(f)
	
}


#' Add modelled catches to plots of wasps per place
#'
#' Get the modelled wasp catches per place from the fitted values returned by [resample()], and (optionally) add them to a plot. Typically used after plotting the number of wasps caught in each place with [plot_place()], to add a line to the plot with the model predictions.
#'
#' @param f Matrix of fitted values returned by [resample()], one row for each sample and one column for each taxon.
#' @param by Character vector giving the place (generally trap or forest type) each sample was collected. Same length as the number of rows in 'f'. 
#' @param x  x coordinates to use when adding the modelled catch of each place to the plot. Can be a vector, matrix (with taxa in rows and places in columns), or a list of coordinates returned by [plot_place()]. See 'Details' for what happens if 'x' is a matrix with more than one row (i.e. more than one taxon). If not given, default x coordinates will be used.
#' @param tdiff Numeric vector giving the sampling effort of each sample, in trap days. Same length as as the number of rows in 'f'. If NULL, the sampling efforts will be got from the package's internal sample data, by comparing to the row names of 'f'. If NA, results won't be scaled by sampling effort.
#' @param plot If TRUE, the modelled catches of each place will be added to the plot as lines + points. If FALSE, the modelled catches will only be returned, not plotted.
#' 
#' @details If the x coordinates are given as a matrix with each species on its own row, the assumption is each species should be plotted separately. The function will count the wasp catches and x coordinates of species 1, then species 2 etc, and return wasp catch and x coodinate vectors which have the species' values pasted together (e.g. wasp catch vector has species 1 wasp catches, followed by species 2 wasp catches..). This matches how [plot_place()] places the bars when there is more than one species plotted.
#' Usually the x coordinates are a vector, or a matrix with just one row. Then the species will be lumped together = the fitted values of the different species in 'f' will be combined.
#' Typically, 'x' will be the list returned by [plot_place()]. Then the plotting of modelled values will automatically match the plot; the x coordinates will be extracted from the list as a matrix with either one or several rows.
#'
#' @return List with two items, returned invisibly:
#' * x Vector of x coordinates of each place. 
#' * y Total number of wasps for each place as estimated by the model. Scaled by sampling effort if 'tdiff' was given.
#' 
#' @examples
#' # get example wasp data
#' f = system.file("extdata", "wasps_example.csv", package = "turkuwasps", mustWork = TRUE)
#' wasps = read_wasps(f)
#' 
#' # remove damaged samples and their wasps
#' tmp = ecology_usable(wasps)
#' x = tmp$wasps
#' m = tmp$samples
#' 
#' # fit model, with zero resamples since we don't need p values
#' model = "offset(tdiff_log) + days + rain + forest_type + deadwood"
#' a = resample(model, x, m, nBoot=0)
#' 
#' # plot how many wasps were caught in each trap versus how many the model predicts
#' coords = plot_place(x$trap, m=m)
#' default_legend("forest_type", "Uganda 2014-2015")
#' plot_modelled_place(a$fit, m$trap, x=coords)
#' 
#' # plot how many wasps were caugh and modelled in each forest type, each species separate
#' coords = plot_place(x$forest_type, m=m, taxon=x$taxon)
#' plot_modelled_place(a$fit, m$forest_type, x=coords)
#'
#' @export
plot_modelled_place = function(f, by, x=NULL, tdiff=NULL, plot=TRUE){
	
	# if x coordinates weren't given, use the sequence 1,2,3..
	if (is.null(x)){
		x = 1:nlevels0(by)
	}
	
	# if 'x' was returned by plot_place(), extract the x coordinates
	if (is.list(x)){
		x = x$x
	}
	
	# sort the places (or rather, their factor levels) to default order
	by = default_order(by)
	
	# double-check that the places are in the same order as in the plot
	if (! is.null(colnames(x))){
		xnames = colnames(x)
		bynames = levels0(by)
		if (! identical(xnames, bynames[bynames %in% xnames])){
			warning("The results may be in the wrong order. The names of the x coordinates either do not match the names of the places, or are in a different order to the names of the places as given by 'levels0(by)'.")
		}
	}
	
	# check that by is the right length
	if (nrow(f) != length(by)){ stop("'by' is the wrong length") }	
	
	# check that tdiff is either the right length, or is NULL or NA 
	if (! length(tdiff) == nrow(f) & ! is.null(tdiff) & ! is_na(tdiff)) {
	#	if (length(tdiff) == 1) {
	#		if (! is.na(tdiff)) { stop("'tdiff' is the wrong length") }
	#	} else {
			stop("'tdiff' is the wrong length")
	#	}
	}	
		
	# get the number of wasps in each place, for all species combined unless 'x' is a multirow matrix..
	if (! is_multirow(x)){
			
		# combine the species
		f = rowSums(f)
		
		# get the number of wasps and sampling effort for each place
		n = get_modelled_place(f, by, tdiff)
		wasps = n$wasps
		tdiff_wasps = n$tdiff_wasps
		
		
	# .. if 'x' is a multirow matrix, get the number of wasps in each place for each species separately
	} else {
			
		# create empty vectors for the wasps and sampling effort	
		wasps = NULL
		tdiff_wasps = NULL
		
		# add the numbers of wasps and sampling efforts one species at a time
		for (sp in 1:nrow(x)){
			
			# get the number of wasps and sampling effort of this species
			n = get_modelled_place(f[, sp], by, tdiff)
			wasps = c(wasps, n$wasps)
			tdiff_wasps = c(tdiff_wasps, n$tdiff_wasps)
		
		}

	}
	
	# make sure x coordinates are a vector
	x = as.vector(t(x))

	# scale by sampling effort (does nothing if tdiff was NULL)
	y = wasps / tdiff_wasps
	
	# add to the plot if asked to do so
	if (plot){
		graphics::lines(x, y)
		graphics::points(x, y, cex=0.5, pch=20)
	}
	
	# return invisibly
	invisible(list(x=x, y=y))

}


#' Add modelled catches to plots of wasps over time
#'
#' Get the modelled wasp catches over time from the fitted values returned by [resample()], and (optionally) add them to a plot. Typically used after plotting the number of wasps caught each day with [plot_time()], to add a line to the plot with the model predictions. Currently only gives the predicted number of wasps per trap day for each day, cannot be used for plots that are not scaled down by sampling effort.
#'
#' @param f Matrix of fitted values returned by [resample()], one row for each sample and one column for each taxon.
#' @param xlim Interval object giving the x limits of the plot. Typically as returned by [plot_time()]. If given, the modelled catches will be added as a line to the existing plot. If not, the modelled catches will only be returned, not plotted.
#' @param mdate Interval object giving the sample start and end dates. Same length as as the number of rows in 'f'. If NULL, the start and end dates will be got from the package's internal sample data, by comparing to the row names of 'f'.
#' @param tdiff Numeric vector giving the sampling effort of each sample, in trap days. Same length as as the number of rows in 'f'. If NULL, the sampling efforts will be got from the package's internal sample data, by comparing to the row names of 'f'.
#'
#' @return List with two items, returned invisibly:
#' * x Vector of datetimes (see [as.datetime()]). In a sequence at one-day intervals, giving the end datetimes of the daily averages of wasp catches.
#' * y Numeric vector. Daily averages of the modelled wasp catches.
#' 
#' @examples
#' # get example wasp data
#' f = system.file("extdata", "wasps_example.csv", package = "turkuwasps", mustWork = TRUE)
#' wasps = read_wasps(f)
#' 
#' # remove damaged samples and their wasps
#' tmp = ecology_usable(wasps)
#' x = tmp$wasps
#' m = tmp$samples
#' 
#' # fit model, only do one resample since we don't need p values
#' model = "offset(tdiff_log) + days + rain + forest_type + deadwood"
#' a = resample(model, x, m, nBoot=1)
#' 
#' # get the start and end of when each wasp was collected
#' waspdates = as.interval(x$start, x$end)
#'
#' # plot how many wasps were caught each day
#' z = plot_time(waspdates, m, x$taxon, ylab="wasps / trap day")
#' 
#' # add a line showing how many wasps each day the fitted model predicts
#' plot_modelled_time(a$fit, xlim=z$xlim)
#'
#' @export
plot_modelled_time = function(f, xlim=NULL, mdate=NULL, tdiff=NULL){
	
	# combine the columns (species) of the fitted data
	f = rowSums(f)
	
	# if mdate wasn't given, get the start and end dates of the samples from the package's sample data
	if (is.null(mdate)){
		m = turkuwasps::malaise_sample
		s = m[m$name %in% names(f), "start"]
		e = m[m$name %in% names(f), "end"]
		mdate = as.interval(s, e)
	}
	
	# if tdiff wasn't given, get the sampling efforts of the samples from the package's sample data
	if (is.null(tdiff)){
		m = turkuwasps::malaise_sample
		tdiff = m[m$name %in% names(f), "tdiff"]
	}
	
	# count the daily average of how many wasps were caught in the fitted data
	wasps = smooth_data(f, mdate, k=1, p=0)
	
	# count the daily average of the sampling effort
	tdiff = smooth_data(tdiff, mdate, k=1, p=0)
	
	# get the daily average of how many wasps per trap day the fitted model predicts
	y = wasps$y / tdiff$y
	
	# get the corresponding end dates
	x = wasps$x
	
	# add to the plot if asked to do so
	if (! is.null(xlim)){
		graphics::lines(x=x, y=y, xlim=xlim)
	}
	
	# return invisibly
	invisible(list(x=x, y=y))
	
}


#' Get the modelled number of wasps caught in each place
#'
#' Helper function used by [plot_modelled_place()]. Gets the modelled number of wasps caught in each place, and the sampling efforts of each place if asked to scale by sampling effort.
#'
#' Any samples which couldn't be fitted by the model (NA value in 'f') will be ignored.
#'
#' @param f Vector of fitted values returned by [resample()], one value for each sample. Should be a vector, i.e. the matrix returned by [resample()] should have every colum (i.e. species) added together, or just one species selected from the matrix.
#' @param by Character vector giving the place (generally trap or forest type) the sample was collected. Same length as 'f'. 
#' @param tdiff Numeric vector giving the sampling effort of each sample, in trap days. Same length as as the number of rows in 'f'. If NULL, the sampling efforts will be got from the package's internal sample data, by comparing to the row names of 'f'. If NA, results won't be scaled by sampling effort (the function will return a sampling effort of 1).
#' 
#' @return List with two items:
#' * wasps Total number of wasps in each place as estimated by the fitted model. Vector.
#' * tdiff_wasps Total sampling effort in trap days of each place. Vector. If 'tdiff' was NULL, this will be a vector of ones.
#'
#' @keywords internal
#'
get_modelled_place = function(f, by, tdiff=NULL){
	
	# if tdiff wasn't given, get the sampling efforts of the samples from the package's sample data
	if (is.null(tdiff)){
		m = turkuwasps::malaise_sample
		tdiff = m[m$name %in% names(f), "tdiff"]
	}
	
	# ignore NA values
	i = which(! is.na(f))
	
	# get the number of wasps caught in each place
	wasps = sum_by(f[i], by[i])
	
	# get the sampling effort of each place, if asked to scale by sampling effort
	if (! is_na(tdiff)){
		tdiff_wasps = sum_by(tdiff[i], by[i])
	} else {
		tdiff_wasps = 1
	}	
	
	# return
	return(list(wasps=wasps, tdiff_wasps=tdiff_wasps))
	
}


#' Check if a variable is a matrix with several rows
#'
#' Helper function used by [plot_modelled_place()]. 
#'
#' @param x Variable to be checked. Typically a vector or matrix
#' 
#' @return TRUE if 'x' is a matrix with more than one row, otherwise FALSE. Matrixes that R calls by a different name (e.g. tables, 2D arrays etc) are counted as a matrix.
#'
#' @keywords internal
#'
is_multirow = function(x){
	
	multirow = FALSE
	if (is.array(x)){
		if (nrow(x) > 1){
			multirow = TRUE
		}
	}
	
	return(multirow)
	
}