#' Fit glm and analyse it
#'
#' Fit generalised linear models to the wasp catches, and get the p values for whether forest type, rain etc affect the catch of each species. This is basically a wrapper for the package [mvabund], functions [manyglm()], [anova.manyglm()], and [summary.manyglm()]. The analyses take a long time to finish, anything from some minutes (`nBoot=9`) to hours (`nBoot=499`)!
#'
#' @param model Character string giving the model to fit. Generally something like "offset(tdiff_log) + days + rain + forest_type + deadwood". The variables should be found as column names in `m`. Converted to [formula], and will accept e.g. interaction terms in the same format as for formulae.
#' @param x Data frame with the wasp data. Must contain columns "sample" and "taxon". 
#' @param m Data frame with the Malaise sample data. Must contain columns "name" and "event".
#' @param pairwise Character string giving the column in `m` for which pairwise p values should be calculated. E.g. `pairwise = "forest_type"` will check what forest types differ significantly from each other, not just if significant differences exist between forest types. Should only be used for categorical variables such as forest type, trap etc; not numeric such as rainfall. If NULL, only checks if significant differences exist, does not do pairwise checks. 
#' @param family Probability distribution used to fit the model. Passed to [manyglm()]. In general, this will be "negative.binomial" or "poisson".
#' @param ...  Other parameters passed to [anova.manyglm()], and [summary.manyglm()]. These will override any default parameters. In general, there will be little need to adjust anything except `nBoot` (number of resamples). It is nine by default, to give a quick but approximate result. More accurate results will need e.g. `nBoot=499`, which can take hours to finish.  
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
		p_pairwise_sp = get_p_sp(summaries, pairwise, levels0(m[, pairwise]))
		
	# ..if `pairwise` was not given, return blank variables for pairwise comparisons
	} else {
		p_pairwise <- p_pairwise_sp <- summaries <- NULL
	}
	
	# return
	return(list(fit=fit$fitted.values, coefficients=fit$coefficients, p=p, p_sp=p_sp, p_pairwise=p_pairwise, p_pairwise_sp=p_pairwise_sp, mg=fit, anova=a, summaries=summaries))
	
}
