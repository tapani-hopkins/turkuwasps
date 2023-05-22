#' Fit glm and analyse it
#'
#' Fit generalised linear models to the wasp catches, and get the p values for whether forest type, rain etc affect the catch of each species. This is basically a wrapper for the package [mvabund], functions [manyglm()], [anova.manyglm()], and [summary.manyglm()]. The analyses take a long time to finish, anything from some minutes (`nBoot=9`) to hours (`nBoot=499`)!
#'
#' @param model Character string giving the model to fit. Generally something like "offset(tdiff_log) + days + rain + forest_type + deadwood". The variables should be found as column names in `m`. Converted to [formula], and will accept e.g. interaction terms in the same format as for formulae.
#' @param x Data frame with the wasp data. Must contain columns "sample" and "taxon". 
#' @param m Data frame with the Malaise sample data. Must contain columns "name" and "event".
#' @param pairwise Character vector of the columns in `m` for which pairwise p values should be calculated. E.g. `pairwise = "forest_type"` will check what forest types differ significantly from each other, not just if significant differences exist between forest types. Should only be used for categorical variables such as forest type, trap etc; not numeric such as rainfall. If NULL, only checks if significant differences exist. 
#' @param family Probability distribution used to fit the model. Passed to [manyglm()]. In general, this will be "negative.binomial" or "poisson".
#' @param ...  Other parameters passed to [anova.manyglm()], and [summary.manyglm()]. These will override any default parameters. In general, there will be little need to adjust anything except `nBoot` (number of resamples). It is nine by default, to give a quick but approximate result. More accurate results will need e.g. `nBoot=499`, which can take several hours to finish.  
#' 
#' @return List with items:
#' * anova Results of [anova.manyglm()]
#' * fit Fitted values for each sample and taxon.
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
	
	#
	a = do.call(mvabund::anova.manyglm, args=analysis_args)
	
	# xxx works if pairwise is one variable, not vector. Move to helper function!
	if (! is.null(pairwise)){
		
		# make a copy of the sample data and analysis arguments
		M = m
		args2 = analysis_args
		
		# create empty list to save analysis results in
		summaries = list()
		
		# get the levels of the variable (e.g. forest types, sites..)
		levs = levels0(m[, pairwise])
		
		# run analyses several times, each time comparing a different level to the others
		# (ignore last level, since that will already have been compared to the others)
		for (i in levs[-length(levs)]){
			
			# compare level `i` to the others (by making it the first factor level)
			M[, pairwise] = relevel0(m[, pairwise], i)
			
			# fit the model to the data and save in arguments
			args2$object = mvabund::manyglm(model, data=M, family=family)
			
			# analyse and add analysis results to list
			s = do.call(mvabund::summary.manyglm, args=args2)
			summaries = c(summaries, list(s))
			
		}
		
			
		# xxx extract p values and perhaps fitted values from summaries
		
	}
	
	#a = anova(fit, test = "LR", resamp="pit.trap", p.uni="unadjusted", nBoot=9)
	#s = summary(mg1, test = "LR", resamp="pit.trap", p.uni="unadjusted", nBoot=9)
	
	# xxx modify these!!
	return(list(anova=a, fit=fit$fitted.values))
	
}
