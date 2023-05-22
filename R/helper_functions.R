#' Print text as italic
#'
#' Helper function used by [default_legend()]. Makes a character vector (typically species names) display in italic font. Exactly how this works is a bit of a mystery to me: R handles italics, bold etc rather unclearly.
#'
#' @param x Character vector which is to be converted to italic.
#'
#' @return Vector which when displayed in a plot, is in italics. 
#'
as_italic = function(x){
	
	# create temporary function which makes *one* vector item italic
	to_italic = function(y){
		bquote(italic(.(y)))
	}
	
	# apply the temporary function to all items in the vector
	x = as.expression(lapply(x, to_italic))
	
	# return
	return(x)
	
} 


#' Get default breaks for label_plot_time()
#'
#' Helper function used by [label_plot_time()]. Gets the default breaks for x axis tick marks. These are generally every three months, during the five collecting events.
#'
#' @return Vector of datetime objects. 
#'
default_breaks = function(){
	
	# store default breaks
	breaks = as.datetime(c(
		"1998-08-01 00:00:00 UTC-05:00", "1998-11-01 00:00:00 UTC-05:00", "1999-02-01 00:00:00 UTC-05:00",
		"2000-01-01 00:00:00 UTC-05:00", "2000-04-01 00:00:00 UTC-05:00", "2000-07-01 00:00:00 UTC-05:00",
		"2000-10-01 00:00:00 UTC-05:00", "2001-01-01 00:00:00 UTC-05:00", 
		"2008-06-01 00:00:00 UTC-05:00", "2008-09-01 00:00:00 UTC-05:00",
		"2011-04-01 00:00:00 UTC-05:00", "2011-07-01 00:00:00 UTC-05:00", "2011-10-01 00:00:00 UTC-05:00",
		"2012-01-01 00:00:00 UTC-05:00", 
		"2014-09-01 00:00:00 UTC+03:00", "2014-12-01 00:00:00 UTC+03:00", "2015-03-01 00:00:00 UTC+03:00", 
		"2015-06-01 00:00:00 UTC+03:00", "2015-09-01 00:00:00 UTC+03:00"
	))
	
	# return
	return(breaks)
	
}


#' Get default bar order and bar colours for barplots
#'
#' Helper function used by [plot_place()]. Gives defaults for what bars to include in the barplot, what order to place them in, and what colour to use.
#'
#' @param x Vector of which trap, forest type or other location each wasp came from. Either as strings or factor.
#'
#' @return List with items `x` (locations converted to factor, with the factor levels telling what bars to plot) and `colour` (the colours to give each bar). 
#'
default_bars = function(x){
	
	# find out if the locations are forest types, sites or traps
	location = get_locationtype(x)	
	
	# get defaults from the appropriate data frame
	d = get_locationdata(location)
		
	# only include collecting events that are in `x`
	i = match(x, d$name)
	ii = which(d$event %in% d$event[i])
	d = d[ii, ]
		
	# store the locations and their default order, as factor levels of `x`
	x = factor(x, levels=d$name)
		
	# save default colours 
	colour=d$colour
	
	# return as list
	return(list(x=x, colour=colour))
	
}


#' Get default colours for taxa
#'
#' Helper function used by e.g. [plot_time()]. Gets the default colours used to plot different species. Gets the colours from a sequence of 30 colours (15 unique), which is recycled if more than 30 colours are needed.
#' 
#' @param n Number of colours needed. 
#'
#' @return Vector of colours in hexadecimal (e.g. "#00FFFF"), of length `n`. 
#'
default_colours = function(n){
	
	# use three colour gradients, 5 colours in each
	g1 = grDevices::colorRampPalette(c("darkgreen", "wheat1"))(5)
	g2 = grDevices::colorRampPalette(c("darkred", "yellow"))(5)
	g3 = grDevices::colorRampPalette(c("darkorchid4", "cyan"))(5)
	
	# place gradients in a sequence which cycles smoothly between dark and bright colours
	col = c(g1, rev(g2), g3, rev(g1), g2, rev(g3))
	
	# pick the desired number of colours
	col = rep(col, length.out=n)
	
	# return
	return(col)
	
}


#' Get default x limits for plot_time()
#'
#' Helper function used by [plot_time()]. Gets the default x limits to use for the plot.
#'
#' @param x Vector of interval objects, telling when each wasp was collected.
#'
#' @return Interval object. 
#'
default_xlims = function(x){
	
	# store default breaks in the x axis (≈ the start and end of each collecting event)
	xlims = as.datetime(c("1998-07-01 00:00:00 UTC-05:00", "1999-03-01 00:00:00 UTC-05:00", "2000-01-01 00:00:00 UTC-05:00", "2001-02-01 00:00:00 UTC-05:00", "2008-05-01 00:00:00 UTC-05:00", "2008-09-01 00:00:00 UTC-05:00", "2011-03-01 00:00:00 UTC-05:00", "2012-01-01 00:00:00 UTC-05:00", "2014-08-01 00:00:00 UTC+03:00", "2015-10-01 00:00:00 UTC+03:00"))
	
	# get last breakpoint before the wasps were collected
	s = xlims[xlims <= min(x)]
	s = s[length(s)]

	# get first breakpoint after the wasps were collected
	e = xlims[xlims >= max(x)]
	e = e[1]

	# save as interval
	xlim = as.interval(s, e)
	
	# return
	return(xlim)
	
}


#' Filter samples by collecting event of wasps
#'
#' Helper function used by [resample()]. Filters samples based on the caught wasps. Only samples from the same collecting event(s) as the wasps are kept.
#'
#' @param x Vector of which sample each wasp came from. Character or factor.
#' @param m Data frame with the Malaise sample data. Must contain columns "name" and "event".
#'
#' @return Data frame with the Malaise sample data. Only contains samples from the same collecting event(s) as the wasps came from.
#'
filter_samples = function(x, m){
		
	# find out what event(s) these wasps come from
	i = match(x, m$name)
	events = levels0(m$event[i])
		
	# get samples from those events
	i = which(m$event %in% events)
	m = m[i, ]
	
	# return
	return(m)
	
}


#' Get data for locations
#'
#' Helper function used by e.g. [default_bars()]. Gets the data for forest types, traps or other locations.
#'
#' @param x Name of the location type as string. Currently one of "forest_type", "site", "trap" or "sample".
#'
#' @return Data frame with the data of the location. 
#'
#' @seealso `data(package="turkuwasps")` for a list of all the datasets in the package.
#'
get_locationdata = function(x){
	
	# get the appropriate dataset
	d = switch(x, 
		"forest_type" = turkuwasps::forest_type,
		"site" = turkuwasps::site, 
		"trap" = turkuwasps::trap
	)
	
	# return
	return(d)
	
}


#' Find out if locations are forest types, sites or traps
#'
#' Helper function used by e.g. [default_bars()]. Checks what kind of locations a vector contains. Currently recognises forest types, sites and traps. Gives an error if some of the locations were not recognised, or different types of location are mixed.
#'
#' @param x Vector of traps, forest types or other locations. Either as strings or factor.
#'
#' @return Name of the location type as string. Currently one of "forest_type", "site" or trap". 
#'
get_locationtype = function(x){
	
	# check if the locations are forest types..
	if (all(x %in% turkuwasps::forest_type$name)){	
		loc = "forest_type"
	
	# .. or check if the locations are sites..		
	} else if (all(x %in% turkuwasps::site$name)) {
		loc = "site"
					
	# .. or check if the locations are traps..		
	} else if (all(x %in% turkuwasps::trap$name)) {
		loc = "trap"
		
	# ..stop if it is unclear what the locations are
	} else {
		stop("Could not figure out if the locations were traps or forest types. Check that they are either all traps or all forest types, and that every one is valid.")
	}	
	
	#return
	return(loc)	
	
}


#' Get overall p values from summary.manyglm
#'
#' Helper function used by [resample()]. Extracts the p values from the results given by [summary.manyglm()], and saves in a more convenient format. This extracts the p values between levels of a variable: e.g. if forest type has been analysed, gets the p values for differences between primary and disturbed forest, primary and clearcut etc. 
#'
#' @param summaries List of the results returned by [summary.manyglm()]. The results will all be the same, except that each results compares a different level (e.g. "forest") to the other levels (e.g. "disturbed", "clearcut" etc).
#' @param pairwise Name of the variable to get pairwise p values for. Character string. E.g. "forest_type".
#' @param levs Character vector giving all the levels of the variable. (e.g. "forest", "disturbed", etc)
#'
#' @return Matrix of the p values. Square matrix with row and column names giving the two levels that were compared. (e.g. to get the p value for a difference between forest and clearcut, check row "forest" and column "clearcut") 
#'
get_p = function(summaries, pairwise, levs){

	# create an empty matrix for overall p values
	p = matrix(NA, nrow=length(levs), ncol=length(levs))

	# name the columns and rows in same format as mvabund uses (e.g. "forest_typeprimary")
	dnames = paste0(pairwise, levs)
	dimnames(p) = list(dnames, dnames)


	# go through the p value tables and extract the overall p values
	for (i in 1:length(summaries)){
	
		# get the level that is being compared to (in mvabund format, e.g. "forest_typeprimary")
		lname = paste0(pairwise, levs[i])
	
		# get the table with overall p values
		results = summaries[[i]]$coefficients

		# get the positions of the table's p values in the matrix
		pos = match(rownames(results), dnames)

		# ignore NA values (these are the p values of other variables, i.e. not found in the matrix)
		not_na = !is.na(pos)
		pos = pos[not_na]

		# add p values to the correct places in the matrix
		p[lname, pos] = results[not_na, 2]
		p[pos, lname] = results[not_na, 2]

	}
	
	# tidy up the column and row names (e.g. "forest_typeprimary" >> "primary")
	rownames(p) = sub(pairwise, "", rownames(p))
	colnames(p) = sub(pairwise, "", colnames(p))
	
	# return
	return(p)
	
}


#' Get taxon p values from summary.manyglm
#'
#' Helper function used by [resample()]. Extracts the p values from the results given by [summary.manyglm()], and saves in a more convenient format. This extracts the p values between levels of a variable for each taxon separately: e.g. if forest type has been analysed, gets the p values for differences between primary and disturbed forest, primary and clearcut etc, for species 1, species 2 etc.. 
#'
#' @param summaries List of the results returned by [summary.manyglm()]. The results will all be the same, except that each results compares a different level (e.g. "forest") to the other levels (e.g. "disturbed", "clearcut" etc).
#' @param pairwise Name of the variable to get pairwise p values for. Character string. E.g. "forest_type".
#' @param levs Character vector giving all the levels of the variable. (e.g. "forest", "disturbed", etc)
#'
#' @return List of matrixes of the p values. One list item for each taxon, named with the taxon name. Each matrix is a square matrix with row and column names giving the two levels that were compared. (e.g. to get the p value for a difference between forest and clearcut, check row "forest" and column "clearcut") 
#' To get the p values from the list, type e.g. `p[["Epirhyssa quagga]]["primary", "clearcut"]`.
#'
get_p_sp = function(summaries, pairwise, levs){

	# create an empty matrix for p values
	p = matrix(NA, nrow=length(levs), ncol=length(levs))
	
	# name the columns and rows in same format as mvabund uses (e.g. "forest_typeprimary")
	dnames = paste0(pairwise, levs)
	dimnames(p) = list(dnames, dnames)
	
	# get the taxon names
	taxa = rownames(summaries[[1]]$uni.p)
	
	# create a list of matrixes, one for each taxon
	tmp = summaries[[1]]$uni.p
	p = rep(list(p), length(taxa))
	names(p) = taxa

	# go through the p value tables and extract the overall and species p values
	for (i in 1:length(summaries)){
	
		# get the level that is being compared to (in mvabund format, e.g. "forest_typeprimary")
		lname = paste0(pairwise, levs[i])
	
		# get the table with p values for each taxon
		results = summaries[[i]]$uni.p
		
		# get the positions of the table's p values in the matrix
		pos = match(colnames(results), dnames)
		
		# ignore NA values (these are the p values of other variables, i.e. not found in the matrix)
		not_na = !is.na(pos)
		pos = pos[not_na]
		
		# add p values to the correct places in each taxon's matrix
		for (sp in taxa){
			p[[sp]][lname, pos] = results[sp, not_na]
			p[[sp]][pos, lname] = results[sp, not_na]				
		}

	}
	
	# tidy up the column and row names (e.g. "forest_typeprimary" >> "primary")
	for (sp in 1:length(taxa)){
		rownames(p[[sp]]) = sub(pairwise, "", rownames(p[[sp]]))
		colnames(p[[sp]]) = sub(pairwise, "", colnames(p[[sp]]))
	}
	
	# return
	return(p)
	
}


#' Get the weights for scaling barplots
#'
#' Helper function for scaling the bars of [plot_place()]. Counts the total sampling effort of each bar (e.g. each trap), and tells how to scale the bars by sampling effort.
#'
#' @param barnames Vector of bar names for which the weights are desired. Used to find out if the bars are forest types, sites or traps, and to give the correct weights in the correct order for each bar. Must be valid forest type, site or trap names. Either as strings or factor.
#' @param m Data frame with the Malaise sample data. Used to get the sampling effort of each location. Must contain columns "tdiff" (sampling effort) and one of "forest_type", "site" or "trap" (whatever is being plotted).
#'
#' @return Vector of weights with which to multiply the bar heights. Named vector, in same order as the bars.
#'
get_weights = function(barnames, m){
	
	# check if the locations are forest types, sites or traps
	loc = get_locationtype(barnames)
	
	# get the weights of all the locations
	weight = 1 / sum_by(m$tdiff, m[, loc])
	
	# match up the weights with their corresponding bar
	i = match(barnames, names(weight))
	weight = weight[i]
	
	#return
	return(weight)	
	
}


#' Get factor levels
#'
#' Helper function used e.g. by [default_legend()]. Gets the factor levels of a vector. If the vector is not a factor, factors it first (i.e. equivalent to calling `levels(factor(x))`). Basically a more readable wrapper for [levels()] and [factor()].
#'
#' @param x Vector whose factor levels are wanted.
#' @param ... Other arguments passed to [factor()] if `x` is not already a factor.
#'
#' @return Character vector giving the levels of `x`. 
#'
#' seealso [nlevels0()]
#'
levels0 = function(x, ...){
	
	# get factor levels
	res = levels(x)
	
	# if there are no factor levels, factor `x` first
	if (is.null(res)){
		res = levels(factor(x, ...))
	}
	
	# return
	return(res)
}


#' Get number of factor levels
#'
#' Helper function used e.g. by [plot_place()]. Gets the number of factor levels of a vector. If the vector is not a factor, factors it first. Equivalent to [nlevels()], except handles vectors which are not a factor.
#'
#' @param x Vector whose number of factor levels is wanted.
#' @param ... Other arguments passed to [factor()] if `x` is not already a factor.
#'
#' @return Number of levels of `x`. 
#'
#' seealso [levels0()]
#'
nlevels0 = function(x, ...){
	
	# get number of factor levels
	res = length(levels0(x, ...))

	# return
	return(res)
}


#' Relevel levels of a factor or vector
#'
#' Helper function used e.g. by [resample()]. Moves one of a vector's factor levels to first place. If the vector is not a factor, factors it first. Equivalent to [relevel()], except handles vectors which are not a factor.
#'
#' @param x Vector.
#' @param ref Which level to move to first place. Usually character string.
#'
#' @return Vector `x` with the factor levels placed in a new order. 
#'
relevel0 = function(x, ref){
	
	# make sure `x` is a factor
	if (! is.factor(x)){
		x = factor(x)
	}
	
	# call default relevel
	res = stats::relevel(x, ref)
	
	# return
	return(res)
	
}


#' Add together all the numbers that belong to the same category
#'
#' Helper function used by [get_weights()]. Used to count the total sampling effort for each forest type, trap or other location. This is basically a wrapper for [aggregate()].
#'
#' @param x Vector of numbers to add together.
#' @param by Vector of same length as `x`, giving the category that each number belongs to
#' @param ... Other arguments passed to [aggregate()], which mostly passes them on to [sum()].
#'
#' @return Vector of the sums for each category. Named vector, categories are used as names. 
#'
sum_by = function(x, by, ...){
	
	# aggregate 
	m = stats::aggregate(x, list(by), FUN=sum, ...)
	
	# convert to vector
	X = m[, 2]
	names(X) = m[, 1]
	
	# return
	return(X)
	
}
