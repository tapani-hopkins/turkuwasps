#' Print text as italic
#'
#' Helper function used by [default_legend()]. Makes a character vector (typically species names) display in italic font. Exactly how this works is a bit of a mystery to me: R handles italics, bold etc rather unclearly.
#'
#' @param x Character vector which is to be converted to italic.
#'
#' @return Vector which when displayed in a plot, is in italics. 
#'
#' @keywords internal
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
#' Helper function used by [axis_datetime()]. Gets the default breaks for x axis tick marks. These are generally every three months, during the five collecting events.
#'
#' @return Vector of datetime objects. 
#'
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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


#' Relevel locations to default order
#'
#' Helper function. Gives defaults for what order to place traps, forest types or other locations. Factors a vector of locations so the factor levels are in default order.
#'
#' @param x Vector of traps, forest types or other locations. Either as strings or factor.
#'
#' @return Vector of locations, factored so that the factor levels are in default order. 
#'
#' @keywords internal
#'
default_order = function(x){
	
	# find out if the locations are forest types, sites or traps
	location = get_locationtype(stats::na.omit(x))	
	
	# get defaults from the appropriate data frame
	d = get_locationdata(location)
		
	# only include collecting events that are in `x`
	i = match(x, d$name)
	ii = which(d$event %in% d$event[i])
	d = d[ii, ]
		
	# store the locations and their default order, as factor levels of `x`
	x = factor(x, levels=d$name)
	
	# return
	return(x)
	
}


#' Get default x limits for plot_time()
#'
#' Helper function used by [plot_time()]. Gets the default x limits to use for the plot.
#'
#' @param x Vector of interval objects, telling when each wasp was collected.
#'
#' @return Interval object. 
#'
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @seealso [get_summaries()] which will typically be used to get the list given as parameter `summaries`.
#'
#' @keywords internal
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
#' @seealso [get_summaries()] which will typically be used to get the list given as parameter `summaries`.
#'
#' @keywords internal
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


#' Analyse pairwise differences with summary.manyglm
#'
#' Helper function used by [resample()]. Runs [summary.manyglm()] multiple times, to test for differences between all the levels of a categorical variable. E.g. if forest type is being analysed, analyses the differences between primary and disturbed forest, primary and clearcut, disturbed and clearcut, etc.. 
#'
#' @param m Data frame with the Malaise sample data. Must contain the columns given in `model`.
#' @param pairwise Name of the variable to get pairwise p values for. Character string. E.g. "forest_type". Must be in `model`.
#' @param model Character string or formula (see [as.formula()]) giving the model to fit. Generally something like "mv ~ offset(tdiff_log) + days + rain + forest_type + deadwood". The variables should be found as column names in `m`.
#' @param family Probability distribution used to fit the model. Passed to [manyglm()]. In general, this will be "negative.binomial" or "poisson".
#' @param analysis_args List of arguments to pass to [summary.manyglm()]. 
#'
#' @return List of results given by [summary.manyglm()]. Each result will be otherwise identical, except a different level of `pairwise` will be compared to. E.g. first result will compare primary forest to other forest types, second result disturbed forest to other forest types, etc. One list item for each level of `pairwise`, except the last (since that has already been compared to all the others).
#'
#' @seealso [get_p()] and [get_p_sp()] to convert the results to something more readable.
#'
#' @keywords internal
#'
get_summaries = function(m, pairwise, model, family, analysis_args){
	
		# make a copy of the sample data
		M = m
		
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
			analysis_args$object = mvabund::manyglm(model, data=M, family=family)
			
			# analyse and add analysis results to list
			s = do.call(mvabund::summary.manyglm, args=analysis_args)
			summaries = c(summaries, list(s))
			
		}
		
		# return
		return(summaries)
		
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
#' @keywords internal
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


#' Check if a variable is NA (just a single NA)
#'
#' Helper function. Checks if something is exactly NA. I.e. just a single NA, not e.g. a vector of NAs. Useful e.g. when `ìf(is.na(x))` returns an error because `x` may be a vector.
#'
#' @param x What to check.
#'
#' @return TRUE if `x` is a single NA value, otherwise FALSE.
#'
#' @keywords internal
#'
is_na = function(x){
	
	# return FALSE unless `x` is just a single NA value
	isna = FALSE
	if (length(x) == 1) {
		if (is.na(x)){
			isna = TRUE
		}
	}
	
	# return
	return(isna)
	
}


#' Add legend to extended scatterplot
#'
#' Helper function used by [plot_explore()]. Adds a legend showing the colours used in the grid cells.
#'
#' @param breaks Numeric vector giving the breakpoints between colours. (between 0 and 1)
#' @param col Vector of colours, of length length(breaks)-1.
#'
#' @keywords internal
#'
legend_explore = function(breaks, col){

	# get width and height of plot window
	width = graphics::par("usr")[2] - graphics::par("usr")[1]
	height = graphics::par("usr")[4] - graphics::par("usr")[3]

	# store the left and right limits of the legend
	xl = graphics::par("usr")[1] + width * 1.02
	xr = graphics::par("usr")[1] + width * 1.04

	# store the y coordinates of each rectangle of the legend
	b = (graphics::par("usr")[4] -  0.2 * height) - 0.6 * height * breaks
	yb = b[-length(b)]
	yt = b[-1]

	# draw rectangles, one for each colour
	graphics::rect(xl, yb, xr, yt, col=col, xpd=TRUE, lwd=0.1)
	
	# add labels "1" and "0"
	graphics::text(x=c(xr, xr), y=b[c(1, length(b))], labels=c(0, 1), pos=4, xpd=TRUE, cex=0.8)

}


#' Plot extended scatterplot
#'
#' Helper function used by [explore()]. Plots a scatterplot of e.g. rain on the x axis and number of wasps caught in each sample on the y axis. But also adds:
#' * A fitted line showing trends (e.g. decreasing catch the more rain there is).
#' * A grid coloured in according to how many points there are in each grid cell (i.e. a 2D histogram). The counts are scaled, see "Note". 
#'
#' @param x Numeric vector of the x positions of each point. E.g. the amount of rain of each sample.
#' @param y Numeric vector of the y positions of each point. Typically the number of wasps caught in each sample, or the number of wasps caught per day.
#' @param res Resolution of the grid. Default is to split the x and y axes into 15 grid lines, giving 14*14=196 grid cells.
#' @param ... Other arguments passed to [image()], which draws the grid cell colours. Will overwrite any default arguments such as `col`. (Argument `x` cannot be overwritten)
#'
#' @note The number of points in each grid cell is scaled before plotting, so that the colour of each grid cell shows the portion of points (of the column of cells) that are in the cell. E.g. if rain is on the x axis, then the cells of column 5 mm - 10 mm do not show how many points are in each cell; they show what portion of the points that are between rain=5mm and rain=10mm are in each cell. This makes it easier to compare different rainfall classes, even if some rainfalls have a much larger sample size (more points) than others.
#'
#' @keywords internal
#'
plot_explore = function(x, y, res=15, ...){
	
	# store number of colours to use
	ncolours = 10
	
	# store various default arguments for the plot
	plot_args = list(
		col = grDevices::colorRampPalette(c("#FFF9ED", "darkgreen"))(ncolours)
	)
	
	# add the plot arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	plot_args[names(user_args)] = user_args
	
	# get the x and y positions of the grid lines
	xbreaks = seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length=res)
	ybreaks = seq(min(y, na.rm=TRUE), max(y, na.rm=TRUE), length=res)

	# find out which grid cell each point falls in 
	# (e.g. xbox=3, ybox=2: 3rd cell in x direction, 2nd in y direction)
	xbox = findInterval(x, xbreaks, rightmost.closed=TRUE)
	ybox = findInterval(y, ybreaks, rightmost.closed=TRUE)

	# make sure all cells are counted by adding them to the factor levels
	xbox = factor(xbox, levels=1:(res-1))
	ybox = factor(ybox, levels=1:(res-1))

	# save the centre coordinates of each grid cell, and number of points in each cell
	coords = list(
		x = xbreaks[-1] - diff(xbreaks) / 2, 
		y = ybreaks[-1] - diff(ybreaks) / 2,
		z = table(xbox, ybox)
	)

	# scale the number of points
	coords$z = coords$z / rowSums(coords$z)
	
	# blank out any cells which have no points in them
	coords$z[coords$z == 0] = NA

	# add the coordinates to the plot arguments
	plot_args$x = coords
	
	# split colours among cells so that roughly the same number of cells share each colour
	plot_args$breaks = stats::quantile(coords$z, probs=seq(0, 1, length=ncolours+1), na.rm=TRUE)
	
	# plot
	do.call(graphics::image, args=plot_args)
	
	# add tick marks on the x axis at the grid lines
	graphics::axis(1, xbreaks, labels=FALSE, lwd=0.1)
	
	# add a legend
	legend_explore(plot_args$breaks, plot_args$col)
	
	# add the points, and a (rough) fitted line
	graphics::points(x, y, pch=20, cex=0.8)
	graphics::abline(stats::lm(y ~ x))

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
#' @keywords internal
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
