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
#' Helper function used e.g. by [default_legend()]. Gets the factor levels of a vector, even if the vector is not a factor. Basically a more readable wrapper for [levels()] and [factor()], equivalent to calling `levels(factor(x))`.
#'
#' @param x Vector whose factor levels are wanted.
#' @param ... Other arguments passed to [factor()].
#'
#' @return Character vector giving the levels of `x`. 
#'
levels0 = function(x, ...){
	
	# get factor levels
	res = levels(factor(x, ...))
	
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
