# Miscellaneous functions used by other functions in the package, and not meant for the user.


#' Get default breaks for the x axis of plots of datetime
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


#' Filter samples by collecting event of wasps
#'
#' Helper function used by e.g. [resample()]. Filters samples based on the caught wasps. Only samples from the same collecting event(s) as the wasps are kept.
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

