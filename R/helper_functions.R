#' Get default bar order and bar colours for barplots
#'
#' Helper function used by [plot_place()]. Gives defaults for what bars to include in the barplot, what order to place them in, and what colour to use.
#'
#' @param x Vector of which trap, forest type or other location each wasp came from. Either as strings or factor.
#'
#' @return List with items `x` (locations converted to factor, with the factor levels telling what bars to plot) and `colour` (the colours to give each bar). 
#'
get_defaults = function(x){
	
	# find out if the locations are forest types or traps
	location = get_locationtype(x)	
	
	# get defaults from "forest_type" if the locations are forest types..
	if(location == "forest_type"){	
		d = turkuwasps::forest_type	
			
	# ..get defaults from "trap" if the locations are traps	
	} else if (location == "trap") {
		d = turkuwasps::trap
	}
		
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


#' Find out if locations are forest types or traps
#'
#' Helper function used by e.g. [get_defaults()]. Checks what kind of locations a vector contains. Currently recognises forest types and traps. Gives an error if some of the locations were not recognised, or forest types and traps are mixed.
#'
#' @param x Vector of traps, forest types or other locations. Either as strings or factor.
#'
#' @return Name of the location type as string. Currently one of "forest_type" or trap". 
#'
get_locationtype = function(x){
	
	# check if the locations are forest types..
	if(all(x %in% turkuwasps::forest_type$name)){	
		loc = "forest_type"
				
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
#' @param barnames Vector of bar names for which the weights are desired. Used to find out if the bars are forest types or traps, and to give the correct weights in the correct order for each bar. Must be valid forest type or trap names. Either as strings or factor.
#' @param m Data frame with the Malaise sample data. Used to get the sampling effort of each location. Must contain columns "tdiff" (sampling effort) and one of "forest_type" or "trap" (whatever is being plotted).
#'
#' @return Name of the location type as string. Currently one of "forest_type" or trap". 
#'
get_weights = function(barnames, m){
	
	# check if the locations are forest types or traps
	loc = get_locationtype(barnames)
	
	# get the weights of all the locations
	weight = 1 / sum_by(m$tdiff, m[, loc])
	
	# match up the weights with their corresponding bar
	i = match(barnames, names(weight))
	weight = weight[i]
	
	#return
	return(weight)	
	
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