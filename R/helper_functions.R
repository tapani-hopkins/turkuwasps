#' Get default bar order and bar colours for barplots
#'
#' This is a helper function used by [plot_place()]. Gives defaults for what bars to include in the barplot, what order to place them in, and what colour to use.
#'
#' @param x Vector of which trap, forest type or other location each wasp came from. Either as strings or factor.
#'
#' @return List with items `x` (locations converted to factor, with the factor levels telling what bars to plot) and `colour` (the colours to give each bar). 
#'
get_defaults = function(x){
	
	# get defaults from "forest_type" if the locations are forest types..
	if(all(x %in% turkuwasps::forest_type$name)){	
		d = turkuwasps::forest_type	
			
	# ..get defaults from "trap" if the locations are traps..		
	} else if (all(x %in% turkuwasps::trap$name)) {
		d = turkuwasps::trap
		
	# ..stop if it is unclear what the locations are
	} else {
		stop("Could not figure out if the locations were traps or forest types. Check that they are either all traps or all forest types, and that every one is valid.")
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
