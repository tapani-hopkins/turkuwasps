#' Plot wasps per place
#'
#' Barplot the number of wasps caught in each trap, forest type, or other location. This is basically a wrapper for two base R functions: [table()] for counting how many wasps belong to each bar, and [barplot()] to draw the plot.
#'
#' @param x Vector of which trap, forest type or other location each wasp came from. Either as strings or factor. (If factor, and `defaults`=FALSE, one bar is plotted for each factor level, in the same order as the levels)
#' @param m Data frame with the Malaise sample data, if wanting to scale the bars by sampling effort. Must contain columns "tdiff" (sampling effort) and one of "forest_type" or "trap" (whatever is being plotted). If NULL, bars show the number of  wasps without taking sampling effort into account.
#' @param taxon Vector giving the taxon of each wasp. If given, splits the bars to show how many wasps of each taxon were caught.
#' @param defaults If TRUE, uses default settings for what bars to show, and for the order and colour of the bars. For example, if `x` contains Ugandan traps, draws one bar for each Ugandan trap, in successional order (primary forest to farm), with primary forest in dark green, swamp in blue etc.
#' @param ...  Graphical parameters and other arguments passed to [barplot()]. These will override any default values (e.g. colours). Colours (argument `col`) are for each taxon if `taxon` was given, or for each bar if it was not. 
#'
#' @return x-coordinates of the bars, returned silently (save these to variable to continue drawing on the barplot).
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
#' #
#' plot_place(x$trap, m)
#' 
#' @export
plot_place = function(x, m=NULL, taxon=NULL, defaults=TRUE, ...){
	
	# store various default arguments for the barplot
	barplot_args = list(
		cex.names = 0.6, 
		las = 2
	)
	
	# get default bar order and colour if asked to do so..
	if (defaults){
		
		# get default bars and add them as factor levels to `x`
		d = get_defaults(x)
		x = d$x
		
		# add default colours to barplot arguments, except if bars are split by taxon
		if (is.null(taxon)){
			barplot_args["col"] = list(d$colour)
		}
	
	# .. if not using default bars, make sure 'x' is a factor
	} else if (! is.factor(x)){
		x = factor(x)
	}
	
	# get bar names
	barnames = levels(x)
	
	# add the barplot arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	barplot_args[names(user_args)] = user_args
		
	# if no taxa were given, get bar heights
	if (is.null(taxon)){
		
		# get bar heights by counting the wasps
		height = table(x)
		
		# scale by sampling effort if asked to do so
		if (! is.null(m)){
			weight = get_weights(barnames, m)
			height = height * weight
		}
	
	# ..if taxa were given, get bar heights split by taxon
	} else {
		
		# get bar heights by counting the wasps, split by taxon
		height = table(taxon, x)
		
		# scale by sampling effort if asked to do so
		if (! is.null(m)){
			weight = get_weights(barnames, m)
			height = t(t(height) * weight)
		}
	}
		
	# add the bar heights to the barplot arguments
	barplot_args["height"] = list(height)
	
	# barplot the wasps
	xcoords = do.call(graphics::barplot, args=barplot_args)
	
	# add the bar names to the x coordinates
	xcoords = as.vector(xcoords)
	names(xcoords) = barnames
	
	# check for special case where bar names must be repeated
	# (if beside=TRUE and there are taxa, causes multiple bars per location)
	mult = length(xcoords) / length(barnames)
	if (mult != 1){
		names(xcoords) = rep(barnames, each=mult)
	}
	
	# return the x coordinates of the bars invisibly
	invisible(xcoords)
}