#' Plot wasps per place
#'
#' Barplot the number of wasps caught in each trap, site, or other location. This is basically a wrapper for two base R functions: [table()] for counting how many wasps belong to each bar, and [barplot()] to draw the plot.
#'
#' @param x Vector of which trap, site or other location each wasp came from. Either as strings or factor. (If factor, and `defaults`=FALSE, one bar is plotted for each factor level, in the same order as the levels)
#' @param taxon Vector giving the taxon of each wasp. If given, splits the bars to show how many wasps of each taxon were caught.
#' @param weights How much to scale each bar by. Vector of same length as the number of bars, or can also be a single number. Typically used to scale down bars by sampling effort.
#' @param defaults If TRUE, uses default settings for what bars to show, and for the order and colour of the bars. For example, if `x` contains Ugandan traps, draws one bar for each Ugandan trap, in successional order (primary forest to farm), with primary forest in dark green, swamp in blue etc.
#' @param ...  Graphical parameters and other arguments passed to [barplot()]. These will override any default values (e.g. colours). Colours (argument `col`) are for each taxon if `taxon` was given, or for each bar if it was not. 
#'
#' @return x-coordinates of the bars, returned silently (save these to variable to continue drawing on the barplot).
#' @export
plot_place = function(x, taxon=NULL, weights=1, defaults=TRUE, ...){

	# store various default arguments for the barplot
	barplot_args = list(
		cex.names = 0.7, 
		las = 2
	)
	
	# get default bar order and colour if asked to do so..
	if (defaults){
		
		# get default bars and add them as factor levels to `x`
		d = get_defaults(x)
		x = d$x
		
		# add default colours to barplot arguments, except if bars are split by taxon
		if (is.null(taxon)){
			barplot_args = c(barplot_args, list(col=d$colour))
		}
	
	# .. if not using default bars, make sure 'x' is a factor
	} else if(! is.factor(x)){
		x = factor(x)
	}
	
	# add the barplot arguments given by the user (overwrite any defaults with the same name)
	userpar = list(...)
	barplot_args[names(userpar)] <- userpar
		
	# if no taxa were given, get bar heights, by counting the wasps then scaling by `weights`..
	if(is.null(taxon)){
		height = table(x)
		height = height * weights
	
	# ..if taxa were given, get bar heights split by taxon, by counting the wasps then scaling by `weights`
	} else {
		height = table(x, taxon)
		height = t(height * weights)
	}
		
	# add the bar heights to the barplot arguments
	barplot_args = c(list(height=height), barplot_args)
	
	# barplot the wasps
	xcoords = do.call(graphics::barplot, args=barplot_args)
	
	# return the x coordinates of the bars invisibly
	invisible(xcoords)
}