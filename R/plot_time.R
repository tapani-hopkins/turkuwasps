#' Plot wasps over time
#'
#' Plot the number of wasps caught each day (or other time period). XXXXX
#'
#' @param x Vector of which trap, forest type or other location each wasp came from. Either as strings or factor. (If factor, and `defaults`=FALSE, one bar is plotted for each factor level, in the same order as the levels)
#' @param m Data frame with the Malaise sample data, if wanting to scale the bars by sampling effort. Must contain columns "tdiff" (sampling effort) and one of "forest_type", "site" or "trap" (whatever is being plotted). If NULL, bars show the number of  wasps without taking sampling effort into account.
#' @param taxon Vector giving the taxon of each wasp. If given, plots each taxon separately. (but see "Details" for what happens if `beside` is set to FALSE by the user.)
#' @param defaults If TRUE, uses default settings for what bars to show, and for the order and colour of the bars. For example, if `x` contains Ugandan traps, draws one bar for each Ugandan trap, in successional order (primary forest to farm), with primary forest in dark green, swamp in blue etc.
#' @param ...  Graphical parameters and other arguments passed to [barplot()]. These will override any default values (e.g. colours). Colours (argument `col`) are for each taxon if `taxon` was given, or for each bar if it was not. 
#'
#' @details The plotting of taxa is different if argument `beside` is set to FALSE. One bar will be shown for each location, with the bars split by taxon to show how many wasps of each taxon were caught. Default colours will not be used.  
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
#' # plot
#' plot_place(x$trap, m)
#' 
#' # plot each species separately
#' plot_place(x$forest_type, m, taxon=x$taxon)
#' 
#' @export
plot_time = function(x, m=NULL, taxon=NULL, xlim=as.interval(min(x), max(x)), step=3600*24, ...){
	
	# store various default arguments for the barplot
	barplot_args = list(
		axisnames = FALSE, 
		border = FALSE,
		space = 0, 
		xaxs = "i",
		xpd = FALSE,
		width = 1
	)
	
	# add the barplot arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	barplot_args[names(user_args)] = user_args
	
	# if no taxa were given, set all taxa to ""
	if (is.null(taxon)){
		taxon = rep("", length(x))
	}
	
	# create a sequence of time intervals that covers `xlim`
	d = seq(xlim$s, xlim$e, step)
	d = as.interval(d[2:length(d)] - step, d[2:length(d)])
	
	# get the number of taxa and their names
	ntaxa = length(levels0(taxon))
	taxa = levels0(taxon)

	# create a blank matrix to store the bar heights in
	height = matrix(
		nrow = ntaxa, 
		ncol = length(d), 
		dimnames = list(taxa, as.character(means(d)))
	)
	
	# count the number of wasps in each bar, for each taxon separately
	for (sp in 1:ntaxa){
		i = which(taxon == taxa[sp])
		height[sp, ] = sum_overlap(x[i], d)
	}
	
	# scale bars by number of traps in use if `m` was given
	if (! is.null(m)){
		
		# get the weights for each bar 
		weight = sum_overlap(as.interval(m$start, m$end), d, proportional=FALSE)
		weight = weight / step
		
		# scale bars by weight
		height = t(t(height) / weight)
	}
	
	# add the bar heights to the barplot arguments
	barplot_args$height = height
	
	# barplot
	xcoords = do.call(graphics::barplot, args=barplot_args)

	# return the bar positions and heights invisibly
	invisible(list(x=xcoords, y=height, d=d))
	
}