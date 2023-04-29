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
plot_time = function(x, m=NULL, taxon=NULL, xlim=NULL, step=3600*24, defaults=TRUE, ...){
	
	# if x limits weren't given, set to cover all of `x`
	if (is.null(xlim)){
		xlim = c(min(x$s), max(x$e))	
	}
	
	
	# create a sequence of time intervals that covers `xlim`
	d = seq(xlim[1], xlim[2], step)
	d = as.interval(d[2:length(d)] - step, d[2:length(d)])
	
	#
	height = sum_overlap(x, d)
	
	# 
	weight = sum_overlap(as.interval(m$start, m$end), d, proportional=FALSE)
	weight = weight / step
	height = height / weight
	
	# xx
	graphics::barplot(height, space=0, border=F, width=1, xaxs = "i")
}