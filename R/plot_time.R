#' Plot wasps over time
#'
#' Plot the number of wasps caught each day (or other time period). This is at heart function [barplot()], with one bar for each time period.
#'
#' @param x Vector of interval objects, telling when each wasp was collected.
#' @param m Data frame with the Malaise sample data, if wanting to scale by sampling effort (= number of traps in use at any given time). Must contain columns "start" and "end", giving start and end datetime of when each sample was collected. If NULL, the plot shows the total number of wasps collected per unit time, without taking into account how many traps were in use.
#' @param taxon Vector giving the taxon of each wasp. If given, shows each taxon in a different colour.
#' @param xlim Interval object (see [as.interval()]) giving the desired left and right limits of the plot. If not given, default limits given by [default_xlims()] are used.
#' @param step Length in seconds of the time periods for which to count the number of wasps. Default is to plot wasps at a resolution of 86400 seconds = one day.
#' @param xlabel If TRUE, the x axis is labelled (using default settings) by [label_plot_time()]. If FALSE, the x axis is left blank.
#' @param ...  Graphical parameters and other arguments passed to [barplot()], which handles the actual drawing. These will override any default values (e.g. colours, which by default come from [default_colours()]). Colours (argument `col`) are for each taxon, or if taxa were not given then just one colour should be given. Argument `xlim` is handled differently to standard [barplot()].
#'
#' @return A list giving the x and y coordinates of each bar (= time period `step` seconds long), returned silently. The list has three items:
#' * `x` The x coordinates of the centre of each bar, in plot coordinates. Use these to draw e.g. lines and further labels on the plot.
#' * `y` The height of each bar, in plot coordinates.
#' * `d` The start and end datetimes of each bar. 
#' * `xlim` The left and right limits of the plot as an interval object.
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
#' # get the start and end of when each wasp was collected
#' waspdates = as.interval(x$start, x$end)
#'
#' # plot
#' plot_time(waspdates, m, x$taxon, ylab="wasps / trap day")
#' 
#' # plot with modified x axis
#' tmp = plot_time(waspdates, m, x$taxon, xlabel=FALSE)
#' label_plot_time(tmp$xlim, step=3600*24, srt=270, format="%d %b %Y")
#' 
#' @export
plot_time = function(x, m=NULL, taxon=NULL, xlim=default_xlims(x), step=3600*24, xlabel=TRUE, ...){
	
	# if no taxa were given, set taxon of all wasps to ""
	if (is.null(taxon)){
		taxon = rep("", length(x))
	}
	
	# get the number of taxa and their names
	ntaxa = length(levels0(taxon))
	taxa = levels0(taxon)
	
	# store various default arguments for the barplot
	barplot_args = list(
		axisnames = FALSE, 
		border = FALSE,
		col = default_colours(ntaxa),
		space = 0, 
		xaxs = "i",
		xpd = FALSE,
		width = 1
	)
	
	# add the barplot arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	barplot_args[names(user_args)] = user_args
	
	# create a sequence of time intervals that covers `xlim`
	d = seq(xlim$s, xlim$e, step)
	d = as.interval(d[2:length(d)] - step, d[2:length(d)])

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
	
	# add default x labels if asked to do so
	if (xlabel){
		label_plot_time(xlim, step)
	}
	
	# return the bar positions and heights invisibly
	invisible(list(x=xcoords, y=height, d=d, xlim=xlim))
	
}