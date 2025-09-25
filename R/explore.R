#' Explore how rain, temperature etc affected with wasp catches
#'
#' Plot extended scatterplots of wasp catches versus rain, temperature etc. Plot a scatterplot of e.g. rain on the x axis and number of wasps caught in each sample on the y axis. Then add:
#' * A fitted line showing trends (e.g. decreasing catch the more rain there is).
#' * A grid coloured in according to how many points there are in each grid cell (i.e. a 2D histogram). The counts are scaled columnwise, see "Notes". 
#'
#' @param x Vector of which sample each wasp came from. Chararacter string or factor. 
#' @param m Data frame with the Malaise sample data. Must contain columns "name" and "event", and all the columns given by `what` and `scale`.
#' @param what Character vector giving the name of the columns of `m` that are to be plotted. If NULL, plots all numeric columns (e.g. rain, temperature etc) except for the column given by `scale`.
#' @param scale Character string giving the name of the column to use for scaling wasp catches. Default is to show number of wasps per trap day on the y axis. If empty (scale="") or the column isn't in `m`, shows number of wasps caught, irrespective of whether the sample was collected during one day or two weeks.
#' @param res Resolution of the grid. Default is to split the x and y axes into 15 grid lines, giving 14*14=196 grid cells.
#' @param ... Other arguments passed to [image()], which draws the grid cell colours. Will overwrite any default arguments such as `col`. (Argument `x` of [image()] cannot be overwritten)
#'
#' @note The number of points in each grid cell is scaled columnwise before plotting. This means that the colour of each grid cell shows the *portion* of points (of the column of cells) that are in the cell, not the absolute number of points in the cell. 
#' @note E.g. if rainfall (in mm) is on the x axis, then each cell in column 5 mm - 10 mm shows what portion of the points between rain=5mm and rain=10mm are in the cell. 
#' @note Without scaling like this, it gets hard to compare different levels of rainfall. The sample sizes (number of points) can be quite different in different columns.
#'
#' @examples
#' # get path to example wasp data
#' f = system.file("extdata", "wasps_example.csv", package = "turkuwasps", mustWork = TRUE)
#' 
#' # read the wasp data and get the corresponding sample data
#' tmp = read_wasps(f)
#' x = tmp$x
#' m = tmp$m
#' 
#' # show how catches varied with rain and amount of wood 
#' explore(x$sample, m, what=c("rain", "deadwood", "livewood"))
#' 
#' @export
#'
explore = function(x, m, what=NULL, scale="tdiff", res=15, ...){
	
	# store various default arguments for the plot
	plot_args = list(
		res = res,
		ylab = "wasps"
	)
	
	# get the plot arguments given by the user
	user_args = list(...)

	# if column names were not given, use all numeric columns in `m`
	if (is.null(what)){
		col_class = lapply(m, class)
		i = which(col_class == "numeric")
		what = colnames(m[, i])
	}
	
	# ignore the column used for scaling
	what = what[what != scale]
	
	# only use samples from the collecting event(s) the wasps come from
	m = filter_samples(x, m)
	
	# make sure that all samples are counted (not just those that caught wasps)
	x = factor(x, levels=m$name)
	
	# count the number of wasps caught in each sample
	nwasps = table(x)
	
	# scale catches (by e.g. length of sample) if asked to do so, and adjust y axis label
	if (scale %in% colnames(m)){		
		nwasps = nwasps / m[, scale]
		plot_args$ylab = paste0("wasps / ", scale) 
	}
	
	# store default graphical parameters
	default_parameters = graphics::par(no.readonly = TRUE)
	
	# reserve space in the plot window for all the plots
	n = length(what)
	ncol = ceiling(n^0.5)
	nrow = ceiling(n / ncol)
	N = 1:(ncol*nrow)
	N[N>n] = 0
	graphics::layout(matrix(N, nrow, ncol))
	
	# draw one plot at a time
	for (i in what){
		
		# add x, y, and x axis label to the plot arguments
		plot_args$x = m[, i]
		plot_args$y = nwasps
		plot_args$xlab = i
		
		# overwrite with any arguments given by user
		plot_args[names(user_args)] = user_args
	
		# plot
		do.call(plot_explore, args=plot_args)

	}
	
	# return to default graphical parameters
	graphics::par(default_parameters)
	
}