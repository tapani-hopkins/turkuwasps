#' Plot weather data
#'
#' Plot weather data such as rainfall or temperature over time. By default, uses the data in [weather] and uses default colours, axes etc.
#' 
#' @param what Name of the column in the weather data that should be plotted. Default is to plot rainfall.
#' @param xlim Interval object (see [as.interval()]) giving the x limits of the plot. If NULL, the start and end datetimes of the Ugandan sampling are used.
#' @param k How much to smooth the data by (see [smooth_data()]). Number of days over which to smooth the data. Default (21) is to take the average of the past 21 days. If NA, the data is not smoothed.
#' @param nticks How many tick marks to have on the default y axis. Approximate, passed to [pretty()].The default (3ish) prevents the axis from getting squashed in typical plotting of weather data, where there is little vertical space. If NA, the default y axis is not drawn.
#' @param add If TRUE, the data is added to the existing plot with [points.datetime()]. If FALSE, the default, draws a new plot with [plot.datetime()].
#' @param weather Data frame giving the weather data. Must contain columns "start", "end", and the column to be plotted. Default is to use the package's built in data [weather].
#' @param ... Graphical parameters and other arguments passed to [plot.datetime()] or (if `add` is TRUE, to [points.datetime()]). These will override any default values. Typical arguments to adjust include `axes`, `bty`, `col`, `type`, `xlabel`.
#'
#' @return A list giving the x and y coordinates, returned silently. The list has three items:
#' * `x` The datetimes of each point. These are at the end of the time period during which the weather was measured (or for smoothed data, the end of the time period over which the data was smoothed).
#' * `y` The y coordinates of each point.
#' * `xlim` The left and right limits of the plot as an interval object.
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
#' # get x limits that cover the wasp data
#' xlim = plot_time(as.interval(x$start, x$end), plot=FALSE, xlabel=FALSE)$xlim
#' 
#' # adjust the plot layout
#' layout(matrix(1:2), heights=c(1, 3))
#' 
#' # plot rainfall
#' par(mar=c(1.5, 4.1, 0.5, 0.1))
#' plot_weather(xlim=xlim, ylab="rain (mm)")
#' 
#' # plot wasp catches
#' par(mar=c(3.5, 4.1, 2, 0.1))
#' plot_time(as.interval(x$start, x$end), m, x$taxon, ylab="wasps / trap day")
#' 
#' @export
#'
plot_weather = function(what="rain", xlim=NULL, k=21, nticks=3, add=FALSE, weather=turkuwasps::weather, ...){
	
	# stop if the 'what' isn't in the weather data
	wcols = colnames(weather)[sapply(weather, class) == "numeric"]
	if (! what %in% wcols) { 
		stop(paste0("Cannot find column '", what, "' in the weather data. The weather data contains the following columns with numeric data: ", paste(wcols, collapse=", ")))
	}
	
	# if x limits weren't given, use default Ugandan x limits
	if (is.null(xlim)){
		xlim = as.interval("2014-08-01 00:00:00 UTC+03:00", "2015-10-01 00:00:00 UTC+03:00")
	}
	
	# store various default arguments for the plot
	plot_args = list(
		axes = FALSE,
		bty = "n", 
		col = "blue",
		type = "l",
		xlabel = FALSE,
		xlim = xlim
	)
	
	# remove extra arguments if adding to an existing plot
	if (add){
		plot_args = plot_args[! names(plot_args) %in% c("axes", "xlabel")]
	}
	
	# use red as the default colour for temperatures and blue for rain
	if (what != "rain"){ plot_args$col="red" }
	
	# set the axis labels to the same colour as default
	plot_args$col.lab = plot_args$col
	
	# add the plot arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	plot_args[names(user_args)] = user_args
	
	# get the dates of the weather measurements
	wdate = as.interval(weather$start, weather$end)
	
	# get the weather data, and smooth it if asked to do so
	if (is.na(k)){
		w = list(x=wdate$e, y=weather[, what])
	} else {
		w = smooth_data(weather[, what], xdate=wdate, k=k, xlim=xlim)
	}
	
	# add the x and y coordinates to the plot arguments
	plot_args$x = w$x
	plot_args$y = w$y
	
	# plot
	if (add){ do.call(points, args=plot_args) } else { do.call(plot, args=plot_args) }
	
	# add a default y axis if asked to do so
	if (! is.na(nticks)){
		ticks = pretty(par("yaxp")[1:2], n=nticks)
		ticks = ticks[-length(ticks)]
		axis(2, at=ticks, col=plot_args$col, col.axis=plot_args$col)
	}
	
	# return the x and y coordinates invisibly
	invisible(list(x=w$x, y=w$y, xlim=xlim))

}