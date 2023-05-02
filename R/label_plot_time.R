#' Label plots created by plot_time()
#'
#' Add labels and tick marks to the x axis of plots created by [plot_time()]. Called by [plot_time()] by default, and can also be used to get a custom x axis without having to delve into the complex details of labelling x axes that show datetimes.
#'
#' @param xlim Interval object (see [as.interval()] giving the left and right limits of the plot.
#' @param step The width (in seconds) of each bar.
#' @param ticks If TRUE (the default), draw tick marks above the label. These tick marks cannot be customised by the user: if wanting custom line widths, colours etc, you will have to draw them separately.
#' @param format Character string telling how to format the labels. See [strftime()] for accepted formats. Default is year-month-day.
#' @param breaks Vector of datetime objects (see [as.datetime()]) telling where to place labels. Anything outside the plot area is ignored. If not given, uses default breaks provided by [default_breaks()], which are at about three month intervals.
#' @param ... Other arguments passed to [text()] (which handles the actual drawing of the labels). Typically these can include `srt` to adjust the angle at which labels are drawn, and `cex` to adjust the label size.
#'
#' @return List of the arguments given to [text()], returned invisibly. These may be useful as a starting point if wanting to create custom labels, especially arguments `x` and `y` which give the plot coordinates of each label. 
#'
#' @export
label_plot_time = function(xlim, step, ticks=TRUE, format="%Y-%m-%d", breaks=default_breaks(), ...){
	
	# ignore breaks that are outside the plot area
	breaks = breaks[breaks >= xlim$s & breaks <= xlim$e]

	# store default arguments for the x axis labels
	label_args = list(
		
		# place labels at the correct datetime
		x = (breaks - xlim$s) / step,
		
		# place labels below the x axis, leave a space that is 3% of the plot height
		y = -graphics::par("usr")[4] * 0.03,
		
		# convert labels to the desired format
		labels = get_date(breaks, format),
		
		# store various default arguments
		adj = c(0, 0.5), 
		cex = 0.7,
		srt = 270+22,
		xpd = TRUE
		
	)
	
	# add the arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	label_args[names(user_args)] = user_args
	
	# draw the tick marks
	if (ticks){
		n = length(x)
		x = label_args$x
		y = label_args$y
		graphics::segments(x0=x, y0=rep(y*0.1, n), y1=rep(y*0.6, n), lwd=0.4, xpd=TRUE)
	}
	
	# draw the labels
	do.call(graphics::text, args=label_args)
	
	# return arguments invisibly
	invisible(label_args)
	
}