#' Convert datetime to character
#'
#' Convert datetime objects to strings of the format "2014-10-14 11:04:00 UTC+03:00". 
#'
#' @param x Vector of datetimes.
#' @param ... Other parameters. Included for consistency with [as.character()], currently these are ignored.
#'
#' @return Character vector.
#'
#' @export
as.character.datetime = function(x, ...){
	
	# get datetime (in local time) as text
	d = x$d + x$tz
	d = format(d, "%Y-%m-%d %H:%M:%S UTC")
	
	# get time zone as text
	tz = as_h_min(x$tz)
	
	# join the datetime and time zone
	x = paste0(d, tz)	
	
	# return
	return(x)
	
}


#' Convert to datetime
#'
#' Convert strings of the format "2014-10-14 11:04:00 UTC+03:00" to datetime objects. 
#'
#' @param x Character vector giving dates, times and time zone offsets. Should ideally be in the format "2014-10-14 11:04:00 UTC+03:00", but may convert OK even if e.g. the time is missing. Can also be a datetime object, in which case `x` is returned without changes.
#'
#' @return Vector of datetime objects.
#' 
#' @seealso [axis_datetime()], [get_date()], [is_datetime()] and [set_tz()]. The following base functions have been modified to work with datetimes:
#' * [as.character.datetime()], [c.datetime()], [ceiling.datetime()], [floor.datetime()], [length.datetime()], [lines.datetime()], [max.datetime()], [mean.datetime()], [min.datetime()], [plot.datetime()], [points.datetime()], [print.datetime()], [seq.datetime()]
#' * basic operators such as `+`, `-` (see examples)
#' * data frames accept datetimes (but convert them to character)
#'
#' @examples
#'	
#' # save two dates as string
#' x = c("2014-10-14 11:04:00 UTC+03:00", "2014-11-02 01:04:00 UTC-02:30")
#'
#' # convert to datetime
#' x = as.datetime(x)
#'  
#' # try out different operations with datetimes
#' x
#' x + 15
#' x[2] - x[1]
#' floor(x)
#' c(x, x)
#' seq(x[1], x[2], by=3600*24)
#'
#' # datetimes are converted to character when saved in a data frame
#' m = data.frame(a=1:2, x=1:2)
#' m$x = x
#' m
#'
#' @export
as.datetime = function(x){
	
	# do nothing if `x`is already a datetime object
	if (is_datetime(x)){
		return(x)
	}
	
	# get the time zone text (e.g. "UTC+03:00")
	i = regexpr("*UTC*", x) + 3
	tz = substr(x, i, i+5)
	
	# extract the hours, minutes, and + or - sign
	h = as.numeric(substr(tz, 2, 3))
	min = as.numeric(substr(tz, 5, 6))
	mult = as.numeric(paste0(substr(tz, 1, 1), "1"))
	
	# save the time offset in seconds
	tz = mult * (3600 * h + 60 * min)
	
	# save the date and time (e.g. "2014-10-14 11:04:00") as POSIXct
	d = as.POSIXct(x, tz="UTC")
	
	# adjust so that all datetimes are in UTC
	d = d - tz
	
	# save datetimes and their offsets
	x = list(d=d, tz=tz)
	
	# convert to class "datetime"
	class(x) = "datetime"
	
	# return
	return(x)
	
}


#' Get time zone offsets of datetimes as hours and minutes
#'
#' Helper function for getting the time zone offsets of datetime objects in the format "03:00". Used by e.g. [as.character.datetime()]. 
#'
#' @param secs Vector of time zone offsets in seconds.
#'
#' @return Character vector of time zone offsets in hours and minutes.
#' 
as_h_min = function(secs){
	
	# convert seconds to decimal hours
	z = secs / 3600
	
	# get absolute value (easier to handle than treating + and - separately)
	Z = abs(z)
	
	# get hours (make - hours negative again)
	h = floor(Z)
	h[z<0] = -h[z<0]
	
	# get minutes
	min = 60 * (Z - floor(Z))
	
	# convert to string (e.g. "+03" and "00)
	h = formatC(h, digits=0, width=3, format="f", flag="0+")
	min = formatC(min, digits=0, width=2, format="f", flag="0")
	
	# join hours and minutes (e.g. "+03:00")
	tz = paste0(h, ":", min)
	
	# return
	return(tz)
	
}


#' Label the x axis of datetime plots
#'
#' Add labels and tick marks to the x axis of plots that have datetimes on the x axis, such as plots created by [plot_time()]. Called by [plot_time()] by default.
#'
#' @param xlim Interval object (see [as.interval()] giving the left and right limits of the plot.
#' @param ticks If TRUE (the default), draw tick marks above the label. These tick marks cannot be customised by the user: if wanting custom line widths, colours etc, you will have to draw them separately.
#' @param format Character string telling how to format the labels. See [strftime()] for accepted formats. Default is year-month-day.
#' @param breaks Vector of datetime objects (see [as.datetime()]) telling where to place labels. Anything outside the plot area is ignored. If not given, uses default breaks provided by [default_breaks()], which are at about three month intervals.
#' @param ... Other arguments passed to [text()] (which handles the actual drawing of the labels). Typically these can include `srt` to adjust the angle at which labels are drawn, and `cex` to adjust the label size.
#'
#' @return List of the arguments given to [text()], returned invisibly. These may be useful as a starting point if wanting to create custom labels, especially arguments `x` and `y` which give the plot coordinates of each label. 
#'
#' @export
axis_datetime = function(xlim, ticks=TRUE, format="%Y-%m-%d", breaks=default_breaks(), ...){
	
	# ignore breaks that are outside the plot area
	breaks = breaks[breaks >= xlim$s & breaks <= xlim$e]
	
	# if there are no breaks in the plot area, add breaks to the start and end
	if (length(breaks) == 0){
		breaks = c(xlim$s, xlim$e)
	}

	# get the coordinates of the plot region
	left = graphics::par("usr")[1]
	right = graphics::par("usr")[2]
	bottom = graphics::par("usr")[3]
	top = graphics::par("usr")[4]
		
	# store default arguments for the x axis labels
	label_args = list(
		
		# place labels at the correct datetime
		x = left + (breaks - xlim$s) / tdiff(xlim) * (right - left) ,
		
		# place labels below the x axis, leave a space that is 3% of the plot height
		y = bottom - (top - bottom) * 0.03 ,
		
		# convert labels to the desired format
		labels = get_date(breaks, format) ,
		
		# store various default arguments
		adj = c(0, 0.5) , 
		cex = 0.7 ,
		srt = 270 + 22 ,
		xpd = TRUE
		
	)
	
	# add the arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	label_args[names(user_args)] = user_args
	
	# draw the tick marks
	if (ticks){
		x0 = label_args$x
		y0 = bottom - 0.1 * 0.03 * (top - bottom)
		y1 = bottom - 0.6 * 0.03 * (top - bottom)
		n = length(x0)
		graphics::segments(x0=x0, y0=rep(y0, n), y1=rep(y1, n), lwd=0.4, xpd=TRUE)
	}
	
	# draw the labels
	do.call(graphics::text, args=label_args)
	
	# return arguments invisibly
	invisible(label_args)
	
}


#' Concatenate datetimes
#'
#' Join datetime objects with [c()]. 
#'
#' @param ... Vectors of datetimes.
#'
#' @return Vector of datetimes.
#' 
#' @method c datetime
#' 
#' @export
c.datetime = function(...){
	
	# list datetimes to be joined
	args = list(...)
	
	# create a datetime object in which the result will be stored
	x = args[[1]]
	
	# list datetimes and time zone offsets separately
	d = lapply(args, FUN=function(x){ x$d })
	tz = lapply(args, FUN=function(x){ x$tz })
	
	# join datetimes and time zone offsets separately, store in `x`
	x$d = do.call(c, d)
	x$tz = do.call(c, tz)
	
	# return
	return(x)
}


#' Set the time of datetimes to the end of the day
#'
#' Convert e.g. "2014-10-14 11:04:00 UTC+03:00" to "2014-10-15 00:00:00 UTC+03:00". 
#'
#' @param x Vector of datetimes.
#'
#' @return Vector of converted datetimes.
#'
#' @method ceiling datetime
#' @export
ceiling.datetime = function(x){
	
	# get start of day + 24 hours
	x = floor(x) + 3600 * 24
	
	# return
	return(x)
	
}


#' Set the time of datetimes to the start of the day
#'
#' Convert e.g. "2014-10-14 11:04:00 UTC+03:00" to "2014-10-14 00:00:00 UTC+03:00". 
#'
#' @param x Vector of datetimes.
#'
#' @return Vector of converted datetimes.
#' 
#' @method floor datetime
#' @export
floor.datetime = function(x){
	
	# create temporary function to get hours, minutes etc as numbers 
	get = function(x, f){
		as.numeric(get_date(x, f))
	}
	
	# get number of seconds from start of day
	secs = 3600 * get(x, f="%H") + 60 * get(x, f="%M") + get(x, f="%S")

	# subtract from datetimes
	x = x - secs
	
	# return
	return(x)
	
}


#' Format a datetime as string
#'
#' Get datetimes as string in different formats. Default is to get the date, e.g. "2014-10-14". Accepts all formats used by [strptime()], see `?strptime` for formats. Ugandan datetimes are in format "%Y-%m-%d %H:%M:%S UTC+03:00".
#'
#' @param x Vector of datetimes.
#' @param f String giving the desired format. 
#'
#' @return Character vector.
#' @export
get_date = function(x, f="%Y-%m-%d"){
	
	# get local time
	localtime = x$d + x$tz
	localtime = format(localtime, f)
	
	# return
	return(localtime)

}


#' Check for datetimes
#'
#' Find out if an object is a datetime object. 
#'
#' @param x Datetime or vector of datetimes.
#'
#' @return TRUE if `x`is a datetime, FALSE otherwise.
#' 
#' @export
is_datetime = function(x){
	
	# check if `x` is a datetime object
	return(methods::is(x, "datetime"))
	
}


#' Get length of vector of datetimes
#'
#' Get the length of a vector of datetimes with [length()]. Datetime objects are basically a list with two items (`d`=datetimes, `tz`=time zone offsets), so unmodified [length()] would always return length=2, irrespective of the actual length. 
#'
#' @param x Vector of datetimes.
#'
#' @return Length of the vector.
#' 
#' @method length datetime
#' @export
length.datetime = function(x){
	
	# get length from the datetimes
	return(length(x$d))
	
}


#' Add lines to a datetime plot
#'
#' Add lines to a plot that has datetimes on the x axis. Basically a wrapper for [lines()], but converts the datetimes to plot coordinates.
#'
#' @param x Vector of datetimes.
#' @param y Vector of y coordinates, same length as `x`.
#' @param xlim Interval object (see [as.interval()]) giving the left and right limits of the plot.
#' @param ...  Graphical parameters and other arguments passed to [lines()], which handles the actual drawing.
#'
#' @seealso [as.datetime()]
#'  
#' @export
lines.datetime = function(x, y, xlim, ...){
	
	# get the coordinates of the plot region
	left = graphics::par("usr")[1]
	right = graphics::par("usr")[2]
	
	# convert the datetimes to plot coordinates
	x2 = left + (x - xlim$s) / tdiff(xlim) * (right - left)
	
	# draw the lines	
	graphics::lines(x2, y, ...)	
	
}


#' Get latest datetime
#'
#' Get the latest item in a vector of datetime objects. 
#'
#' @param ... Vector of datetimes.
#' @param na.rm If TRUE, NA values are removed. Passed to [max()].
#'
#' @return Datetime object.
#'
#' @note Unlike in base [max()], only one vector can be given as an argument. Any others will be ignored.
#'
#' @method max datetime
#' @export
max.datetime = function(..., na.rm=FALSE){
	
	# get first vector in arguments
	x = list(...)[[1]]
	
	# get latest datetime
	i = which(x$d == max(x$d))
	x = x[i][1]
	
	# return
	return(x)
	
}


#' Get mean datetime
#'
#' Get the average of a vector of datetime objects. 
#'
#' @param x Vector of datetimes.
#' @param ... Other arguments passed to [mean()].
#'
#' @return Datetime object (in timezone offset UTC+00:00).
#'
#' @method mean datetime
#' @export
mean.datetime = function(x, ...){
	
	s = as.datetime("2000-01-01 00:00:00 UTC+00:00")	
	
	m = mean(x - s)
	x = s + m

	# return
	return(x)
	
}


#' Get earliest datetime
#'
#' Get the earliest item in a vector of datetime objects. 
#'
#' @param ... Vector of datetimes.
#' @param na.rm If TRUE, NA values are removed. Passed to [min()].
#'
#' @return Datetime object.
#'
#' @note Unlike in base [min()], only one vector can be given as an argument. Any others will be ignored.
#' 
#' @method min datetime
#' @export
min.datetime = function(..., na.rm=FALSE){
	
	# get first vector in arguments
	x = list(...)[[1]]
	
	# get earliest datetime
	i = which(x$d == min(x$d))
	x = x[i][1]
	
	# return
	return(x)
	
}


#' Make basic operators work on datetimes
#'
#' Define methods for handling basic operations on datetime objects. (e.g. `+`, `-` `<` etc) This is called by R whenever e.g. a datetime is subtracted from another datetime; not meant to be called by the user.
#'
#' @param e1 Vector of datetimes (or numbers).
#' @param e2 Vector of datetimes (or numbers).
#'
#' @return Result of operation. Varies by operator, see details.
#'
#' @details
#' Supported operators are:
#' * `+` Add seconds to datetimes. 
#' * `-` Get the difference (in seconds) between two datetimes. Or if a number is subtracted from a datetime, subtract that number of seconds from it. 
#' * `==` Check if two datetimes occur at the same time. 
#' * `!=` Check if two datetimes do not occur at the same time. 
#' * `<` Check if a datetime is before another. 
#' * `<=` Check if a datetime is before or at same time as another. 
#' * `>` Check if a datetime is after another. 
#' * `>=` Check if a datetime is after or at same time as another. 
#' All comparisons are done in UTC. What time zone the datetimes happen to be displayed in is irrelevant. For example, "2000-01-01 12:00:00 UTC+03:00" and "2000-01-01 09:00:00 UTC+00:00" are equal, because they occurred at the same time. And "2000-01-01 12:00:00 UTC+03:00" and "2000-01-01 12:00:00 UTC+00:00" are not equal, since one occurred three hours before the other. 
#' 
#' @seealso Section "Ops" in `?groupGeneric`, which lists all the operators.
#' 
#' @method Ops datetime
#' @export
Ops.datetime = function (e1, e2){
	
	# find out which operator was used (e.g. "+", "-", etc)
	op = .Generic
	
	# if +, add seconds to the datetime
	if (op == "+"){	
		if (is_datetime(e1)){
			e1$d = e1$d + e2
			e1$tz = rep(e1$tz, length(e1$d) / length(e1$tz))
			return(e1)
		} else {
			e2$d = e2$d + e1
			e2$tz = rep(e2$tz, length(e2$d) / length(e2$tz))
			return(e2)
		}		
	} 
	
	# if -, either get the number of seconds between two datetimes, 
	# or subtract seconds from one datetime 
	if (op == "-"){
		if (is_datetime(e2)){
			difference = as.numeric(difftime(e1$d, e2$d, units="secs"))
			return(difference)
		} else {
			e1$d = e1$d - e2
			e1$tz = rep(e1$tz, length(e1$d) / length(e1$tz))
			return(e1)
		}
	}
	
	# if ==, check if datetimes were at the same time (time zones ignored)
	if (op == "=="){
		return(e1$d == e2$d)
	}
	
	# if !=, check if datetimes were not at the same time (time zones ignored)
	if (op == "!="){
		return(e1$d != e2$d)
	}
	
	# if <, check if a datetime was before another (time zones ignored)
	if (op == "<"){
		return(e1$d < e2$d)
	}
	
	# if <=, check if a datetime was before or at same time as another (time zones ignored)
	if (op == "<="){
		return(e1$d <= e2$d)
	}

	# if >, check if a datetime was after another (time zones ignored)	
	if (op == ">"){
		return(e1$d > e2$d)
	}

	# if >=, check if a datetime was after or at same time as another (time zones ignored)		
	if (op == ">="){
		return(e1$d >= e2$d)
	}
	
	# stop if the operator doesn't work for datetime objects
	stop(paste0("'", op, "' does not work for datetime objects."))
	
}


#' Draw a scatterplot with datetimes
#'
#' Draw a scatterplot that has datetimes on the x axis. Basically a wrapper for [plot.default()], but converts the datetimes to plot coordinates.
#'
#' @param x Vector of datetimes.
#' @param y Vector of y coordinates, same length as `x`.
#' @param xlim Interval object (see [as.interval()]) giving the left and right limits of the plot. If not given, default limits are used.
#' @param xlabel If TRUE, the x axis is labelled (using default settings) by [axis_datetime()]. If FALSE, the x axis is left blank.
#' @param ...  Graphical parameters and other arguments passed to [plot()], which handles the actual drawing.
#'
#' @seealso [as.datetime()]
#'  
#' @return A list giving the x and y coordinates, returned silently. The list has four items:
#' * `x` The x coordinates of each point, in plot coordinates.
#' * `y` The y coordinates of each point, in plot coordinates.
#' * `d` The x coordinates of each point, as datetimes.
#' * `xlim` The left and right limits of the plot as an interval object.
#'  
#' @export
plot.datetime = function(x, y, xlim=NULL, xlabel=TRUE, ...){
	
	# get pretty y limits for the plot region
	top = max(y, na.rm=TRUE)
	bottom = min(min(y, na.rm=TRUE), 0)
	top = top + (top - bottom) * 0.04
	bottom = bottom - (top - bottom) * 0.04
	
	# store default arguments for the plot
	plot_args = list(
		xaxs = "i" ,
		xaxt = "n" ,
		xlab = "" ,  
		yaxs = "i" ,
		ylab = "" ,
		ylim = c(bottom, top)
	)
	
	# add the arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	plot_args[names(user_args)] = user_args
	
	# if xlim wasn't given, use default datetime limits on the x axis
	if (is.null(xlim)){
		xlim = as.interval(min(x, na.rm=TRUE), max(x, na.rm=TRUE))
	}
	
	# set the x coordinates of the plot region
	left = 0
	right = tdiff(xlim) / 3600 / 24
	
	# add the x coordinates of the plot region to the arguments
	plot_args$xlim = c(left, right)
	
	# add the x coordinates (by converting from datetimes), and the y coordinates
	plot_args$x = left + (x - xlim$s) / tdiff(xlim) * (right - left)
	plot_args$y = y
	
	# draw the plot
	do.call(graphics::plot, args=plot_args)
	
	# add a default x axis if asked to do so
	if (xlabel) {
		axis_datetime(xlim)
	}
	
	# return the x and y coordinates, the x coordinates as datetimes, and the x limits
	invisible(list(x=plot_args$x, y=y, d=x, xlim=xlim))
	
}


#' Add points to a datetime plot
#'
#' Add points to a plot that has datetimes on the x axis. Basically a wrapper for [points()], but converts the datetimes to plot coordinates.
#'
#' @param x Vector of datetimes.
#' @param y Vector of y coordinates, same length as `x`.
#' @param xlim Interval object (see [as.interval()]) giving the left and right limits of the plot.
#' @param ...  Graphical parameters and other arguments passed to [points()], which handles the actual drawing.
#'
#' @seealso [as.datetime()]
#'  
#' @export
points.datetime = function(x, y, xlim, ...){
	
	# get the coordinates of the plot region
	left = graphics::par("usr")[1]
	right = graphics::par("usr")[2]
	
	# convert the datetimes to plot coordinates
	x2 = left + (x - xlim$s) / tdiff(xlim) * (right - left)
	
	# draw the points
	graphics::points(x2, y, ...)
	
}


#' Print datetimes
#'
#' Print datetimes in the format "2014-10-14 11:04:00 UTC+03:00". Datetime objects are basically a list with two items (`d`=datetimes, `tz`=time zone offsets), which would not display tidily with unmodified [print()]. 
#'
#' @param x Vector of datetimes.
#' @param ... Other arguments passed to [print()].
#'
#' @return Printed character vector.
#'
#' @method print datetime
#' @export
print.datetime = function(x, ...){
	
	# convert to character
	x = as.character(x)
	
	# print
	print(x, ...)
	
}


#' Get sequence of datetimes
#'
#' Get a sequence of datetime objects. Creates a sequence that starts from the first datetime, and stops at or after the second datetime. 
#'
#' @param s Starting datetime.
#' @param e End datetime.
#' @param step Interval between datetimes in seconds. Default is 3600*24 seconds = one day.
#' @param ... Other arguments can be given for compatibility with [seq()], but will be ignored.
#'
#' @return Vector of datetime objects.
#'
#' @method seq datetime
#' @export
seq.datetime = function(s, e, step=3600*24, ...){
	
	# get the required length of the sequence
	n = ceiling((e-s) / step)
	
	# create a sequence of seconds
	secs = 0:n * step
	
	# add the seconds to the start datetime
	x = s + secs
	
	# return
	return(x)
	
}


#' Set time zone of datetimes
#'
#' Set the desired time zone offset of datetime objects. This does not change the date and time, they are always stored as UTC internally. This just adjusts what time zone to display the datetimes in.
#'
#' @param x Vector of datetimes.
#' @param tz Character vector of time zone offsets, in format "+00:00". Can also be a single string, in which case it is recycled to the same length as `x`.
#'
#' @return Vector of datetime objects.
#'
#' @export
set_tz = function(x, tz="+00:00"){
	
	# extract the hours, minutes, and + or - sign
	h = as.numeric(substr(tz, 2, 3))
	min = as.numeric(substr(tz, 5, 6))
	mult = as.numeric(paste0(substr(tz, 1, 1), "1"))
	
	# save the new time offset in seconds
	x$tz[] = mult * (3600 * h + 60 * min)
		
	# return
	return(x)
}


#' Make data frames accept datetimes
#'
#' Called by R whenever something is saved by name into a data frame (e.g. `x$name = value`). Not meant to be called by the user. Coverts any datetime object to character before continuing. Datetime objects are basically a list with two items (`d`=datetimes, `tz`=time zone offsets), not vectors, so they won't save properly into data frames if not converted.  
#'
#' @param x Data frame into which a variable is being saved.
#' @param name Name to give to the column in the data frame.
#' @param value Variable being saved in the data frame.
#'
#' @return Data frame with new column added. If the new column is a datetime, it is converted to character.
#' 
#' @export
'$<-.data.frame' <- function(x, name, value) {
		
	# convert to character if `value` is a datetime
	if (is_datetime(value)){
		value = as.character.datetime(value)
	}
	
	# call base function
	return(base::'$<-.data.frame'(x, name, value)) 

}


#' Select items of a datetime vector
#'
#' Called by R whenever items are selected by index in a vector of datetime objects (e.g. `x[1]`). Not meant to be called by the user. Datetime objects are basically a list with two items (`d`=datetimes, `tz`=time zone offsets), not vectors, so the standard square brackets would not work properly.  
#'
#' @param x Vector of datetimes.
#' @param i Indexes of items to select. Default is to select everything.
#'
#' @return Vector of datetimes.
#' 
#' @method [ datetime
#' @export
'[.datetime' = function(x, i=1:length(x)){
	
	# get items of the actual datetimes and the time zone offsets separately
	x$d = x$d[i]
	x$tz = x$tz[i]
	
	# return
	return(x)
	
}


#' Save datetimes into a vector
#'
#' Called by R whenever datetime objects are saved by index into a vector of datetimes (e.g. `x[2] = d`). Not meant to be called by the user. Datetime objects are basically a list with two items (`d`=datetimes, `tz`=time zone offsets), not actually vectors, so the standard code for saving would not work properly.  
#'
#' @param x Vector of datetimes.
#' @param i Indexes of items where to save.
#' @param j Parameter called by R, but I'm not sure what, if anything, it does. Not used.
#' @param value The datetime(s) to be saved in `x`.
#'
#' @return Vector of datetimes.
#' 
#' @method [<- datetime
#' @export
'[<-.datetime' = function(x, i, j="not used", value){
	
	#
	if (! is_datetime(value)){
		stop(paste("Only datetimes can be saved into a datetime.", value, "is not a datetime object."))
	}
	
	#
	x$d[i] = value$d
	x$tz[i] = value$tz
	
	# return
	return(x)
	
}

