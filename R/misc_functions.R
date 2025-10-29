# Miscellaneous turkuwasps functions. Mainly helper functions used by other functions, but may be useful for the user too.


#' Combine columns
#'
#' Combine the columns in a data frame. Mainly for combining e.g. the forest type and collecting event of wasp data for easy use in rarefaction plots etc. A handier wrapper for [paste()], since it preserves factor levels and their order.
#'
#' @param x Data frame. Usually wasp data where each row is an individual wasp.
#' @param what Character vector giving the names of the columns of `x` that are to be combined.
#' @param sep String to separate the values of the columns with. Default is a space: e.g. "primary" and "Uganda 2014-2015" become "primary Uganda 2014-2015".
#' @param all If FALSE (the default), simply combines the values in the columns. If TRUE, combines the values and gives the result as a factor, whose levels include all possible combinations of column values. See 'Details'.
#'
#' @return Character vector or factor of the same length as there are rows in `x`. Will be a character vector if `all` is FALSE and none of the combined columns were factors. Otherwise a factor.
#'
#' @details The function tries to preserve the order of any factor levels. So if any of the columns is a factor, it will return a factor whose levels are in the same order. (e.g. if "primary" was the first forest type, then "primary Uganda 2014-2015" will come before "disturbed Uganda 2014-2015".) 
#' @details If `all` is TRUE, the function will return a factor whose levels include all possible combinations of the combined columns. I.e. if combining forest type (5 levels) and event (5 levels), there will be 5*5=25 levels, even if the data only consisted of one row which combined to "primary Uganda 2014-2015".
#' @details If 'all' is FALSE and none of the columns is a factor, returns a character vector.
#'
#' @examples
#' # make a simple example dataset
#' x = data.frame(type=c("disturbed", "primary"), event=c("Uganda", "Amazon"))
#' x
#'
#' # combine
#' combine_columns(x, c("type", "event"))
#'
#' # combine and show the four possible factor levels
#' combine_columns(x, c("type", "event"), all=TRUE)
#'
#' # make the first column a factor with three levels
#' x$type = factor(x$type, levels=c("primary", "disturbed", "farm"))
#'
#' # combine and show the six possible factor levels
#' combine_columns(x, c("type", "event"), sep="_", all=TRUE)
#'
#' @export
#'
combine_columns = function(x, what, sep=" ", all=FALSE){

	# paste the columns together 
	combination = x[, what[1]]
	for (i in 2:length(what)){
		combination = paste(combination, x[, what[i]], sep=sep)
	}
		
	# get all the possible combinations of the columns' values or levels, in the same order as any factor levels
	levs0 = NULL
	levs = levels0(x[, what[1]])
	for (i in 2:length(what)){
		for (ii in 1:length(levs)){
			levs0 = c(levs0, paste(levs[ii], levels0(x[, what[i]]), sep=sep))
		}
		levs = levs0
	}
		
	# save as factor, with all possible combinations as factor levels, if asked to do so..
	if (all){	
		combination = factor(combination, levels=levs)
	
	# .. or if any of the columns were factors, save as factor without the unused levels
	} else if (any(lapply(x, class) == "factor")){
		combination = factor(combination, levels=levs)
		combination = droplevels(combination)
	}
	
	# return the combined columns
	return(combination)
	
}


#' Smooth data that has been collected during datetime intervals
#'
#' Smooth e.g. weather data that has been collected at specific time intervals. Useful to e.g. show average rainfall.
#'
#' @param x Numeric vector of data values, such as rainfall per day, or temperature.
#' @param xdate Vector of intervals (see [as.interval()]) of same length as `x`, giving the times when the data was collected.
#' @param k Number of days over which to smooth the data. Default (28) is to take the average of the past 28 days. Larger numbers give smoother results.
#' @param p Minimum portion of the data that is required. Filters out random spikes at the start and end of when there is data. Default is to return NA if less than 25% of the period over which the data is averaged has data. 
#' @param xlim Interval object giving the start and end of the time period for which smoothed data is wanted. If not given, the smoothed data will cover the entire period from which there is data.
#' @param step Desired resolution (in seconds) of the smoothed data. Default is to return a smoothed data point every 24 hours.
#'
#' @return List giving the datetimes and values of the smoothed data. The list has two items: 
#' * `x` Vector of datetimes giving the end of the datat point's time interval.
#' * `y` Vector of smoothed data.
#'
#' @details
#' The default smoothing takes the average of the past 28 days' data, at one-day intervals. Data that was collected at the start or end of the 28 day period is weighted down: if e.g. only half of the time period during which the data was collected is in the 28-day period, it'll have a 50% weight in the weighted average.
#' 
#' @examples
#' # get the start and end times of weather data
#' wdate = as.interval(weather$start, weather$end)
#' 
#' # get the amount of rain per day
#' rain = weather$rain / tdiff(wdate) * 3600 * 24
#'
#' # smooth the rain data
#' r = smooth_data(rain, wdate)
#' 
#' # plot the raw data as points, and the smoothed data as a line
#' xlim = as.interval(min(wdate), max(wdate))
#' plot(wdate$e, rain, xlim=xlim, pch=20, cex=0.1, col="blue")
#' lines(r$x, r$y, xlim=xlim, col="blue")
#'
#' @export
#'
smooth_data = function(x, xdate, k=28, p=0.25, xlim=NULL, step=3600*24){
	
	# if xlim wasn't given, span the whole time period of the data
	if (is.null(xlim)){
		xlim = as.interval(min(xdate), max(xdate))
	}
	
	# create data points for the smoothed data at `step` second intervals
	px = seq(xlim$s, xlim$e, step=step)
	px_k = as.interval(px - 3600*24*k, px)
	
	# create variable for the smoothed data
	y = rep(NA, length(px))
	
	# Smooth the data and add to each smoothed data point
	for (i in 1:length(px_k)){
		
		# count how much each data point overlaps with this time period
		over = overlap(xdate, px_k[i])
		
		# get the weights of each data point (0: not in time period, 1: entirely in time period)
		weight = over / tdiff(xdate)
		
		# check how large a part of the time period has data
		coverage = sum(over) / tdiff(px_k[i])
		
		# take the weighted average of the time period's data if there's enough data..
		if (coverage >= p){
			y[i] = stats::weighted.mean(x, w=weight, na.rm=TRUE)
		
		# .. if there's not enough data, set to NA
		} else {
			y[i] = NA
		}
		
	}
	
	# return the smoothed data and their time interval
	return(list(x=px, y=y))
	
}

