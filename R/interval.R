#' Convert to interval
#'
#' Convert vectors giving the start and end of time intervals to interval objects. 
#'
#' @param s Vector of datetimes giving the start of each interval. Either datetime objects (see [as.datetime()]) or something that can be converted to datetime.
#' @param e Vector of datetimes giving the end of each interval. Either datetime objects (see [as.datetime()]) or something that can be converted to datetime.
#'
#' @return Vector of interval objects.
#' 
#' @seealso [as.datetime()], [is_interval()], [means()], [overlap()], [overlaps()], [tdiff()]. The following base functions have been modified to work with datetimes:
#' * [c.interval()], [length.interval()], [max.interval()], [min.interval()], [print.interval()]
#' * basic operators such as `+`, `-` (see examples)
#'
#' @examples
#'	
#' # save start and end datetimes
#' s = as.datetime(c("2014-01-14 00:00:00 UTC+00:00", "2014-01-14 12:00:00 UTC+00:00"))
#' e = as.datetime(c("2014-01-15 00:00:00 UTC+00:00", "2014-01-15 12:00:00 UTC+00:00"))
#'  
#' # save as interval
#' x = as.interval(s, e)
#'
#' # try out different operations with intervals
#' x
#' x + 15
#' c(x, x[1])
#'
#' # get length in seconds, and overlap
#' tdiff(x)
#' overlap(x, x[1])
#'
#' @export
as.interval = function(s, e){
	
	# make sure the start and end times are datetime objects
	s = as.datetime(s)
	e = as.datetime(e)
	
	# save start and end datetimes in a list
	x = list(s=s, e=e)
	
	# convert to class "interval"
	class(x) = "interval"
	
	# return
	return(x)
	
}


#' Concatenate intervals
#'
#' Join interval objects with [c()]. 
#'
#' @param ... Vectors of intervals.
#'
#' @return Vector of intervals.
#' 
#' @method c interval
#' @export
c.interval = function(...){
	
	# list intervals to be joined
	args = list(...)
	
	# create an interval object in which the result will be stored
	x = args[[1]]
	
	# list start and end intervals separately
	s = lapply(args, FUN=function(x){ x$s })
	e = lapply(args, FUN=function(x){ x$e })
	
	# join start and end intervals separately, store in `x`
	x$s = do.call(c, s)
	x$e = do.call(c, e)
	
	# return
	return(x)
}


#' Check for intervals
#'
#' Find out if an object is an interval object. 
#'
#' @param x Interval or vector of intervals.
#'
#' @return TRUE if `x`is an interval, FALSE otherwise.
#' 
#' @export
is_interval = function(x){
	
	# check if `x` is an interval object
	return(methods::is(x, "interval"))
	
}


#' Get latest datetime in intervals
#'
#' Get the latest datetime in a vector of interval objects. Checks the start and end datetimes, and returns the latest one.
#'
#' @param ... Vector of datetimes.
#' @param na.rm If TRUE, NA values are removed. Passed to [max()].
#'
#' @return Datetime object.
#'
#' @note Unlike in base [max()], only one vector can be given as an argument. Any others will be ignored.
#'
#' @method max interval
#' @export
max.interval = function(..., na.rm=FALSE){
	
	# get first vector in arguments
	x = list(...)[[1]]
	
	# get latest datetime in start or end dates
	s = max(x$s, na.rm=na.rm)
	e = max(x$e, na.rm=na.rm)
	x = max(c(s, e), na.rm=na.rm)
		
	# return
	return(x)
	
}


#' Get means of intervals
#'
#' Get the averages in between the start and end datetimes of a vector of interval objects. 
#'
#' @param x Vector of intervals.
#'
#' @return Vector of datetimes.
#' 
#' @export
means = function(x){
	
	# get averages of start and end dates
	x = x$s + 0.5 * (x$e - x$s)
	
	# return
	return(x)
	
}


#' Get earliest datetime in intervals
#'
#' Get the earliest datetime in a vector of interval objects. Checks the start and end datetimes, and returns the earliest one.
#'
#' @param ... Vector of datetimes.
#' @param na.rm If TRUE, NA values are removed. Passed to [min()].
#'
#' @return Datetime object.
#'
#' @note Unlike in base [min()], only one vector can be given as an argument. Any others will be ignored.
#'
#' @method min interval
#' @export
min.interval = function(..., na.rm=FALSE){
	
	# get first vector in arguments
	x = list(...)[[1]]
	
	# get latest datetime in start or end dates
	s = min(x$s, na.rm=na.rm)
	e = min(x$e, na.rm=na.rm)
	x = min(c(s, e), na.rm=na.rm)
		
	# return
	return(x)
	
}


#' Get length of vector of intervals
#'
#' Get the length of a vector of intervals with [length()]. Interval objects are basically a list with two items (`s`=start datetimes, `e`=end datetimes), so unmodified [length()] would always return length=2, irrespective of the actual length. 
#'
#' @param x Vector of intervals.
#'
#' @return Length of the vector.
#' 
#' @method length interval
#' @export
length.interval = function(x){
	
	# get length from the start datetimes
	return(length(x$s))
	
}


#' Make basic operators work on intervals
#'
#' Define methods for handling some basic operations on interval objects. (e.g. `+`, `-`) This is called by R e.g. whenever numbers are added or subtracted to an interval; not meant to be called by the user.
#'
#' @param e1 Vector of intervals.(or numbers)
#' @param e2 Vector of intervals (or numbers).
#'
#' @return Result of operation. Varies by operator, see details.
#'
#' @details
#' Supported operators are:
#' * `+` Add seconds to intervals. 
#' * `-` Subtract seconds from intervals. 
#' * `==` Check if two intervals occur at the same time. 
#' * `!=` Check if two intervals do not occur at the same time. 
#' 
#' @seealso Section "Ops" in `?groupGeneric`, which lists all the operators.
#' 
#' @method Ops interval
#' @export
Ops.interval = function (e1, e2){
	
	# find out which operator was used (e.g. "+", "-", etc)
	op = .Generic
	
	# if +, add seconds to the start and end datetimes
	if (op == "+"){	
		if (is_interval(e1)){
			e1$s = e1$s + e2
			e1$e = e1$e + e2
			return(e1)
		} else {
			e2$s = e2$s + e1
			e2$e = e2$e + e1
			return(e2)
		}		
	} 
	
	# if -, subtract seconds from the start and end datetimes
	if (op == "-"){
			e1$s = e1$s - e2
			e1$e = e1$e - e2
			return(e1)
	}
	
	# if ==, check if start and end datetimes were at the same time (time zones ignored)
	if (op == "=="){
		return(e1$s == e2$s & e1$e == e2$e)
	}
	
	# if !=, check if start or end datetimes were not at the same time (time zones ignored)
	if (op == "!="){
		return(e1$s != e2$s | e1$e != e2$e)
	}
	
	# stop if the operator doesn't work for interval objects
	stop(paste0("'", op, "' does not work for interval objects."))
	
}


#' Check how much intervals overlap
#'
#' Get the number of seconds that interval objects overlap with an interval. 
#'
#' @param x Vector of intervals to check for overlap.
#' @param with Interval objects that `x` overlap with.
#'
#' @return Vector of numbers, giving the number of seconds that each interval in `x` overlaps the interval with.
#'
#' @export
overlap = function(x, with){
	
	# helper function for getting whichever datetime is smaller
	get_smaller = function(x, with){
		x[with < x] = with
		return(x)
	}
	
	# helper function for getting whichever datetime is larger
	get_larger = function(x, with){
		x[with > x] = with
		return(x)
	}

	# get start and end of each overlap (gives nonsense results if there is no overlap at all)
	x$s = get_larger(x$s, with$s)
	x$e = get_smaller(x$e, with$e)
	
	# get length of overlap, and set to to 0 if there was no overlap at all
	tdiff = tdiff(x)
	tdiff[! overlaps(x, with)] = 0
	
	# return
	return(tdiff)
	
}


#' Check if intervals overlap
#'
#' Check which interval objects overlap with an interval. 
#'
#' @param x Vector of intervals to check.
#' @param with Interval objects that `x` either overlap with or don't.
#'
#' @return Vector of TRUE or FALSE.
#'
#' @export
overlaps = function(x, with){
	
	# check if intervals do not overlap (=entirely before or after `with`)
	no_overlap = (with$s >= x$e | with$e <= x$s)
	
	# return whether intervals overlap or not
	return(! no_overlap)
	
}


#' Print intervals
#'
#' Print intervals in a readable format. Interval objects are basically a list with two items (`s`=start datetimes, `e`=end datetimes), which would not display tidily with unmodified [print()]. 
#'
#' @param x Vector of intervals.
#' @param ... Other arguments passed to [print()].
#'
#' @return Printed data frame.
#'
#' @method print interval
#' @export
print.interval = function(x, ...){
	
	# add start and end to data frame as character
	x = data.frame(start=as.character(x$s), end=as.character(x$e))
	
	# transpose data frame and blank out the column names
	x = t(x)
	colnames(x) = rep("", ncol(x))
	
	# print
	print(x, ...)
	
}


#' Get sum of how much intervals overlap
#'
#' Find out how much a vector of interval objects overlaps with another. For each interval of the second vector, counts the sum of overlaps with the intervals of the first vector.  
#'
#' @param x Vector of intervals. To be overlapped with each interval of `y`.
#' @param y Vector of intervals that `x` overlap with.
#' @param proportional If FALSE, return the total overlap with each item of `y` in seconds. If TRUE (the default), return the total overlap as a proportion of the length of the intervals in `x`. (e.g. if half of an interval in `x` overlaps, add 0.5 to the sum)
#'
#' @return Numeric vector of same length as `y`. Gives the sum of overlap either in seconds (proportional=FALSE), or as proportion of `x`. 
#'
#' @examples
#' 
#' # save a one-day interval into `i`
#' i = as.interval(s="2000-01-01 00:00:00 UTC+00:00", e="2000-01-02 00:00:00 UTC+00:00")
#' 
#' # save four identical intervals in `x`
#' x = c(i, i, i, i)
#' 
#' # save the interval, and the interval + 12 hours, in `y`
#' y = c(i, i+12*3600)
#'
#' # proportional overlap of `x` with `y` 
#' # (gives 4 for y[1] = all four days in `x` overlapped entirely)
#' # (gives 2 for y[2] = all four days in `x` overlapped by half)
#' sum_overlap(x, y)
#' 
#' # overlap (in seconds) of `x` with `y`
#' # (gives 4*24 hours for y[1]) 
#' # (gives 4*12 hours for y[2])
#' sum_overlap(x, y)
#' 
#' @export
sum_overlap = function(x, y, proportional=TRUE){
	
	# initialise `ysum` (for sums of overlaps)
	ysum = rep(NA, length(y))
	
	# get sums, for each interval in `y`
	for (i in 1:length(y)){
		
		# get overlap with this interval
		over = overlap(x, y[i])
		
		# convert to proportional overlaps if asked to do so
		if (proportional){ 
			over = over / tdiff(x) 
		}
		
		# save the sum of overlaps in `ysum`
		ysum[i] = sum(over)
		
	}
	
	# return
	return(ysum)
	
}


#' Select items of an interval vector
#'
#' Called by R whenever items are selected by index in a vector of interval objects (e.g. `x[1]`). Not meant to be called by the user. Interval objects are basically a list with two items (`s`=start datetimes, `e`=end datetimes), not vectors, so the standard square brackets would not work properly.  
#'
#' @param x Vector of intervals.
#' @param i Indexes of items to select. Default is to select everything.
#'
#' @return Vector of intervals.
#' 
#' @method [ interval
#' @export
'[.interval' = function(x, i=1:length(x)){
	
	# get items of the start and end datetimes separately
	x$s = x$s[i]
	x$e = x$e[i]
	
	# return
	return(x)
	
}


#' Get length of interval
#'
#' Find out how many seconds there are between the start and end of an interval object. 
#'
#' @param x Vector of intervals.
#'
#' @return Numeric vector.
#'
#' @export
tdiff = function(x){
	
	# return
	return(x$e - x$s)
	
}


#' Save intervals into a vector
#'
#' Called by R whenever interval objects are saved by index into a vector of intervals (e.g. `x[2] = d`). Not meant to be called by the user. Interval objects are basically a list with two items (`s`=start datetimes, `e`=end datetimes), not actually vectors, so the standard code for saving would not work properly.  
#'
#' @param x Vector of intervals.
#' @param i Indexes of items where to save.
#' @param j Parameter called by R, but I'm not sure what, if anything, it does. Not used.
#' @param value interval(s) to be saved in `x`.
#'
#' @return Vector of intervals.
#' 
#' @method [<- interval
#' @export
'[<-.interval' = function(x, i, j="not used", value){
	
	#
	if (! is_interval(value)){
		stop(paste("Only intervals can be saved into an interval.", value, "is not an interval object."))
	}
	
	#
	x$s[i] = value$s
	x$e[i] = value$e
	
	# return
	return(x)
	
}