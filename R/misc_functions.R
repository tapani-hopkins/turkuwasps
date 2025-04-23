# Miscellaneous functions. Many are used by other functions, but can also be useful for the user.


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


#' Get factor levels
#'
#' Get the factor levels of a vector. If the vector is not a factor, factor it first (i.e. equivalent to calling `levels(factor(x))`). Basically a more readable wrapper for [levels()] and [factor()].
#'
#' @param x Vector whose factor levels are wanted.
#' @param ... Other arguments passed to [factor()] if `x` is not already a factor.
#'
#' @return Character vector giving the levels of `x`. 
#'
#' @seealso [nlevels0()], [relevel0()]
#'
#' @examples
#' # 'x' contains three different forest types: primary, disturbed, or farm
#' x = c("primary", "disturbed", "primary", "primary", "farm")
#' levels0(x)
#'
#' @export
#'
levels0 = function(x, ...){
	
	# get factor levels
	res = levels(x)
	
	# if there are no factor levels, factor `x` first
	if (is.null(res)){
		res = levels(factor(x, ...))
	}
	
	# return
	return(res)
}


#' Get number of factor levels
#'
#' Get the number of factor levels of a vector. If the vector is not a factor, factor it first. Equivalent to [nlevels()], except handles vectors which are not a factor.
#'
#' @param x Vector whose number of factor levels is wanted.
#' @param ... Other arguments passed to [factor()] if `x` is not already a factor.
#'
#' @return Number of levels of `x`. 
#'
#' @seealso [levels0()], [relevel0()]
#'
#' @examples
#' # 'x' contains three different forest types: primary, disturbed, or farm
#' x = c("primary", "disturbed", "primary", "primary", "farm")
#' nlevels0(x)
#'
#' @export
#'
nlevels0 = function(x, ...){
	
	# get number of factor levels
	res = length(levels0(x, ...))

	# return
	return(res)
}


#' Relevel levels of a factor or vector
#'
#' Move one of a vector's factor levels to first place. If the vector is not a factor, factor it first. Equivalent to [relevel()], except handles vectors which are not a factor.
#'
#' @param x Vector.
#' @param ref Which level to move to first place. Usually character string.
#'
#' @return Vector `x` with the factor levels placed in a new order. 
#'
#' @seealso [nlevels0()], [levels0()]
#'
#' @examples
#' # 'x' contains three different forest types: primary, disturbed, or farm
#' x = c("primary", "disturbed", "primary", "primary", "farm")
#' levels0(x)
#'
#' # reorder the levels so that primary is shown first
#' x = relevel0(x, "primary")
#' levels0(x)
#'
#' @export
#'
relevel0 = function(x, ref){
	
	# make sure `x` is a factor
	if (! is.factor(x)){
		x = factor(x)
	}
	
	# call default relevel
	res = stats::relevel(x, ref)
	
	# return
	return(res)
	
}

