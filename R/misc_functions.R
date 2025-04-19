# Miscellaneous functions which are used by other functions, but can be useful for the user too.

#' Get factor levels
#'
#' Get the factor levels of a vector. If the vector is not a factor, factor it first (i.e. equivalent to calling `levels(factor(x))`). Basically a more readable wrapper for [levels()] and [factor()].
#'
#' @param x Vector whose factor levels are wanted.
#' @param ... Other arguments passed to [factor()] if `x` is not already a factor.
#'
#' @return Character vector giving the levels of `x`. 
#'
#' seealso [nlevels0()], [relevel0()]
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
#' seealso [levels0()], [relevel0()]
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
#' seealso [nlevels0()], [levels0()]
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

