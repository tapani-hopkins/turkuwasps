# Miscellaneous utility functions. These are useful in their own right, even outside package 'turkuwasps'. Used a lot by the functions in the package 'turkuwasps'.


#' Print text as italic
#'
#' Make a character vector (typically species names) display in italic font. Currently makes all the text italics. To mix italic and non-italic font, you will need to mess about with stuff like e.g. `expression("Species one "*"n=199")` or function [bquote()].
#'
#' @param x Character vector which is to be converted to italic.
#'
#' @return Vector which when displayed in a plot, is in italics. 
#'
#' @export
#'
as_italic = function(x){
	
	# create temporary function which makes *one* vector item italic
	to_italic = function(y){
		bquote(italic(.(y)))
	}
	
	# apply the temporary function to all items in the vector
	x = as.expression(lapply(x, to_italic))
	
	# return
	return(x)
	
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


#' Save plots as PNG
#'
#' Save plots to file as a PNG image. This is a wrapper for [png()], with default settings for resolution, dimensions, transparency etc.  
#'
#' @param f File path where to save the PNG file.
#' @param width Width of the plot in mm.
#' @param height Height of the plot in mm.
#' @param bg Background colour. Typically "transparent" or "white" (the default). 
#' @param res Resolution as an integer. It's slightly uncertain exactly how [png()] handles this when using mm as units, but the bigger this number the higher the resolution.
#' @param what Plotting commands. The plot created by these will be saved to PNG. See examples for how to write these.  
#'
#' @examples
#' \dontrun{
#'
#' # save plot commands..
#' figure = expression( {
#' 	barplot(1:10, space=0, col=1:10)
#' 	legend("topleft", legend=1:10, fill=1:10)
#' } )
#'
#' # .. then save to file
#' save_png("example.png", what=figure)
#' 
#' # alternatively, you can write the commands inside the function
#' save_png("example.png", what={
#' 	barplot(1:10, space=0, col=1:10)
#' 	legend("topleft", legend=1:10, fill=1:10)
#' })
#' 
#' # another alternative is to run as a function
#' figure1 = function(){
#' 	barplot(1:10, space=0, col=1:10)
#' 	legend("topleft", legend=1:10, fill=1:10)
#' }
#'
#' save_png("example.png", what=figure1())
#'
#' }
#' 
#' @export
save_png = function(f="turkuwasp_image.png", width=164, height=140, bg="white", res=300, what){
	
	# start saving to png file with default settings
	grDevices::png(filename=f, width=width, height=height, units="mm", bg=bg, res=res)
	
	# do plotting commands
	eval(what)
	
	# save png
	grDevices::dev.off()
	
}


#' Add together all the numbers that belong to the same category
#'
#' Split a numeric vector into categories and get the sum of each category. This is basically a more readable wrapper for [aggregate()].
#'
#' @param x Vector of numbers to add together.
#' @param by Vector of same length as `x`, giving the category that each number belongs to
#' @param ... Other arguments passed to [aggregate()], which mostly passes them on to [sum()].
#'
#' @return Vector of the sums for each category. Named vector, categories are used as names. 
#'
#' @examples
#' # save two vectors giving the number of wasps caught in different places, and the place names
#' n = c(1, 2, 3, 4, 5, 6)
#' place = c("forest", "swamp", "farm", "forest", "swamp", "farm")
#'
#' # get the total number of wasps in each place
#' sum_by(n, by=place)
#' 
#' @export
#'
sum_by = function(x, by, ...){
	
	# aggregate 
	m = stats::aggregate(x, list(by), FUN=sum, ...)
	
	# convert to vector
	X = m[, 2]
	names(X) = m[, 1]
	
	# return
	return(X)
	
}