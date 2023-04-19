#' Get the weights for scaling barplots
#'
#' Get how much to scale the bars of [plot_place()] by. Typically used to count the total sampling effort of e.g. each trap, and scale down the bars by sampling effort.
#'
#' @param tdiff Vector of sampling efforts (typically of each Malaise sample).
#' @param group Vector of same length as `tdiff`, giving the group (e.g. trap) that each sampling effort belongs to.
#' @param ... Other arguments passed to [sum_by], which passes them on to [aggregate()], which mostly passes them on to [sum()].
#'
#' @return Vector of the sums for each category. Named vector, groups are used as names. 
#'
#' @examples
#' # get sample data (with damaged samples removed)
#' m = turkuwasps::malaise_sample 
#' m = ecology_usable(m)$samples
#' 
#' # get weights of each trap
#' get_weights(m$tdiff, m$trap) 
#' 
#' # get weights of each forest type
#' get_weights(m$tdiff, m$forest_type) 
#' 
#' @export
get_weights = function(tdiff, group, ...){
	
	# get total sampling effort of each grouping, and divide by it to get weights
	x = 1 / sum_by(tdiff, group, ...)
	
	# return
	return(x)
}