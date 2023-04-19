#' Get data that is usable in ecological analyses
#'
#' Get the wasp and sample data that can be used in ecological analyses. Removes samples that were damaged in the field, and any other samples marked by the user as being unusable (typically because the wasp jar rotted). Removes wasps from these samples, and also any wasps that have no data on what trap they came from. 
#'
#' @param x Data frame with the wasp data. Must contain columns "sample" and "trap", which give the Malaise sample and trap that each wasp came from. 
#' @param unusable Character vector giving any additional samples that are unusable. Case sensitive (e.g. "CCT1-141022", not "cct1-141022"). 
#'
#' @return List with two elements:
#' * `wasps` The wasp data without wasps that came from unusable samples.
#' * `samples` The sample data (i.e. data frame [malaise_sample]) without unusable samples.
#' @export
ecology_usable = function(x, unusable=""){
	
	# get malaise sample data and add unusable samples to `unusable`
	m = turkuwasps::malaise_sample
	unusable = c(unusable, m$name[m$damaged])
	
	# remove wasps that came from unusable samples
	i = which(! x$sample %in% unusable)
	x = x[i, ]
	
	# remove wasps with empty values for the trap (typically hand-netted wasps)
	i = which(! is.na(x$trap) & x$trap!="")
	x = x[i, ]
	
	# remove unusable samples
	i = which(! m$name %in% unusable)
	m = m[i, ]
	
	# return
	return(list(wasps=x, samples=m))
	
}