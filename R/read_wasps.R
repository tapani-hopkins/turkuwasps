#' Read wasp data from file
#'
#' Read a csv file downloaded from Kotka (Kotka Collection Managemen System). This is basically a wrapper for [read.csv()]. Preserves the column names, removes extra header rows, and (optionally) simplifies the columns. Simplification removes columns which are not needed, and renames other columns + adds several new columns so the data can be directly used by other functions.
#'
#' @param file Name of the file to read from. 
#' @param simplify If TRUE, drop irrelevant columns such as notes. 
#' @param ... Arguments passed to [read.csv]. 
#'
#' @return Data frame with the wasp data. If simplified, has the following columns:
#' * id
#' * event
#' * forest_type
#' * trap
#' * sample
#' @export
read_wasps = function(file, simplify=TRUE, ...){
	
	# store default arguments for read.csv
	read_args = list(
		check.names = FALSE, 
		as.is = TRUE
	)
	
	# add arguments given by the user (overwrite defaults if need be)
	user_args = c(list(file=file), list(...))
	read_args[names(user_args)] = user_args
	
	# read the file
	x = do.call(utils::read.csv, args=read_args)
	
	# drop the second header row
	x = x[-1, ]
	
	# xxx get identifier, event, forest type, trap, sample, datetime start and end, sex, species, (xxx problems dataset?)
	X = x
	if (simplify){
		
		#
		msample = sub("http://mus.utu.fi/ZMUT.", "", x$"MYSeparatedFrom")
		i = match(msample, malaise_sample$name)
		
		X$id = paste0("ZMUT.", x$MYObjectID)
		X$event = x$"MYGathering[0][MYCollectingEventName]"
		
		X = X[, (ncol(x)+1):ncol(X) ]
	}
	
	# xxx get problems (wasps which can't be used for ecological analyses)
	
	
	# return
	return(X)
}