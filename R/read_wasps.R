#' Read wasp data from file
#'
#' Read a csv file downloaded from Kotka (Kotka Collection Managemen System). This is basically a wrapper for [read.csv()]. Reads the column names ok (read.csv often corrupts them), removes extra header rows, and (optionally) simplifies the data. Simplification removes columns which are not needed, and renames other columns + adds several new columns so the data can be directly used by other functions.
#'
#' @param file Name of the file to read from. 
#' @param simplify If TRUE, convert the data to something more usable. Drop irrelevant columns, rename remaining columns, get forest type, trap etc from the sample data. 
#' @param ... Arguments passed to [read.csv]. 
#'
#' @return Data frame with the wasp data. If simplified, has the following columns:
#' * id Wasp's identifier, e.g. "ZMUT.53". (in short form, long form is e.g. "http://mus.utu.fi/ZMUT.53")
#' * sex F for female, M for male, U for unknown.
#' * taxon
#' * event
#' * forest_type
#' * site
#' * trap
#' * sample Malaise sample the wasp came from.
#' * start Date and time when the wasp's sample started to be collected.
#' * end Date and time when the wasp's sample stopped being collected.
#' * tdiff Number of days that the sample was collected.
#' * ecology_use True if the wasp can be used for ecological analyses. FALSE if its sample was damaged or otherwise unrepresentative of a normal catch.
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
	
	# simplify data if asked to do so
	if (simplify){
		
		# convert some columns (or column names) so they are more readable 
		id = paste0("ZMUT.", x$MYObjectID)
		sex = x$"MYGathering[0][MYUnit][0][MYSex]"
		taxon = x$"MYGathering[0][MYUnit][0][MYIdentification][0][MYTaxon]"
		event = x$"MYGathering[0][MYCollectingEventName]"
		
		# save the sample data in a short variable (makes next lines of code tidier)
		m = turkuwasps::malaise_sample
		
		# find each wasp's sample in the sample data
		samp = sub("http://mus.utu.fi/ZMUT.", "", x$"MYSeparatedFrom")
		i = match(samp, m$name)
		
		# overwrite `x` with more readable columns (or column names) + columns from the sample data
		x = data.frame(list(id=id, sex=sex, taxon=taxon, event=m$event[i], forest_type=m$forest_type[i], site=m$site[i], trap=m$trap[i], sample=m$name[i], start=m$start[i], end=m$end[i], tdiff=m$tdiff[i], ecology_use=!m$damaged[i]))	
		
		# mark NA values in "ecology_use" as not usable for ecological analyses (typically hand-netted wasps)
		x$ecology_use[is.na(x$ecology_use)] = FALSE
		
	}	
	
	# return
	return(x)
}