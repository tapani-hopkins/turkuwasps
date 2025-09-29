#' Get data that is usable in ecological analyses
#'
#' Get the wasp and sample data that can be used in ecological analyses. Removes samples that were damaged in the field, and any other samples marked by the user as being unusable (typically because the wasp jar rotted). Removes wasps from these samples, and also any wasps that have no data on what trap they came from.
#'
#' @param x Data frame with the wasp data. Must contain columns "sample" and "trap", which give the Malaise sample and trap that each wasp came from. 
#' @param unusable Character vector giving any additional samples that are unusable. Case sensitive (e.g. "CCT1-141022", not "cct1-141022"). 
#'
#' @return List with four elements:
#' * `wasps` The wasp data without wasps that came from unusable samples.
#' * `samples` The sample data (i.e. data frame [malaise_sample]) without unusable samples.
#' * `removed_wasps` The row numbers of wasps which were removed as unusable. Useful if wanting to go back to the raw data, to see which wasps were kept and which filtered out.
#' * `removed_samples` The row numbers of samples which were removed as unusable. Useful if wanting to go back to the raw data, to see which samples were kept and which filtered out.
#' @export
ecology_usable = function(x, unusable=""){
	
	# get malaise sample data and add unusable samples to `unusable`
	m = turkuwasps::malaise_sample
	unusable = c(unusable, m$name[m$damaged])
	
	# get wasps that came from unusable samples or had an empty value for the trap (typically hand-netted wasps)
	i0 = which(x$sample %in% unusable)
	i1 = which(is.na(x$trap) | x$trap == "")
	i_x = union(i0, i1)
	
	# remove unusable wasps
	x = x[-i_x, ]
	
	# remove unusable samples
	i_m = which(m$name %in% unusable)
	m = m[-i_m, ]
	
	# return
	return(list(wasps=x, samples=m, removed_wasps=i_x, removed_samples=i_m))
	
}


#' Read Kotka file
#'
#' Read a csv file downloaded from Kotka (Kotka Collection Management System). This is basically a wrapper for [read.csv()]. Reads the column names ok (read.csv often corrupts them), removes extra header rows, and (optionally) simplifies the data. Simplification removes columns which are not needed, and renames other columns + adds several new columns so the data can be directly used by other functions.
#'
#' @param file Name of the file to read from. 
#' @param simplify If TRUE, convert the data to something more usable. Drop irrelevant columns, rename remaining columns, get forest type, trap etc from the sample data. 
#' @param columns Named character vector giving the name of columns in the Kotka file that you want to keep if simpifying the data. For example, `columns=c(location="MYDocumentLocation", notes="MYNotes")` adds the columns "MYDocumentLocation" and "MYNotes" to the simplified data, renaming them to "location" and "notes". Will overwrite any standard columns of the same name. If NULL, only the standard columns "id", "sex" etc will be returned.
#' @param ... Arguments passed to [read.csv()]. 
#'
#' @return Data frame with the wasp data. If simplified, has the following columns:
#' * id Wasp's identifier, e.g. "ZMUT.53". (in short form, long form is e.g. "http://mus.utu.fi/ZMUT.53")
#' * sex F for female, M for male, U for unknown.
#' * taxon Taxon, taken from the first column with an identification. If there are several identifications, this may need overwriting.
#' * event Collecting event, e.g. "Uganda 2014-2015".
#' * forest_type Habitat type the wasp came from, if relevant.
#' * site Site the wasp came from, if relevant.
#' * trap Trap the wasp came from.
#' * sample Sample the wasp came from. Usually a Malaise trap sample.
#' * start Date and time when the wasp's sample started to be collected.
#' * end Date and time when the wasp's sample stopped being collected.
#' * tdiff Number of days that the sample was collected.
#' * season Season when the sample was collected.
#' * ecology_use True if the wasp can be used for ecological analyses. FALSE if its sample was damaged or otherwise unrepresentative of a normal catch. Rarely used, since [ecology_usable()] will typically be used to remove unusable wasps before analyses.
#' @export
read_kotka = function(file, simplify=TRUE, columns=NULL, ...){
	
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
		
		# save a backup of the Kotka format data
		x_kotka = x
		
		# convert some columns (or column names) so they are more readable 
		id = paste0("ZMUT.", x$MYObjectID)
		sex = x$"MYGathering[0][MYUnit][0][MYSex]"
		taxon = x$"MYGathering[0][MYUnit][0][MYIdentification][0][MYTaxon]"
		event = x$"MYGathering[0][MYCollectingEventName]"
		
		# save the sample data in a short variable (makes next lines of code tidier)
		m = turkuwasps::malaise_sample
		
		# find each wasp's sample in the sample data
		i = match(x$"MYSeparatedFrom", m$kotka_id)
		
		# overwrite `x` with more readable columns (or column names) + columns from the sample data
		x = data.frame(list(id=id, sex=sex, taxon=taxon, event=m$event[i], forest_type=m$forest_type[i], site=m$site[i], trap=m$trap[i], sample=m$name[i], start=m$start[i], end=m$end[i], tdiff=m$tdiff[i], season=m$season[i], ecology_use=!m$damaged[i]))	
		
		# mark NA values in "ecology_use" as not usable for ecological analyses (typically hand-netted wasps)
		x$ecology_use[is.na(x$ecology_use)] = FALSE
		
		# add the columns the user asked for, renaming them to what was asked for
		if (! is.null(columns)){
			x[, names(columns)] = x_kotka[, columns]
		}
		
	}	
	
	# return
	return(x)
}


#' Read wasp data from Kotka file
#'
#' Read wasp data from a csv file downloaded from Kotka (Kotka Collection Management System), and return the wasp data and its corresponding sample data. This tidies up the data into a standard format, e.g. removing extra columns and renaming columns, and getting the wasps' forest type, trap etc from the sample data. Also (optionally) filters out samples that were damaged or otherwise unrepresentative of a normal catch, and the wasps from those samples.
#'
#' @param file Name of the file to read from. 
#' @param ecology_usable If TRUE, any samples that are unrepresentative of a normal catch will be filtered out, and so will wasps from those samples. Such samples are typically samples that were damaged during collecting (e.g. trampled by elephants). Use parameter `unusable` to filter out more samples, e.g. if a sample rotted before you had time to separate the taxon you are currently dealing with. If FALSE, samples will not be filtered.
#' @param unusable Character vector giving any additional samples that are unusable. Case sensitive (e.g. "CCT1-141022", not "cct1-141022"). Only has an effect if `ecology_usable=TRUE`.
#' @param columns Named character vector giving the name of columns in the Kotka file that you want to keep. For example, `columns=c(location="MYDocumentLocation", notes="MYNotes")` adds the columns "MYDocumentLocation" and "MYNotes" to the data, renaming them to "location" and "notes". Will overwrite any standard columns of the same name. If NULL, only the standard columns "id", "sex" etc will be returned. Useful e.g. if the species names of the wasps are in an unusual column.
#' @param factor If TRUE, convert columns "forest_type", "site" and "trap" to factor, with the factor levels in default order. Especially useful for the Ugandan data: this makes sure the traps are plotted in the correct successional order instead of e.g. alphabetical order.
#' @param ... Arguments passed to [read.csv()], which does the basic reading of the csv file. 
#'
#' @seealso [read_kotka()] if you just want to read a Kotka csv file without tidying it up, [ecology_usable()] for filtering out unrepresentative samples and their wasps. This function is basically a wrapper for these two functions.
#'
#' @return List with items `x`, a data frame with the wasp data, and `m`, a data frame with the sample data. The sample data has the same columns as data frame [malaise_sample], the wasp data has the following columns:
#' * id Wasp's identifier, e.g. "ZMUT.53". (in short form, long form is e.g. "http://mus.utu.fi/ZMUT.53")
#' * sex F for female, M for male, U for unknown.
#' * taxon Taxon, taken from the first column with an identification. If there are several identifications, this may need overwriting.
#' * event Collecting event, e.g. "Uganda 2014-2015".
#' * forest_type Habitat type the wasp came from, if relevant.
#' * site Site the wasp came from, if relevant.
#' * trap Trap the wasp came from.
#' * sample Sample the wasp came from. Usually a Malaise trap sample.
#' * start Date and time when the wasp's sample started to be collected.
#' * end Date and time when the wasp's sample stopped being collected.
#' * tdiff Number of days that the sample was collected.
#' * season Season when the sample was collected.
#' * ecology_use True if the wasp can be used for ecological analyses. FALSE if its sample is marked in dataset [malaise_sample] as damaged or otherwise unrepresentative of a normal catch. Rarely used, since parameter `ecology_usable` will typically be used to remove unusable wasps before analyses.
#' @export
read_wasps = function(file, ecology_usable=TRUE, unusable="", columns=NULL, factor=TRUE, ...){
	
	# get the wasp data
	x = read_kotka(file, simplify=TRUE, columns=columns, ...)
	
	# get those samples (and their wasps) which are usable in ecological analyses if asked to so..
	if (ecology_usable){
		wasps = ecology_usable(x, unusable=unusable)
		x = wasps$wasps
		m = wasps$samples
		
	# .. otherwise get all malaise samples
	} else {
		m = turkuwasps::malaise_sample
	}
		
	# remove samples that come from events for which no wasps were collected
	m = m[m$event %in% x$event, ]
	
	# convert trap, forest type etc columns to factor if asked to do so
	# put the factor levels in the default order, same order as in datasets `trap`, `forest_type` etc
	if (factor){
		
		# get the default forest type, site etc data
		forest_type = turkuwasps::forest_type
		site = turkuwasps::site
		trap = turkuwasps::trap
		
		# factor wasp data, only the factor levels of events from which wasps were collected
		x$forest_type = factor(x$forest_type, levels=forest_type$name[forest_type$event %in% x$event])
		x$site = factor(x$site, levels=site$name[site$event %in% x$event])
		x$trap = factor(x$trap, levels=trap$name[trap$event %in% x$event])
		
		# factor sample data, only the factor levels of events from which wasps were collected
		m$forest_type = factor(m$forest_type, levels=forest_type$name[forest_type$event %in% x$event])
		m$site = factor(m$site, levels=site$name[site$event %in% x$event])
		m$trap = factor(m$trap, levels=trap$name[trap$event %in% x$event])
		
	}
	
	# return
	return(list(x=x, m=m))
	
}
