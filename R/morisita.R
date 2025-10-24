
#' Get the Morisita dissimilarity between places
#'
#' Get Morisita indexes showing how dissimilar the species assemblages of different forest types, traps etc were. Handy way of getting a rough overview of what places have similar species in similar proportions. See Horn (1966) for a decription of the index.
#'
#' @param x Table of how many individuals of each species were caught in each place. Places in rows, each species in its own column.
#' @param digits How many digits to round the index to. Integer, or if NA the indexes are not rounded.
#'
#' @return Matrix of Morisita dissimilarities between the places, each dissimilarity ranging from 0 (no difference between the two places) to 1 (completely different places). 
#'
#' @note The dissimilarities are approximate, and very small near-zero values may be off by a few hundredths because of rounding errors. The function replaces small negative values (anything between -0.04 and 0) with 0.
#' 
#' @section References:
#' * Horn HS 1966. Measurement of ”Overlap” in Comparative Ecological Studies. The American Naturalist 100: 419–424. <http://www.jstor.org/stable/2459242>
#' 
#' @examples
#' # get path to example wasp data
#' f = system.file("extdata", "wasps_example.csv", package = "turkuwasps", mustWork = TRUE)
#' 
#' # read the wasp data
#' tmp = read_wasps(f)
#' x = tmp$x
#' 
#' # get how many wasps of each species were in each forest type, and show the dissimilarities
#' M = table(x$forest_type, x$taxon)
#' morisita(M)
#' 
#' @export
#' 
morisita = function(x, digits=2){
	
	# get the names of the places from the row names
	places = rownames(x)
	
	# create an empty matrix for the dissimilarities
	m = matrix(0, nrow=nrow(x), ncol=nrow(x), dimnames=list(places, places))
	
	# cycle through each combination of places and get its dissimilarity
	combinations = utils::combn(places, 2, simplify=FALSE)
	for (i in combinations){
		d = get_morisita(x[i, ])
		m[i[1], i[2]] <- m[i[2], i[1]] <- d
	}
	
	# set small rounding errors to zero
	m[m < 0 & m >= -0.04] = 0
	
	# round the results to make them more readable
	if (! is.na(digits) ){
		m = round(m, digits)
	}
	
	# return dissimilarity matrix
	return(m)
	
}

#' Get the Morisita dissimilarity between two places
#'
#' Get Morisita indexes showing how dissimilar the species assemblages of two different places were. Mainly a helper function used by function [morisita()].
#'
#' @param x Table of how many individuals of each species were caught in both places. Two rows (one for each place), each species in its own column.
#'
#' @return Morisita dissimilarity, ranging from 0 (no difference between the two places) to 1 (completely different places). 
#'
get_morisita = function(x){
	
	# get the abundance per species and total abundance at both places
	x1 = x[1, ]
	x2 = x[2, ]
	n1 = sum(x1)
	n2 = sum(x2)

	# count the Simpson's indices
	l1 = sum( x1 * (x1 - 1) ) / ( n1 * (n1 - 1) )
	l2 = sum( x2 * (x2 - 1) ) / ( n2 * (n2 - 1) )

	# count the dissimilarity
	d = 1 - 2 * sum( x1 * x2 ) / ( n1 * n2 ) / ( l1 + l2 )
	
	# return
	return(d)
}