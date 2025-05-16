#' Calculate evenness indexes
#'
#' Estimate the evenness of species abundances with an index. Currently only a very rough guide for comparing rarefaction curves.
#'
#' @param n Vector of species abundances
#' @param q Integer giving the kind of index to return. Typically 1 or 2 (the default). See Details.
#' @param digits How many digits to round the returned index to.
#'
#' @details
#' There are a bewildering variety of evenness indices. This uses the classification of evenness indices in 
#' \href{https://doi.org/10.1111/j.1600-0706.2011.19897.x}{https://doi.org/10.1111/j.1600-0706.2011.19897.x}, 
#' where different values of `q` give a different index.
#' The index ranges from near 0 (very uneven) to 1 (all species have same abundance).
#' This function is currently very preliminary, and will likely be replaced with more complex evenness analyses in future versions of the package.
#' 
#' @return Index ranging from near 0 (almost all individuals belong to one species) to 1 (all species have the same abundance). 
#'
#' @examples
#'
#' # create uneven abundance data and plot it
#' n = c(sp1=10, sp2=9, sp3=7, sp4=3, sp5=2, sp6=1, sp7=1, sp8=1)
#' barplot(n, ylab="individuals")
#' 	
#' # create even abundance data and plot it
#' n2 = c(sp1=6, sp2=6, sp3=5, sp4=5, sp5=4, sp6=4, sp7=4, sp8=4)
#' barplot(n2, ylab="individuals")
#'
#' # estimate how even the abundances are
#' evenness(n)
#' evenness(n2)
#' 
#' @export
#'
evenness = function(n, q=2, digits=2){
	
	# ignore NA values
	n = n[!is.na(n)]
	
	# scale the abundances down to proportional abundances
	p = n / sum(n)
	
	# get the "effective number of species".. 
	if (q!=1){
		D = sum(p^q) ^ (1/(1-q))
		
	# .. if q=1, use the limit of the regular formula
	} else {
		D = exp(-sum(p*log(p)))
	}
	
	# calculate the index and round it
	e = D/length(p)
	e = round(e, digits)
		
	# return the rounded index
	return(e)
	
}
