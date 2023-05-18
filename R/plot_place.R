#' Plot wasps per place
#'
#' Barplot the number of wasps caught in each trap, forest type, or other location. This is basically a wrapper for two base R functions: [table()] for counting how many wasps belong to each bar, and [barplot()] to draw the plot.
#'
#' @param x Vector of which trap, forest type or other location each wasp came from. Either as strings or factor. (If factor, and `defaults`=FALSE, one bar is plotted for each factor level, in the same order as the levels)
#' @param m Data frame with the Malaise sample data, if wanting to scale the bars by sampling effort. Must contain columns "tdiff" (sampling effort) and one of "forest_type", "site" or "trap" (whatever is being plotted). If NULL, bars show the number of  wasps without taking sampling effort into account.
#' @param taxon Vector giving the taxon of each wasp. If given, plots each taxon separately.
#' @param defaults If TRUE, uses default settings for what bars to show, and for the order and colour of the bars. For example, if `x` contains Ugandan traps, draws one bar for each Ugandan trap, in successional order (primary forest to farm), with primary forest in dark green, swamp in blue etc.
#' @param ...  Graphical parameters and other arguments passed to [barplot()]. These will override any default values such as colours. Colours (argument `col`) are for each bar (but see "Details" for what happens if `beside` is set to FALSE by the user.). 
#'
#' @details The plot will look different depending on whether taxa (argument `taxon`) were given or not. If they were not, a fairly standard barplot is drawn, with one bar for each location (trap, forest type, site..). If taxa are given, one set of such bars will be drawn for each taxon: i.e. first draw the bars of taxon 1, then to the right of that the bars of taxon 2, etc.
#' 
#' Argument `beside` can be set to FALSE by the user, but doing so will change what the plot looks like. One bar will be plotted for each location, and the bars will be split/stacked by taxon. (If taxa were not given, all wasps are assumed to belong to the same taxon.) Colours will be for each taxon, not for each bar. (Default taxon colours will be used if `defaults` is TRUE) Normally it is best to leave argument `beside` alone. 
#' 
#' @return List with items `x` (x coordinates of the bars) and `y` (heights of the bars), returned silently. Save these to variable to continue drawing on the barplot. Both `x` and `y` are taxon * location tables, with as many rows as there are taxa, and  as many columns as there are locations. 
#' 
#' @seealso [default_legend()] for placing a default legend on this plot.
#'
#' @examples
#' # get example wasp data
#' f = system.file("extdata", "wasps_example.csv", package = "turkuwasps", mustWork = TRUE)
#' wasps = read_wasps(f)
#' 
#' # remove damaged samples and their wasps
#' tmp = ecology_usable(wasps)
#' x = tmp$wasps
#' m = tmp$samples
#' 
#' # plot
#' plot_place(x$trap, m)
#' 
#' # plot each species separately
#' plot_place(x$forest_type, m, taxon=x$taxon)
#' 
#' @export
plot_place = function(x, m=NULL, taxon=NULL, defaults=TRUE, ...){
	
	# set taxon of all wasps to "" if no taxa were given	
	if (is.null(taxon)){
		taxon = rep("", length(x))
	}
	
	# store various default arguments for the barplot
	barplot_args = list(
		beside = TRUE,
		cex.names = 0.6, 
		las = 2,
		space = 0.2,
		width = 5/6
	)
		
	# add the barplot arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	barplot_args[names(user_args)] = user_args
	
	# adjust bar spacing if `beside`is TRUE (and user did not give spacing)
	if (barplot_args$beside & is.null(user_args$space)){
		if (all(taxon == "")){
			barplot_args$space = c(0, 0.2)	
		} else {
			barplot_args$space = c(0, 1)
		}	
	}
	
	# get default bar order and colour if asked to do so
	if (defaults){
		
		# get default bars and add them as factor levels to `x`
		d = default_bars(x)
		x = d$x
		
		# add default colours to arguments (unless user gave colours)
		if (is.null(barplot_args$col)){
			
			# add default colours for locations..
			if (barplot_args$beside){
				barplot_args$col = d$colour	
				
			# .. or default colours for taxa
			} else {
				barplot_args$col = default_colours(nlevels0(taxon))
			}
			
		}
		
	}
	
	# check if bars are to be grouped by taxon (affects how bar coordinates and heights are stored)
	groupbytaxon = barplot_args$beside & any(taxon != "")
	
	# get bar heights by counting the wasps, split by taxon
	height = table(x, taxon)
	
	# scale by sampling effort if asked to do so
	if (! is.null(m)){
		barnames = levels0(x)
		weight = get_weights(barnames, m)
		height = height * weight
	}
			
	# if grouping by taxon, give `barplot` the heights with taxa in columns and locations in rows..
	if (groupbytaxon){
		barplot_args["height"] = list(height)
		
	# .. otherwise, with locations in columns and taxa in rows
	} else {
		barplot_args["height"] = list(t(height))
	}
	
	# barplot the wasps
	tmp = do.call(graphics::barplot, args=barplot_args)
	
	# store the x coordinates of the bars in a table that matches `height`
	xcoords = height
	xcoords[] = tmp
	
	# transpose so columns give locations, rows taxa
	xcoords = t(xcoords)
	height = t(height)
	
	# return the x and y coordinates of the bars invisibly
	invisible(list(x=xcoords, y=height))
}