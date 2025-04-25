#' Draw rarefaction curves
#'
#' Helper function used by [plot_rarefaction()]. Draws rarefaction curves created by [get_rarefaction()].
#' 
#' @param r List of coordinates for the curves returned by [get_rarefaction()]. Should have the X coordinates of the curve, and the average, max and min Y coordinates of the curve.
#' @param ci If TRUE, confidence intervals are shown around the rarefaction curve. Default is to only show the curve without confidence intervals.
#' @param add If TRUE, the rarefaction curve is added to an existing plot. Default is to create a new plot.
#' @param pch What symbols to use on the curve. Typically an integer between 0:18. See [points()] for accepted values. Default is for the curve to be drawn without symbols.
#' @param pch_col Colour to be used for the symbols.
#' @param ...  Graphical parameters passed to the two functions which draw the curves, [plot()] and [lines()]. These will override any default values such as colours. A few parameters (such as 'type' and 'pch') may not work as expected.
#' 
#' @keywords internal
#' 
draw_rarefaction = function(r, ci=FALSE, add=FALSE, pch=NULL, pch_col=NULL, ...){
	
	# store various default arguments for the plot
	plot_args = list(
		col = "black",
		lty = 1,
		lwd = 1.5,
		xlab = "wasps",
		ylab = "species"
	)
	
	# add the plot arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	plot_args[names(user_args)] = user_args
	
	# draw the points in the same colour as the curve, unless the user gave a colour
	if (is.null(pch_col)){
		pch_col = plot_args$col
	}
	
	# add the curve's coordinates to the plot arguments
	plot_args["x"] = list(r$x)
	plot_args["y"] = list(r$y)
	
	# create a new blank plot unless adding to an existing plot
	if (!add){
		plot_args2 = plot_args
		plot_args2["type"] = list("n")
		do.call(plot, plot_args2)
	}
	
	# draw the rarefaction curve
	do.call(graphics::lines, args=plot_args)
	
	# add symbols to the curve if 'pch' was given by the user
	if (!is.null(pch)){
		i = floor(seq(1, length(r$x), length.out=5))
		graphics::points(r$x[i], r$y[i], pch=pch, cex=0.6, col=pch_col)
	}
	
	# draw the upper and lower limits of the curve
	if (ci){
		
		# make a transparent colour (9% opacity) for the upper and lower limits
		oldcol = grDevices::col2rgb(plot_args$col, alpha=TRUE)
		nc = oldcol * c(1, 1, 1, 0.09)  # 9% opacity
		nc = grDevices::rgb(nc[1], nc[2], nc[3], nc[4], maxColorValue=oldcol[4])
	
		# draw as a polygon
		graphics::polygon( c(r$x, rev(r$x)) , c(r$y_min, rev(r$y_max)), border=NA, col=nc)
		
	}
	
}



#' Get rarefaction curves
#'
#' Helper function used by [plot_rarefaction()]. Randomly resamples wasp data to create rarefaction curves, and takes the average, minimum and maximum of the curves.
#'
#' @param x Data frame containing the wasp data. Must contain columns "taxon" and "sample". Each row is an individual wasp.
#' @param n Number of resamples. Default (10) is fast, but gives very jagged curves. Increase to e.g. 100 to get smooth averaged out curves.
#' @param  p How large a part of the resampled curves to show in confidence intervals. Default (0.84) shows where 84% of the resampled curves fell. Used to estimate if two rarefaction curves are significantly different (e.g. for a significance of 0.05, check if the confidence intervals of two 0.84 curves overlap).
#'
#' @return List with the x coordinates (=number of wasps) of the curves, and the average, min and max y coordinates (number of species). Min and max values are for the interval given by `p`.
#'
#' @keywords internal 
#'
get_rarefaction = function(x, n=10, p=0.84){
	
	# save x coordinates of the rarefied curves
	X = 0:nrow(x)
	
	# create blank matrix for storing the y coordinates of each rarefied curve
	Y = matrix(NA, nrow=n, ncol=length(X))
	
	# set the start of each curve to 0
	Y[, 1] = 0
	
		
	# rarefy the curves
	for (i_n in 1:n){
		
		# sort the samples into random order
		samples = sample(levels0(x$sample))
	
		# save the x and y coordinates of this curve
		# X0 <- Y0 <- rep(NA, length(samples))
		for (i in 1:length(samples)){
			
			# get the taxon of each of the individuals accumulated up to this point
			taxon = x[x$sample %in% samples[1:i], "taxon"]
			
			# count how many individuals and how many taxa have accumulated
			X0 = length(taxon)
			Y0 = nlevels0(taxon)
			
			# store into Y
			Y[i_n, which(X==X0)] = Y0
			
		}		
	
		# get the indices of missing values in this curve, and of non-missing values
		na = which(is.na(Y[i_n, ]))
		nna = which(! is.na(Y[i_n, ]))
		
		# interpolate the missing values
		# e.g. if the first sample had three wasps and one species (x=3, y=1), 
		# and the origin is (x=0, y=0), 
		# then interpolate the points in between them (x=1, y=0.33) (x=2, y=0.66)
		for (i_na in na){
			
			# get the index of the previous and next non-missing point
			i0 = i_na - 1
			i1 = min(nna[nna > i_na])
			
			# save the interpolated value
			Y[i_n, i_na] = Y[i_n, i0] + (Y[i_n, i1] - Y[i_n, i0]) / (i1 - i0)	
		
		}
			
	}
	
	# get the average of the curves
	Y_mean = colMeans(Y)
	
	# get the upper and lower limits of the curves
	# (e.g. if p=0.84, remove the outmost 8% of the curves at each point)
	Y_min <- Y_max <- rep(NA, ncol(Y))
	for (i in 1:ncol(Y)){
		Y_min[i] = stats::quantile(Y[, i], probs=(1-p)/2)
		Y_max[i] = stats::quantile(Y[, i], probs=1-(1-p)/2)
	}
	
	# return the curve coordinates
	return(list(x=X, y=Y_mean, y_min=Y_min, y_max=Y_max))	
	
}


#' Add legend to rarefaction plot
#'
#' Add legend to rarefaction plots. Basically a wrapper for [legend()], adapted to easily draw lines, symbols, several columns, column subtitles etc for rarefaction curve plots.
#' 
#' @param txt The legend text. Character vector, e.g. `c("primary", "swamp", ""disturbed", "clearcut")`. Legend texts will be in the same order as given here (though see `column`). NA values can be used if you want to leave some legend text blank. Either this or `r` must be given. If only `r` is given, `txt` will be got from it.
#' @param r List of curve coordinates and graphical parameters returned by [plot_rarefaction()]. If given, most parameters (e.g. `txt`, `col`, `pch`) will be taken from here. If you also give the same parameters separately, those will be the ones used. Either this or `txt` must be given. If you give both `r` and `txt`, do make sure that they are in the same order!
#' @param column What column each legend text should be placed in. Integer vector of same length as `txt`. E.g. `c(1, 2, 2, 2)` puts the first text in the first column, and the three others in a second column. It is OK to give these in a weird order (e.g. `c(2, 3, 2, 1)`), but trying to leave empty columns won't work (e.g. `c(1, 1, 3, 3)` which lacks column 2). Default is to not split the legend texts into several columns. 
#' @param col The colour of the lines. Vector of length 1, or of same length as `txt`. Should be in the same order as `txt`. NA values can be used if you don't want to draw a line, e.g. `c("darkgreen", "blue", NA, NA)` only draws the first two lines. If not given, all lines will be black.
#' @param pch The symbols to add on top of the lines. Vector of length 1, or of same length as `txt`. Should be in the same order as `txt`. Typically an integer between 0:18, see [points()] for accepted values. NA values can be used if you don't want to draw a symbol, e.g. `c(1, 2, NA, NA)` only draws the first two symbols. If not given, no symbols are drawn.
#' @param pch_col The colour of the symbols. Vector of length 1, or of same length as `txt`. Should be in the same order as `txt`. NA values can be used if you don't want to draw a symbol, e.g. `c("darkgreen", "blue", NA, NA)` only draws the first two symbols. If not given, symbols will be black.
#' @param pch_cex The size of the symbols. Vector of length 1, or of same length as `txt`. Should be in the same order as `txt`. Typically will be 0.5, i.e. 50% of the normal size for symbols in the plot. This is also the default.
#' @param title Overall title for the plot. Character string. Default is to not have a title.
#' @param colnames The subtitles to place over each column. Character vector with one subtitle for each column. Default is to not have subtitles.
#' @param ... Other arguments passed to [legend()]. Typically `x`, which gives the position of the legend (default is "bottomright"). Also, `fill` may sometimes be used to display confidence intervals. Graphical parameters such as `fill` should be vectors of length 1, or of same length as `txt`, in the same order as `txt`. Argument `legend` has no effect, and some rarely used graphical parameters may not work as intended if drawing several columns.
#' 
#' @return List of legend coordinates returned by [legend()], returned silently.
#' 
#' @examples
#' # get example wasp data
#' f = system.file("extdata", "wasps_example.csv", package = "turkuwasps", mustWork = TRUE)
#' wasps = read_wasps(f)
#' 
#' # plot rarefaction curves with separate curves for each forest type
#' col = c(primary="darkgreen", swamp="blue", disturbed="green", clearcut="yellow", farm="orange")
#' r = plot_rarefaction(wasps, by="forest_type", col=col, pch=1:4)
#' 
#' # add a basic legend with the same colours etc as in the plot
#' legend_rarefaction(r=r)
#' 
#' # add another, more complex, legend
#' cnames = c("undisturbed", "disturbed or cut")
#' legend_rarefaction(r=r, column=c(2, 2, 1, 1), colnames=cnames, title="Uganda", x="right")
#'
#'  
#' ## Separate dry and wet season
#'
#' # prepare a new column in the wasp data, for drawing separate curves for dry and wet season
#' # also see to it the curves will be drawn in successional order primary -> farm
#' levs = c("primary", "swamp", "disturbed", "clearcut", "farm")
#' wasps$forest_type = factor(wasps$forest_type, levels=levs)
#' wasps$forest_season = combine_columns(wasps, c("forest_type", "season"), all=TRUE)
#'
#' # plot rarefaction curves with separate curves for each forest type and season
#' col = c(primary="darkgreen", swamp="blue", disturbed="green", clearcut="yellow", farm="orange")
#' col = rep(col, each=2)
#' r = plot_rarefaction(wasps, by="forest_season", col=col, pch=rep(1:2, 5), xlim=c(0, 100))
#'
#' # add a legend with dry and wet season in separate columns
#' txt = rep(levels0(wasps$forest_type), each=2)
#' legend_rarefaction(txt=txt, r=r, column=rep(1:2, 5), colnames=c("dry", "wet"), title="Uganda")
#'
#' @export
#' 
legend_rarefaction = function(txt=NULL, r=NULL, column=NULL, col=NULL, pch=NULL, pch_col=NULL, pch_cex=0.5, title=NULL, colnames=NULL, ...){
	
	# get the legend texts from 'r' if they weren't given by the user
	if (is.null(txt)){
		if (is.null(r)){
			stop("'txt' or 'r' must be given.")
		} else {
			txt = names(r$r)
		}
	}
	
	# store various default arguments for the legend
	legend_args = list(
		border = NA,
		cex = 0.7,
		fill = NULL,
		lwd = 2,
		title = title,
		x = "bottomright"
	)
	
	# add the arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	legend_args[names(user_args)] = user_args
	
	# if graphical parameters weren't given by the user, get them from 'r' or use defaults
	if (is.null(col)){
		if (is.null(r)) { col = "black" } else { col = r$col }
	}
	if (is.null(pch)){
		if (is.null(r)) { pch = NA } else { pch = r$pch }
	}
	if (is.null(pch_col)){
		if (is.null(r)) { pch_col = "black" } else { pch_col = r$pch_col }
	}
	
	# add the line colour and legend texts to the legend arguments
	legend_args$col = col
	legend_args$legend = txt

	# if the legend is in one column, draw the legend and add points..
	if (is.null(column)) {

		# show the legend
		res = do.call(graphics::legend, args=legend_args)
	
		# get midpoints of the lines in the legend
		res_x = res$text$x - (res$text$x[1] - res$rect$left) / 2
		
		# add points to the lines of the legend
		graphics::points(res_x, res$text$y, pch=pch, col=pch_col, cex=pch_cex)
	
	# .. if several columns were asked for, draw legend and points in several columns and add titles
	} else {
		
		# get the number of columns and rows
		ncol = max(column)
		nrow = max(table(column))
		
		# add the number of columns to the legend arguments
		legend_args$ncol = ncol
		
		# repeat some graphical parameters so they're the same length as 'txt'
		rep0 = function(x){
			if(length(x) == 1){ return(rep(x, length(column))) } else { return(x) }
		}
		legend_args[c("border", "fill", "lwd")] = lapply(legend_args[c("border", "fill", "lwd")], rep0)
		pch = rep0(pch)
		pch_col = rep0(pch_col)
		pch_cex = rep0(pch_cex)
		
		# get the names of legend parameters that are vectors of the same length as 'txt'
		is_par = function(p){ length(p) == length(column) && !is.list(p) }
		adjustable = unlist(lapply(legend_args, is_par))
		adjustable = names(adjustable)[adjustable]

		# adjust the order of the parameters, so that the first column is added first, then the second column etc
		legend_args[adjustable] = lapply(legend_args[adjustable], function(p){ p[order(column)] } )
		pch = pch[order(column)]
		pch_col = pch_col[order(column)]
		pch_cex = pch_cex[order(column)]
		column = column[order(column)]
		
		# prepare to pad the ends of the rows with NA, by getting the indexes the legend texts, lines etc will have after padding
		i = NULL
		for (ic in 1:ncol){
			
			# get the indexes of this column's legend texts etc, and add to 'i'
			colindexes = 1:sum(column == ic) + (ic-1) * nrow
			i = c(i, colindexes)
			
		}
		
		# make a function for padding the ends of rows with NA
		adjust_par = function(p){
			P = rep(NA, nrow*ncol)
			P[i] = p
			return(P)
		}

		# pad the ends of rows with NA, so that each legend text, line etc goes in the right column
		legend_args[adjustable] = lapply(legend_args[adjustable], adjust_par)
		pch = adjust_par(pch)
		pch_col = adjust_par(pch_col)
		pch_cex = adjust_par(pch_cex)
		
		# add some space for the column names by adding spaces around the title
		legend_args$title = paste0("\n", legend_args$title, "\n")	
		
		# draw the legend
		res = do.call(graphics::legend, args=legend_args)
		
		# get midpoints of the lines in the legend
		res_x = res$text$x - (res$text$x[1] - res$rect$left) / 2

		# add points to the lines of the legend
		graphics::points(res_x, res$text$y, pch=pch, col=pch_col, cex=pch_cex)
		
		# get suitable places for the column names
		x_colnames = levels0(res$text$x) #res$text$x[c(1, length(res$text$x))]
		y_colnames = res$text$y[1] + 0.3 * (res$rect$top - res$text$y[1])
		
		# add column names
		graphics::text(x_colnames, y_colnames, colnames, cex=legend_args$cex)
		
	}
	
	# return the list of legend coordinates returned by 'legend()'
	invisible(res)
	
}


#' Match colour etc parameters to the right rarefaction curve
#'
#' Helper function used by [plot_rarefaction()]. Checks the length and names of parameters such as `col` and `pch`. If they are named vectors, makes sure they are in the same order as the curves that are being drawn. If not, checks the length. Only used if several curves are drawn.
#' 
#' @param x Parameter to check. Usually a vector, e.g. for the colour of the curves: 'c(primary="darkgreen", disturbed="green", swamp="blue", clearcut="red")'.
#' @param xname Character string telling what parameter is being checked. E.g. "col". Used for error messages.
#' @param levs Character vector giving the levels of the column that is being used to split the data into separate curves. The curves will be drawn in this order, and the parameter's values placed in the same order. E.g. 'c("clearcut", "disturbed", "primary", "swamp")'.
#' @param column_name Character string telling what column is being used to split the data into separate curves. Used for error messages.
#' 
#' @return Named vector of parameter values. The names tell which level each paramater value corresponds to. The vector is the same length and is in the same order as `levs`.
#' 
#' @keywords internal
#' 
match_names = function(x, xname, levs, column_name){
	
	# don't do anything if `x` is null
	if (is.null(x)){ return(NULL) }
	
	# get the names of `x`
	nms = names(x)
	
	# save some error messages
	stopmessages = c(
		paste0("Couldn't match the following to ",  xname, ": ", paste(levs[! levs %in% nms], collapse=", "), ". Check that the names of ", xname, " contain all the values found in column ", column_name, "."), 
		paste0(xname, " is the wrong length. It should be either length 1, or the same length as the number of different kinds of values in column ", column_name, " (i.e. ", length(levs), ").")
	)
	
	# if `x` is a named vector with any of the levels as a name, put it into the same order as the levels..
	if (any(levs %in% nms)){
		
		# reorder `x` into the same order as the levels..
		if (all(levs %in% nms)){
			x = x[match(levs, nms)]
		
		# .. except if `x` is missing some of the levels, give an error message
		} else {
			stop(stopmessages[1], call.=FALSE)
		}
			
	# .. if `x` isn't a named vector, keep the order but check its length and name it
	} else {
		
		# recycle `x` if it's only length 1..
		if (length(x) == 1){
			x = rep(x, length(levs))
			
		# .. and if it's the wrong length, give an error message
		} else if (length(x) != length(levs)){
			stop(stopmessages[2], call.=FALSE)
		}
		
		# name the parameter values
		names(x) = levs
		
	}
	
	# return `x`
	return(x)
	
}		


#' Plot rarefaction curves
#'
#' Plot rarefaction curves showing how quickly species accumulated. Draws "sample-based" curves (see Details), and displays the number of wasps caught on the x axis and number of species on the y axis.
#'
#' @param x Data frame containing the wasp data. Must contain columns "taxon" and "sample". Each row is an individual wasp.
#' @param n Number of resamples. Default (10) is fast, but gives very jagged curves. Increase to e.g. 100 to get smooth averaged out curves.
#' @param p How large a part of the resampled curves to show in confidence intervals. Default (0.84) shows where 84% of the resampled curves fell. Used to estimate if two rarefaction curves are significantly different (e.g. for a significance of 0.05, check if the confidence intervals of two 0.84 curves overlap).
#' @param ci If TRUE, confidence intervals are shown around the rarefaction curve. Default is to only show the curve without confidence intervals.
#' @param add If TRUE, the rarefaction curve(s) are added to an existing plot. Default is to create a new plot.
#' @param by Name of column in 'x' to split the data by. E.g. if `by`="forest_type", draws separate rarefaction curves for each forest type. Curves are drawn in the same order as the order of the factor levels of the column (change the levels with [factor()] if you e.g. want the curves in different order in the legend drawn by [legend_rarefaction()]). Default is to draw one curve containing all the wasps.
#' @param col Colour to be used for the curves. Typically a string if only one curve is drawn. If several curves are drawn (`by` is not NULL), should preferably be a named character vector giving the colour for each curve. But unnamed vectors or a string work too, see 'Details'.
#' @param pch What symbols to use on the curve. Typically an integer between 0:18 if only one curve is drawn, see [points()] for accepted values. If several curves are drawn (`by` is not NULL), should preferably be a named vector giving the symbol for each curve. But unnamed vectors or a single integer work too, see 'Details'. Default is for the curve to be drawn without symbols.
#' @param pch_col Colour to be used for the symbols. Typically a string if only one curve is drawn. If several curves are drawn (`by` is not NULL), should preferably be a named character vector giving the colour of the symbols for each curve. But unnamed vectors or a string work too, see 'Details'. Default is black.
#' @param ...  Graphical parameters passed to the two functions which draw the curves, [plot()] and [lines()]. These will override any default values such as colours. A few parameters (such as 'type' and 'pch') may not work as expected. 
#' 
#' @details
#' ## Named vectors for graphical parameters
#' If drawing several curves, parameters such as the colour (`col`) should preferably be given as named vectors. The function will use the names to match the colours to the corresponding curve.
#'
#' For example if drawing four curves for four forest types ("primary", "swamp", "disturbed", "clearcut"), the colours could be: `col = c(primary="darkgreen", swamp="blue", disturbed="green", clearcut="yellow")`. These will automatically be matched to the correct forest type, whatever order the colours were given in.
#' 
#' Unnamed vectors work too: `col = c("darkgreen", "blue", "green", "yellow")`. But then you have to be *completely sure* that the colours are in the same order as the curves are drawn. Curves will be drawn in the same order as the factor levels of the column that the wasp data is split by. You can check what the order is with `levels0(x[, by])`.
#' 
#' A single value for the colours and other parameters works but gives the same value to all curves.
#' 
#' ## Sample-based curves 
#' The curves are sample-based rarefaction curves. This means that the wasps are drawn randomly, one sample at a time, and the number of species versus number of wasps is added to the plot. Samples keep on being drawn until all the wasps have been added.
#'
#'  Several randomly drawn curves are made (default is 10). The returned curve is an averaged version of these: at each point of the x axis, we take the average number of species. The upper and lower limits are also saved, but are very approximate (e.g. if `p=0.84`, drops the topmost and lowermost resampled curves until 84% of the curves are in the limits).
#'
#' There are good reasons to prefer randomly drawing the wasps one *sample* at a time, instead of one *wasp* at a time (see e.g. Gotelli & Colwell 2011: Estimating species richness). For the wasp data, they boil down to rarefaction curves basically being a re-enactment. We're re-enacting what would happen if we went back and sampled the area again, several times. How many species for a given number of wasps caught would we expect to get? We'd still be collecting the wasps one sample at a time, so it makes sense to keep the wasps of each sample together.
#'
#' @seealso Function [combine_columns()], which makes it easier to split the data by several columns, e.g. to draw separate rarefaction curves for each forest type and collecting event.
#' 
#' @return List with the curve coordinates, number of wasps and parameters, returned silently. This can be passed to [legend_rarefaction()] to draw a legend with the right colours etc. The list has 5 items:
#' * `r` The curve coordinates. List with the x coordinates (=number of wasps) of the curves, and the average, min and max y coordinates (number of species). Min and max values are for the interval given by `p`. If several curves are drawn, returns a list of each curves's coordinates.
#' * `nwasps` Number of wasps in the curve. If several curves are drawn, returns a table (basically a named vector) of the number of wasps of each curve.
#' * `col` Colour of the curve. If several curves are drawn, returns a named vector of the colours of each curve.
#' * `pch` Symbols drawn on the curve. If several curves are drawn, returns a named vector of the symbols of each curve.
#' * `pch_col` Colour of the symbols drawn on each curve. If several curves are drawn, returns a named vector of the symbol colours of each curve.
#'
#' @examples 
#'
#' # get example wasp data
#' f = system.file("extdata", "wasps_example.csv", package = "turkuwasps", mustWork = TRUE)
#' wasps = read_wasps(f)
#' 
#' # plot a rarefaction curve with all wasps
#' plot_rarefaction(wasps, n=5)
#' 
#' # plot separate curves for each forest type
#' col = c(primary="darkgreen", swamp="blue", disturbed="green", clearcut="yellow", farm="orange")
#' r = plot_rarefaction(wasps, by="forest_type", col=col, pch=1:4)
#' 
#' # add a legend
#' legend_rarefaction(r=r)
#' 
#' @export
#' 
plot_rarefaction = function(x, n=10, p=0.84, ci=FALSE, add=FALSE, by=NULL, col="black", pch=NULL, pch_col="black", ...){

	# draw a separate rarefaction curve for each level in column `by`..
	if(! is.null(by)){
		
		# warn if there are missing values in the column
		if(any(is.na(x[, by]))){
			message("Note: ", sum(is.na(x[, by])), " wasps lack data in column ", by, " and are not included in the plot.")
			x = x[! is.na(x[, by]), ]
		}
		
		# get the x and y limits of the plot
		xmax = max(table(x[, by]))
		ymax = max(stats::aggregate(x$taxon, by=list(x[, by]), FUN=nlevels0)$x)
		xlim = c(0, xmax*1.05)
		ylim = c(0, ymax*1.05)
		
		# get the levels of the column
		levs = levels0(x[, by])
		
		# get the number of wasps in each curve
		nwasps = table(x[, by])
		
		# save the colours and points as named vectors, which are in the same order as the curves
		col = match_names(col, "col", levs, by)
		pch = match_names(pch, "pch", levs, by)
		pch_col = match_names(pch_col, "pch_col", levs, by)
		
		# create a list for the wasp data of each curve
		X = vector("list", length(levs))
		names(X) = levs
		
		# create a list for the rarefaction curves
		r = vector("list", length(X))
		names(r) = levs
		
		# split wasp data into separate data frames for each level
		for (i in 1:length(levs)){
			X[[i]] = x[x[, by] == levs[i], ]
		}
		
		#  get the rarefaction curves and draw them
		for (i in 1:length(X)){
			
			# get this rarefaction curve
			r[[i]] = get_rarefaction(X[[i]], n, p)
			
			# save all the arguments, including the x and y limits if not given by the user
			plot_args = c(list(r=r[[i]], ci=ci, add=add, col=col[i], pch=pch[i], pch_col=pch_col[i]), list(...))
			if (! "xlim" %in% names(list(...))){ plot_args$xlim = xlim }
			if (! "ylim" %in% names(list(...))){ plot_args$ylim = ylim }
			
			# draw this rarefaction curve
			do.call(draw_rarefaction, args=plot_args)
			
			# see to it all the other curves are added to the same plot
			add = TRUE
			
		}

	# .. if `by`was not given, just draw one curve
	} else {
	
		# get the rarefaction curve and its upper and lower limits
		r = get_rarefaction(x, n, p)
	
		# draw the rarefaction curve
		draw_rarefaction(r, ci=ci, add=add, col=col, pch=pch, pch_col=pch_col, ...)
		
		# get the number of wasps in the curve
		nwasps=nrow(x)
	
	}
	
	# return the curve coordinates, number of wasps, and parameters, but don't display them
	invisible(list(r=r, nwasps=nwasps, col=col, pch=pch, pch_col=pch_col))
	
}

