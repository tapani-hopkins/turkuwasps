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
		i = floor(seq(1, length(r$x), length.out=10))
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
#' @param pch_col Colour to be used for the symbols. Typically a string if only one curve is drawn. If several curves are drawn (`by` is not NULL), should preferably be a named character vector giving the colour of the symbols for each curve. But unnamed vectors or a string work too, see 'Details'. Default is to use the same colours as for the curves.
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
#' # add a legend (to be updated here)
#' 
#' 
#' 
#' @export
#' 
plot_rarefaction = function(x, n=10, p=0.84, ci=FALSE, add=FALSE, by=NULL, col="black", pch=NULL, pch_col=col, ...){

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

