#' Draw rarefaction curves
#'
#' No documentation yet, needs to be added, helper function
#' ... passed to plot and lines
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
	
	# draw the points in the same colour as the curves, unless the user gave a colour
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
		oldcol = grDevices::col2rgb(plot_args$col, T)
		nc = oldcol * c(1, 1, 1, 0.09)  # 9% opacity
		nc = grDevices::rgb(nc[1], nc[2], nc[3], nc[4], maxColorValue=oldcol[4])
	
		# draw as a polygon
		graphics::polygon( c(r$x, rev(r$x)) , c(r$y_min, rev(r$y_max)), border=NA, col=nc)
		
	}
	
}


#' Plot rarefaction curves
#'
#' Plot rarefaction curves showing how quickly species accumulated. Draws "sample-based" curves (see Details), and displays the number of wasps caught on the x axis and number of species on the y axis.
#'
#' @param x Data frame containing the wasp data. Must contain columns "taxon" and "sample". Each row is an individual wasp.
#' @param n Number of resamples. Default (10) is fast, but gives very jagged curves. Increase to e.g. 100 to get smooth averaged out curves.
#' @param  p How large a part of the resampled curves to show in confidence intervals. Default (0.84) shows where 84% of the resampled curves fell. Used to estimate if two rarefaction curves are significantly different (e.g. for a significance of 0.05, check if the confidence intervals of two 0.84 curves overlap).
#' @param by Name of column in 'x' to split the data by. E.g. if by="forest_type", draws separate rarefaction curves for each forest type.
#' @param ci If TRUE, confidemce intervals are shown around the rarefaction curve. Default is to only show the curve without confidence intervals.
#' @param add If TRUE, the rarefaction curve(s) are added to an existing plot. Default is to create a new plot.
#' @param pch What symbols to use on the curve. Typically an integer between 0:18. See [points()] for accepted values. Default is for the curve to be drawn without symbols.
#' @param pch_col 
#' @param ...  Graphical parameters passed to the two functions which draw the curves, [plot()] and [lines()]. These will override any default values such as colours. A few parameters (such as 'type' and 'pch') may not work as expected. 
#' 
#' 
#' @details XXX
#' xxx Currently, 'by' only handles one column at a time (e.g. forest_type). and draws all in same colour. Needs updating.
#' 
#' @export
plot_rarefaction = function(x, n=10, p=0.84, by=NULL, ci=FALSE, add=FALSE, pch=NULL, pch_col=NULL, ...){

	#
	if(! is.null(by)){
		
		# create list for wasp data
		levs = levels0(x[, by])
		X = vector("list", length(levs))
		
		# split wasp data into one data frame per rarefaction curve
		for (i in 1:length(levs)){
			X[[i]] = x[x[, by]==levs[i], ]
			names(X)[i] = levs[i]
		}
		
		#  create a list for the rarefaction curves
		R = vector("list", length(X))
		names(R) = names(X)
		
		# get and draw the rarefaction curves
		for (i in 1:length(X)){
			
			# save this rarefaction curve and draw it
			R[[i]] = get_rarefaction(X[[i]], n, p)
			draw_rarefaction(R[[i]], ci=ci, add=add, pch=pch, pch_col=pch_col, ...)
			
			# see to it all the other curves are added to the same plot
			add = TRUE
			
		}
		
	} else {
	
		# get the rarefaction curve and its upper and lower limits
		r = get_rarefaction(x, n, p)
	
		# draw the rarefaction curve
		draw_rarefaction(r, ci=ci, add=add, pch=pch, pch_col=pch_col, ...)
	
		# return the curve coordinates but don't display them
		invisible(r)
	
	}
	
}


#' Get rarefaction curves
#'
#' No documentation yet, needs to be added
#'
#' 
#' @export
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