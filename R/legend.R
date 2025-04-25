


#' Add legend to rarefaction plot
#'
#' Add legend to rarefaction plots. Basically a wrapper for [legend()], for easy drawing of the lines, symbols etc of rarefaction curves.
#' 
#' @param txt The legend text. Character vector, e.g. `c("primary", "swamp", ""disturbed", "clearcut")`. Legend texts will be in the same order as given here (though see `column`). Passed to [legend()] as parameter `legend`, so will also accept e.g. NA values. Either this or `r` must be given. If only `r` is given, `txt` will is got from it.
#' @param r List of curve coordinates and graphical parameters returned by [plot_rarefaction()]. If given, most parameters (e.g. `txt`, `col`, `pch`) will be taken from here. If you also give the same parameters separately, your parameters will be the ones used. Either this or `txt` must be given. If you give both `r` and `txt`, do make sure that they are in the same order!
#' @param column What column each legend text should be placed in. Integer vector of same length as `txt`. E.g. `c(1, 2, 2, 2)` puts the first text in the first column, and the three others in a second column. It is OK to give these in a weird order (e.g. `c(2, 3, 2, 1)`), but trying to leave empty columns won't work (e.g. `c(1, 1, 3, 3)` which lacks column 2). Default is to not split the legend texts into several columns. 
#' @param col The colour of the lines. Vector of length 1 or same length as `txt`. Should in the same order as `txt`. NA values can be used if you don't want to draw a line, e.g. `c("darkgreen", "blue", NA, NA)` only draws the first two lines. If not given, all lines will be black.
#' @param pch The symbols to add on top of the lines. Vector of length 1 or same length as `txt`. Should in the same order as `txt`. Typically an integer between 0:18, see [points()] for accepted values. NA values can be used if you don't want to draw a symbol, e.g. `c(1, 2, NA, NA)` only draws the first two symbols. If not given, no symbols are drawn.
#' @param pch_col The colour of the symbols. Vector of length 1 or same length as `txt`. Should in the same order as `txt`. NA values can be used if you don't want to draw a symbol, e.g. `c("darkgreen", "blue", NA, NA)` only draws the first two symbols. If not given, symbols will be the same colour as the lines.
#' @param pch_cex The size of the symbols. Vector of length 1 or same length as `txt`. Should in the same order as `txt`. Typically will be 0.5, i.e. 50% of the normal size for symbols in the plot. This is also the default.
#' @param title Overall title for the plot. Character string. Default is to not have a title.
#' @param colnames The subtitles to place over each column. Character vector with one subtitle for each column. Default is to not have subtitles.
#' @param ... Other arguments passed to [legend()]. Typically `x`, which gives the position of the legend (default is "bottomright"). Also, `fill` may sometimes be used to display confidence intervals. Graphical parameters such as `fill` should be vectors of length 1 or same length as `txt`, in the same order as `txt`. Argument `legend` has no effect, and some rarely used graphical parameters may not work as intended if drawing several columns.
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
#' cnames = c("undisturbed forest", "disturbed or cut forest")
#' legend_rarefaction(r=r, column=c(2, 2, 1, 1), colnames=cnames, title="Uganda", x="right")
#' 
#' # prepare a new column in the wasp data, for drawing separate curves for dry and wet season
#' # also see to it the curves will be drawn in successional order primary -> farm
#' levs = c("primary", "swamp", "disturbed", "clearcut", "farm")
#' wasps$forest_type = factor(wasps$forest_type, levels=levs)
#' wasps$forest_season = combine_columns(wasps, c("forest_type", "season"), all=TRUE)
#'
#' # plot rarefaction curves with separate curves foe each forest type and season
#' col = c(primary="darkgreen", swamp="blue", disturbed="green", clearcut="yellow", farm="orange")
#' col = rep(col, each=2)
#' r = plot_rarefaction(wasps, by="forest_season", col=col, pch=rep(1:2, 5))
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
		if (is.null(r)) { pch_col = col } else { pch_col = r$pch_col }
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
	
	#
	invisible(res)
	
}