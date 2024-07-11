


#' Add legend to rarefaction plot
#'
#' No documentation yet, needs to be added.
#' 
#' @export
legend_rarefaction = function(rownames, colnames=NULL, col="black", pch=NA, pch_col=col, pch_cex=0.5, title=NULL, ...){
	
	# store various default arguments for the legend
	legend_args = list(
		cex = 0.7,
		col = col, 
		lwd = 2,
		pt.cex = 0.5,
		title = title,
		x = "bottomright"
	)
	
	# add the arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	legend_args[names(user_args)] = user_args
	
	
	
	#
	if (is.null(colnames)) {
		
		# 
		legend_args$legend = levels0(rownames)
	
		# show the legend
		res = do.call(graphics::legend, args=legend_args)
	
		# get midpoints of the lines in the legend
		res_x = res$text$x - (res$text$x[1] - res$rect$left) / 2
		
		# add points to the lines of the legend
		graphics::points(res_x, res$text$y, pch=pch, col=pch_col, cex=pch_cex)
	
	# 	
	} else {
		
		# 
		legend_args$legend = rep(levels0(rownames), nlevels0(colnames))
	
		# 
		legend_args$ncol = nlevels0(colnames)
		
		# add some space for the column names by adding spaces around the title
		legend_args$title = paste0("\n", legend_args$title, "\n")	
		
		# show the legend
		res = do.call(graphics::legend, args=legend_args)
		
		# get midpoints of the lines in the legend
		res_x = res$text$x - (res$text$x[1] - res$rect$left) / 2
		
		# add points to the lines of the legend
		graphics::points(res_x, res$text$y, pch=pch, col=pch_col, cex=pch_cex)
		
		# get suitable places for the column names
		x_colnames = res$text$x[c(1, length(res$text$x))]
		y_colnames = res$text$y[1] + 0.3 * (res$rect$top - res$text$y[1])
		
		# add column names
		graphics::text(x_colnames, y_colnames, levels0(colnames), cex=legend_args$cex)
		
	}
	
	#
	invisible(res)
	
}