#' Default legend
#'
#' Add default legend to plots. Either a legend showing the forest types (in default order with default colours), or taxa (with default colour scheme). 
#'
#' @param what What legend to add. Can be "forest type", in which case shows the forest types. Or can be a character vector of taxon names, in which case shows the taxa. Taxa are got from the factor levels of the vector, so it is OK to have the same taxon several times. 
#' @param event Character vector of what collecting events to include if making a forest type legend. Accepted events are "Uganda 2014-2015", "Amazon 1998", "Amazon 2000", "Amazon 2008", "Amazon 2011". *Alternatively*, can be a character vector of locations (e.g. traps), in which case all collecting events involving those locations will be included. If NULL, all events are included. 
#' @param modelled If TRUE, a line for modelled wasp catches will be added to the legend. If FALSE (the default), no modelled catches are added to the legend.
#' @param ... Other arguments passed to [legend()]. Typically `x`, which gives the position of the legend (default is "topright"). Arguments `legend` and `fill` have no effect.
#'
#' @note Taxon names are automatically italicised by [as_italic()], on the assumption they are species names. If wanting non-italics, the legend will have to be drawn manually.
#'
#' @return List returned by [legend()]. Returned invisibly.
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
#' # plot traps
#' tmp = plot_place(x$trap, m)
#' 
#' # add legend
#' default_legend("forest_type", event=colnames(tmp$x)) 
#'
#' # plot over time
#' plot_time(as.interval(x$start, x$end), taxon=x$taxon)
#' 
#' # add legend
#' default_legend(x$taxon, x="topleft")
#' 
#' @export
#'
default_legend = function(what="forest_type", event=NULL, modelled=FALSE, ...){
	
	# store various default arguments for the legend
	legend_args = list(
		cex = 0.8, 
		x = "topright"
	)
	
	# add the arguments given by the user (overwrite any defaults with the same name)
	user_args = list(...)
	legend_args[names(user_args)] = user_args
	
	# get all five events ("Uganda 2014-2015" etc)
	events = levels0(turkuwasps::forest_type$event)
	
	# if `event` was not given, show all events 
	if (is.null(event)){
		event = events

	# if `event` has locations (e.g. traps) instead of events, show events of those locations
	} else if (! any(event %in% events)){
		loc = get_locationdata(get_locationtype(event))
		i = which(loc$name %in% event)
		event = levels0(loc$event[i])
	}
	
	# create forest type legend if asked to do so..
	if (what[1] == "forest_type"){
		
		# get forest type data of the selected collecting events
		d = turkuwasps::forest_type
		d = d[d$event %in% event, ] 
		
		# save forest types and their colours in the legend arguments
		legend_args$legend = d$name
		legend_args$fill = d$colour
	
	# .. or create rarefaction curve legend if asked to do so..
	} else if (what[1] == "rarefaction"){
		
		# xxxx
		
	# .. otherwise, create taxon legend
	} else {
		
		# save the taxa and their colours in the legend arguments
		taxa = levels0(what)
		legend_args$legend = as_italic(taxa)
		legend_args$fill = default_colours(length(taxa))
		
		# reverse the taxa and colours (taxa are stacked bottom to top in the plot)
		legend_args$legend = rev(legend_args$legend)
		legend_args$fill = rev(legend_args$fill)
		
	}
	
	# show the legend
	res = do.call(graphics::legend, args=legend_args)
	
	# add the modelled wasp catch to the legend if asked to do so
	if (modelled){
		graphics::legend(x=res$rect$left, y=res$rect$top - res$rect$h, lwd=1, legend="modelled", cex=0.7, bty="n")
	}
		
	# return
	invisible(res)
	
}