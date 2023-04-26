#' Default legend
#'
#' Add default legend to plots. Either a legend showing the forest types (in default order with default colours), or taxa (with default colour scheme). 
#'
#' @param what What legend to add. Can be "forest type", in which case shows the forest types. Or can be a character vector of taxon names, in which case shows the taxa. Taxa are got from the factor levels of the vector, so it is OK to have the same taxon several times. 
#' @param event Character vector of what collecting events to include if making a forest type legend. Accepted events are "Uganda 2014-2015", "Amazon 1998", "Amazon 2000", "Amazon 2008", "Amazon 2011". *Alternatively*, can be a character vector of locations (e.g. traps), in which case all collecting events involving those locations will be included. If NULL, all events are included. 
#' @param ... Other arguments passed to [legend()]. Typically `x`, which gives the position of the legend (default is "topright"). Arguments `legend` and `fill` have no effect.
#'
#' @return List returned by [legend()]. Returned invisibly.
#'
#' @export
#'
default_legend = function(what="forest_type", event=NULL, ...){
	
	# store various default arguments for the legend
	legend_args = list(
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
	if (what == "forest_type"){
		
		# get forest type data of the selected collecting events
		d = turkuwasps::forest_type
		d = d[d$event %in% event, ] 
		
		# save forest types and their colours in the legend arguments
		legend_args["legend"] = list(d$name)
		legend_args["fill"] = list(d$colour)
		
	} else {
		# show species (xxx to be added)
	}
	
	# show the legend
	res = do.call(graphics::legend, args=legend_args)
	
	# return
	invisible(res)
	
}