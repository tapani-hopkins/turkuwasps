#' Save plots as PNG
#'
#' Save plots to file as a PNG image. This is a wrapper for [png()], with default settings for resolution, dimensions, transparency etc.  
#'
#' @param f File path where to save the PNG file.
#' @param width Width of the plot in mm.
#' @param height Height of the plot in mm.
#' @param bg Background colour. Typically "transparent" or "white" (the default). 
#' @param res Resolution as an integer. It's slightly uncertain exactly how [png()] handles this when using mm as units, but the bigger this number the higher the resolution.
#' @param what Plotting commands. The plot created by these will be saved to PNG. See examples for how to write these.  
#'
#' @examples
#' \dontrun{
#'
#' # save plot commands..
#' figure = expression( {
#' 	barplot(1:10, space=0, col=1:10)
#' 	legend("topleft", legend=1:10, fill=1:10)
#' } )
#'
#' # .. then save to file
#' save_png("example.png", what=figure)
#' 
#' # alternatively, you can write the commands inside the function
#' save_png("example.png", what={
#' 	barplot(1:10, space=0, col=1:10)
#' 	legend("topleft", legend=1:10, fill=1:10)
#' })
#' 
#' # another alternative is to run as a function
#' figure1 = function(){
#' 	barplot(1:10, space=0, col=1:10)
#' 	legend("topleft", legend=1:10, fill=1:10)
#' }
#'
#' save_png("example.png", what=figure1())
#'
#' }
#' 
#' @export
save_png = function(f="turkuwasp_image.png", width=164, height=140, bg="white", res=300, what){
	
	# start saving to png file with default settings
	grDevices::png(filename=f, width=width, height=height, units="mm", bg=bg, res=res)
	
	# do plotting commands
	eval(what)
	
	# save png
	grDevices::dev.off()
	
}