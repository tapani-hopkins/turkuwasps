#' Save plots as PNG
#'
#' Save plots to file as a PNG image. This is a wrapper for [png()], with default settings for resolution, dimensions, transparency etc.  
#'
#' @param f File path where to save the PNG file. 
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
#' save_png("example.png", figure)
#' 
#' # alternatively, you can write the commands in the function
#' save_png("example.png", {
#' 	barplot(1:10, space=0, col=1:10)
#' 	legend("topleft", legend=1:10, fill=1:10)
#' })
#' 
#' }
#' 
#' @export
save_png = function(f="turkuwasp_image.png", what){
	
	# start saving to png file with default settings
	grDevices::png(filename=f, width=164, height=140, units="mm", bg="transparent", res=300)
	
	# do plotting commands
	eval(what)
	
	# save png
	grDevices::dev.off()
	
}