#' @keywords internal
"_PACKAGE"


#' Forest types
#'
#' Ugandan and Peruvian forest types. Used by some functions to get all the forest types, what order to plot them in by default, and what colours to use.
#'
#' @format A data frame with 3 variables. The variables are:
#' * name Name of the forest type, e.g. "primary". 
#' * event Name of collecting event. One of "Uganda 2014-2015", "Amazon 1998", "Amazon 2000", "Amazon 2008", "Amazon 2011". 
#' * colour Colour to use by default when plotting.
#' 
#' Forest types are in the following order: Uganda, then Peru 1998, 2000, 2008, 2011. Within each grouping they are sorted by successional status (e.g. Uganda primary forest to farm).
"forest_type"


#' Samples
#'
#' Ugandan and Peruvian Malaise samples. Used by e.g. [read_wasps()] to get forest type etc for each wasp, and by other functions to find out when and how long each sample was collected. Could also be used to get all the samples, what order to plot them in by default, and what colours to use.
#'
#' @format A data frame with 10 variables. The variables are:
#' * name Name of the sample, e.g. "CCT1-141022".
#' * event Name of collecting event. One of "Uganda 2014-2015", "Amazon 1998", "Amazon 2000", "Amazon 2008", "Amazon 2011". 
#' * forest_type Name of forest type, e.g. "primary". 
#' * site Name of site, e.g. "CC". Mainly relevant for Uganda. For Peru, largely tells the soil type.
#' * trap Name of trap, e.g. "CCT1". 
#' * colour Colour to use by default when plotting. Different colour for each forest type.
#' * latitude Latitude of trap, in decimal degrees. 
#' * start Datetime when sample started to be collected by the Malaise trap, e.g. "2014-10-14 11:04:00 UTC+03:00".
#' * end Datetime when sample stopped being collected by the Malaise trap, e.g. "2014-10-22 11:34:00 UTC+03:00".
#' * tdiff Length of time the sample was collected, in days.
#' * damaged TRUE if the sample was damaged in some way (e.g. trampled by elephant), or doesn't have a complete catch for some other reason. FALSE if all is ok. Damaged samples and wasps from them should not be included in ecological analyses.
#' 
#' Samples are in the following order: Uganda, then Peru 1998, 2000, 2008, 2011. Within each grouping they are sorted by successional status then alphabetical order (e.g. Uganda primary forest samples to farm samples).
#'
"malaise_sample"


#' Traps
#'
#' Ugandan and Peruvian Malaise trap sites. Used by some functions to get all the traps, what order to plot them in by default, and what colours to use.
#'
#' @format A data frame with 11 variables. The variables are:
#' * name Name of the trap, e.g. "CCT1".
#' * event Name of collecting event. One of "Uganda 2014-2015", "Amazon 1998", "Amazon 2000", "Amazon 2008", "Amazon 2011". 
#' * forest_type Name of forest type, e.g. "primary". 
#' * site Name of site, e.g. "CC". Mainly relevant for Uganda. For Peru, largely tells the soil type.
#' * colour Colour to use by default when plotting. Different colour for each forest type.
#' * latitude Latitude of trap, in decimal degrees. 
#' * longitude Longitude of trap, in decimal degrees. 
#' * elevation Elevation of trap, in metres. 
#' * deadwood Amount of dead wood near trap. Clearly decaying wood, sum of diameter squared per distance from trap.
#' * livewood Amount of live trees near trap. Sum of diameter (dbh) squared per distance from trap. 
#' * canopy_open Percentage of the sky visible from the trap. (0% = closed canopy, 100% = open sky)
#' 
#' Traps are in the following order: Uganda, then Peru 1998, 2000, 2008, 2011. Within each grouping they are sorted by successional status then alphabetical order (e.g. Uganda CCT1 to FARMT2).
#'
#' @seealso Zenodo has more details on the trap data, e.g. on how the amount of dead and live wood was calculated: https://doi.org/10.5281/zenodo.2225643
"trap"


#' Example wasp data
#'
#' Example csv file of wasp data downloaded from Kotka (Kotka Collection Management System). Ugandan rhyssine wasps, as published in https://doi.org/10.1098/rsos.190913 and https://doi.org/10.3897/zookeys.878.37845. Used in the examples to show what wasp data typically looks like. 
#' 
#' @name wasps_example
#'
#' @format A csv file with 456 data rows (each corresponding to one wasp) and 58 variables in columns. There are also two header rows. Kotka variables are documented in https://kotka.luomus.fi/documentation/field (link valid 2023-04-20).
#' 
#' @seealso [read_wasps()] which reads data from files like this and converts to something easier to read and use.
NULL

