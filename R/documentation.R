#' @keywords internal
"_PACKAGE"


#' Example results resample()
#'
#' Results of a fitted generalised linear model, returned by [resample()]. Fitted to example Rhyssinae wasp data with 99 resamples and pairwise comparisons between forest types. Used in several of the examples to save time, since it can take several minutes to run the analyses.
#'
#' @format A list with 9 items, returned by [resample()].
#'
"a"


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
#' Ugandan, Peruvian and Skanssi samples. Used by e.g. [read_wasps()] to get forest type etc for each wasp, and by other functions to find out when and how long each sample was collected. Could also be used to get all the samples, what order to plot them in by default, and what colours to use.
#'
#' @format A data frame with 19 variables. The variables are:
#' * name Name of the sample, e.g. "CCT1-141022".
#' * kotka_id Identifier of the sample in the Kotka database, e.g. "http://mus.utu.fi/CCT1-141022".
#' * event Name of collecting event. One of "Uganda 2014-2015", "Amazon 1998", "Amazon 2000", "Amazon 2008", "Amazon 2011", "Skanssi2024". 
#' * forest_type Name of forest type, e.g. "primary". 
#' * site Name of site, e.g. "CC". Mainly relevant for Uganda. For Peru, largely tells the soil type.
#' * trap Name of trap, e.g. "CCT1". 
#' * colour Colour to use by default when plotting. Different colour for each forest type.
#' * start Datetime when sample started to be collected by the trap, e.g. "2014-10-14 11:04:00 UTC+03:00".
#' * end Datetime when sample stopped being collected by the trap, e.g. "2014-10-22 11:34:00 UTC+03:00".
#' * days Number of days from from the start of the sampling event to the middle of when the sample was collected. The start of each sampling event is (somewhat arbitrarily): 
#'    * "Amazon 1998" : "1998-08-01 00:00:00 UTC-05:00"
#'    * "Amazon 2000" : "2000-01-01 00:00:00 UTC-05:00"
#'    * "Amazon 2008" : "2008-05-01 00:00:00 UTC-05:00"
#'    * "Amazon 2011" : "2011-04-01 00:00:00 UTC-05:00"
#'    * "Skanssi2024" : "2024-05-01 00:00:00 UTC+03:00"
#'    * "Uganda 2014-2015" : "2014-09-01 00:00:00 UTC+03:00" .
#' * tdiff Length of time the sample was collected, in days.
#' * tdiff_log Logarithm of the length of time (in days) the sample was collected. Base `e`. 
#' * damaged TRUE if the sample was damaged in some way (e.g. trampled by elephant), or doesn't have a complete catch for some other reason. FALSE if all is ok. Damaged samples and wasps from them should not be included in ecological analyses.
#' * deadwood Amount of dead wood near trap. Clearly decaying wood, sum of diameter squared per distance from trap.
#' * livewood Amount of live trees near trap. Sum of diameter (dbh) squared per distance from trap. 
#' * canopy_open Percentage of the sky visible from the trap. (0% = closed canopy, 100% = open sky)
#' * rain Average daily rainfall (mm) when the sample was collected. Weighted average: weather measurements that occurred entirely during the sampling period are weighted by 1, other measurements are weighted by how much they overlap in time (e.g. half of weather measurement overlaps with sampling period = weighted by 0.5).
#' * t_max Average daily maximum temperature (Celsius) when the sample was collected. Weighted average: weather measurements that occurred entirely during the sampling period are weighted by 1, other measurements are weighted by how much they overlap in time (e.g. half of weather measurement overlaps with sampling period = weighted by 0.5).
#' * season The season when the sample was collected. One of "wet" or "dry" for Ugandan samples. NA for Peruvian samples. Samples which were partly in wet season, and partly in dry, are in dry season if more than half of the sample was collected during dry season, otherwise in wet season. Seasons start (somewhat arbitrarily) on the following datetimes:
#'    * "wet" : "2014-09-01 00:00:00 UTC+03:00"
#'    * "dry" : "2014-12-01 00:00:00 UTC+03:00"
#'    * "wet" : "2015-03-01 00:00:00 UTC+03:00"
#'    * "dry" : "2015-06-01 00:00:00 UTC+03:00"
#'    * "wet" : "2015-09-01 00:00:00 UTC+03:00"
#' 
#' Samples are in the following order: Uganda, then Peru 1998, 2000, 2008, 2011, then Skanssi. Within each grouping they are sorted by successional status then alphabetical order (e.g. Uganda primary forest samples to farm samples).
#'
#' @seealso [weather], which variables "rain" and "t_max" are based on. The code used to add the variables is in folder "data-raw" at https://github.com/tapani-hopkins/turkuwasps; this folder is not included in the loaded package.
"malaise_sample"


#' Sites
#'
#' Ugandan and Peruvian sites. Used by some functions to get all the sites, what order to plot them in by default, and what colours to use.
#'
#' @format A data frame with 4 variables. The variables are:
#' * name Name of the site, e.g. "CCT1". Mainly relevant for Uganda. For Peru, largely tells the soil type.
#' * event Name of collecting event. One of "Uganda 2014-2015", "Amazon 1998", "Amazon 2000", "Amazon 2008", "Amazon 2011". 
#' * forest_type Name of forest type, e.g. "primary". 
#' * colour Colour to use by default when plotting. Different colour for each forest type.
#' 
#' Sites are in the following order: Uganda, then Peru 1998, 2000, 2008, 2011. Within each grouping they are sorted by successional status then alphabetical order (e.g. Uganda CC to FARM).
#'
"site"


#' Traps
#'
#' Ugandan, Peruvian, and Skanssi trap sites. Used by some functions to get all the traps, what order to plot them in by default, and what colours to use.
#'
#' @format A data frame with 11 variables. The variables are:
#' * name Name of the trap, e.g. "CCT1".
#' * event Name of collecting event. One of "Uganda 2014-2015", "Amazon 1998", "Amazon 2000", "Amazon 2008", "Amazon 2011", "Skanssi2024". 
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
#' Traps are in the following order: Uganda, then Peru 1998, 2000, 2008, 2011, then Skanssi. Within each grouping they are sorted by successional status then alphabetical order (e.g. Uganda CCT1 to FARMT2).
#'
#' @seealso Zenodo has more details on the trap data, e.g. on how the amount of dead and live wood was calculated: https://doi.org/10.5281/zenodo.2225643
"trap"


#' Weather
#'
#' Weather data from Uganda and Peru. Used e.g. to plot rainfall or temperature.
#'
#' @format A data frame with 8 variables. The variables are:
#' * event Name of collecting event. One of "Uganda 2014-2015", "Amazon 1998", "Amazon 2000", "Amazon 2008", "Amazon 2011". 
#' * start Datetime when weather started to be measured, e.g. "2014-10-14 09:00:00 UTC+03:00". There is some guesswork involved in these: I believe that Ugandan rain, min and max temperature were collected mornings about 9 AM, and are for the previous 24h; and that in Peru they are for the period 8 AM to next day 8 AM. 
#' * end Datetime when weather stopped being measured, e.g. "2014-10-15 09:00:00 UTC+03:00". There is some guesswork involved in these: I believe that Ugandan rain, min and max temperature were collected mornings about 9 AM, and are for the previous 24h; and that in Peru they are for the period 8 AM to next day 8 AM. 
#' * rain Amount of rain in millimetres during the (24 hour) measurement interval.
#' * t_min Minimum temperature in Celsius during the (24 hour) measurement interval. For the Ugandan data, this is from the forest thermometer.
#' * t_max Maximum temperature in Celsius during the (24 hour) measurement interval. For the Ugandana data, this si from the forest thermometer.
#' * t_avg Average temperature in Celsius during the (24 hour) measurement interval. Only for Peruvian weather data.
#' * season One of "dry" or "wet", giving an approximate estimate of whether the measurement occurred during the dry or wet season. Only for Ugandan weather data.
#' 
#' @seealso Zenodo has more details on the Ugandan and Peruvian weather data:
#' https://doi.org/10.5281/zenodo.2225643
#' https://doi.org/10.5281/zenodo.4030168
"weather"


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

