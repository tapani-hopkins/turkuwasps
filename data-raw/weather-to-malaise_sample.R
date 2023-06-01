## code which was used to add weather data to dataset `malaise_sample`
## can be used to update the data, or add more of it

# save sample and weather data in shorter variables
m = turkuwasps::malaise_sample
w = turkuwasps::weather

# add columns "rain" and "t_max" to the sample data
m$rain = NA
m$t_max = NA

# get the time intervals when each sample or weather measurement was collected
dm = as.interval(m$start, m$end)
dw = as.interval(w$start, w$end)

# get the average daily rain (mm) and max temperature (°C) for each sample 
for (i in 1:length(dm)){
	
	# find out how much each weather measurement overlaps in time with the sample
	over = overlap(dw, dm[i])
	
	# if there are no weather measurements for this sample, set rain and temp to NA..
	if (sum(over) == 0){
		m$rain[i] = NA
		m$t_max[i] = NA
		
	# .. if there are weather measurements, get average rain and temp
	} else {
		
		# get weights for a weighted average
		weight = over / sum(over, na.rm=TRUE)
	
		# get average rain and temp, 
		# weighted by how much each weather measurement overlapped with the sample collection
		m$rain[i] = sum(weight * w$rain , na.rm=TRUE)
		m$t_max[i] = sum(weight * w$t_max , na.rm=TRUE)
	
	}
	
}

# update dataset `malaise_sample`
malaise_sample = m
usethis::use_data(malaise_sample, overwrite = TRUE)