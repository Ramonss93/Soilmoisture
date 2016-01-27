# function for construction of hydro. network - and hillslopes
HydroNetwork <- function(dem, accumulation) {
	#get the hydrological network from the elevation model
	#Args: 
		#dem (format)
		#accumulation: value from which it is a channel
	#Returns: distance to channel
	Upslope <- upslope.area(dem, log=FALSE, atb=FALSE, fill.sinks=TRUE)
	Channel <- Upslope
	Channel[Upslope>accumulation] <- 1
	Channel[Upslope<=accumulation] <- NA
	#search for distance to channel and maximum from divide (!)
	distancetochannel <- distance(Channel)
	lmax <- cellStats(distancetochannel, stat='max', na.rm=TRUE)
	lref <- distancetochannel/(lmax)
	return(lref)
}
