HydroNetwork <- function(dem, accumulation) {
	
  #get the hydrological network from the elevation model
	#Args: dem (format asc or dem), accumulation: value of number of upslope cells before it is a channel
	#Returns: relative distance to channel
	
  Upslope <- upslope.area(dem, log=FALSE, atb=FALSE, fill.sinks=TRUE)
	Channel <- Upslope
	Channel[Upslope>accumulation] <- 1
	Channel[Upslope<=accumulation] <- NA
	distancetochannel <- distance(Channel)
	lmax <- cellStats(distancetochannel, stat='max', na.rm=TRUE)
	lref <- distancetochannel/(lmax)
	return(lref)
}
