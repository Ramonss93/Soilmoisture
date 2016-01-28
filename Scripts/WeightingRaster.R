getWeightingRaster <- function(aspect, slope, profileCurv, lrel) {
	
  #calculates the weighing raster from terrain attributes
  #args: aspect, slope, profile curvature and distance to channel (all raster)
  #output: weighting raster
  #reference: Svetlitchnyi (2003)
  
	coordsysangl = 0
	ke <- aspect
	alpha0 <- cellStats(slope, stat='mean', na.rm=TRUE)
	ke[ke<(45+coordsysangl)] <- -0.01 #north
	ke[ke>=(45+coordsysangl) & ke<(135+coordsysangl)] <- 0.002 #east
	ke[ke>=(135+coordsysangl) & ke<(225+coordsysangl)] <- 0.005 #south
	ke[ke>=(225+coordsysangl) & ke<(315+coordsysangl)] <- -0.003 #west
	ke[ke>=(315+coordsysangl)] <- -0.01 #north
	Ka <- (1-ke*slope)/(1-ke*alpha0)

	a_1 <- calc(lrel, fun=function(lrel){(1-0.2*lrel**0.5)})
	a_2 <- calc(lrel, fun=function(lrel){(0.77+0.43*lrel**1.47)}) 
	a_3 <- calc(lrel, fun=function(lrel){(0.77+0.70*lrel**4.0)})
	
	a_1[lrel>=0.167] <- NA
	a_2[lrel<0.167 | lrel>=0.833] <- NA
	a_3[lrel<0.833] <- NA
	
	covera12 <- cover(a_1, a_2)
	a <- cover(covera12, a_3)

  b_1 <- calc(lrel, fun=function(lrel){(1.04-0.22*lrel**0.93)})
	b_2 <- calc(lrel, fun=function(lrel){(0.86+18.0*(lrel-0.833)**2.0)})

  b_1[lrel>=0.833] <- NA
	b_2[lrel<0.833] <- NA
	b <- cover(b_1, b_2)

  profileCurvResampled <- focal(profileCurv, w=matrix(1/9, nc=3, nr=3))

  Kw <- profileCurvResampled
	
	Kwconvex <- overlay(a,aspect,Ka, fun=function(a, aspect, Ka){(a + 0.16*cos(aspect) + 0.09*sin(aspect))*Ka})
	Kwconcave <- overlay(b,aspect,Ka, fun=function(b, aspect, Ka){(b + 0.14*cos(aspect) + 0.10*sin(aspect) - 0.02*cos(2*aspect))*Ka})

  Kwconvex[profileCurv<0] <- NA 
	Kwconcave[profileCurv>=0] <- NA
	
	Weighting1 <- cover(Kwconcave, Kwconvex)
	Weighting <- focal(Weighting1, w=matrix(1/25, nc=5, nr=5))
	
	return(Weighting)
}