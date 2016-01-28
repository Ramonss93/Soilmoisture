getSoilMoistureContent <- function(SoilMoistureDataSet, Weighting, slope, DEM){
  
  #calculate soil moisture values
  #args: soil moisture dataset (point dataset or raster), weighting raster from Svetlitchnyi(raster), slope(raster), DEM(raster)
  #output: downscaled soil moisture content map (raster)
  
	if (is.data.frame(SoilMoistureDataSet)){
    SoilMoisturePoints <- SpatialPoints(SoilMoistureDataSet[c("X", "Y")], proj4string=CRS(prj_string))
		SoilMoistureDataSet$Slope <- extract(slope, SoilMoisturePoints, pch=16)
		SoilMoistureDataSet
		refSoilMoisture <- mean(SoilMoistureDataSet$SM[SoilMoistureDataSet["Slope"] == min(SoilMoistureDataSet["Slope"])])
		SM <-	Weighting*refSoilMoisture
    return(SM)
	} else {
	  SoilMoistureDataSet <- crop(SoilMoistureDataSet, DEM, snap='near')
	  resampledcrop <- resample(SoilMoistureDataSet, Weighting, method='ngb')
		kw <- resampledcrop*Weighting
		return(kw)
		}
}
