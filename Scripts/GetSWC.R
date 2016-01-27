getSoilMoistureContent <- function(SoilMoistureDataFrame, Weighting){
	SoilMoistureDataFramexy <- empty dataframe
	SoilMoistureDataFramexy$x <- SoilMoistureDataFrame["X"]
	SoilMoistureDataFramexy$y <- SoilMoistureDataFrame["Y"]
	SoilMoisturePoints <- SpatialPoints(SoilMoistureDataFramexy, proj4string=CRS(prj_string))
	SoilMoistureDataFrame$Slope <- extract(slope, SoilMoisturePoints, pch=16)
	SoilMoistureDataFrame
	refSoilMoisture <- mean(SoilMoistureDataFrame$FIELD_4[SoilMoistureDataFrame["Slope"] == min(SoilMoistureDataFrame["Slope"])])
	SM <-	Weighting*refSoilMoisture
	return(SM1)
}
?SpatialPoints																	
#points SM at a certain time (in certain format), three examples with fields X, Y, SM)
SoilMoistureDataFrame <- read.csv('data/Wuest_all_mod.csv')
SoilMoistureDataFrame <- read.csv('data/SM_points_all.csv')
rasterSM <-  raster('data/MC-20130506-SM.tif')
rasterSM <- projectRaster(rasterSM, crs=prj_string)
#preprocess soil moisture
rastercrop <- crop(rasterSMproj, DEM)
rastercrop

plot(Weighting)
getSoilMoistureContent <- function(SoilMoistureDataFrame, Weighting){

	SM <-	Weighting*rastercrop
	
	return(SM1)
}