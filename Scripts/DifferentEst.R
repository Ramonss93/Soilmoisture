difEstReal <- function(SM, SoilMoistureDataSet, prj_string){
  SoilMoisturePoints <- SpatialPoints(SoilMoistureDataSet[c("X", "Y")], proj4string=CRS(prj_string))
  #rasterSMpoints <- rasterize(SoilMoisturePoints, DEM,SoilMoistureDataSet$SM)
  EstimatedSM <- extract(SM, SoilMoisturePoints,  pch=16, sp=T)
  EstimatedSMdf <- data.frame(EstimatedSM)
  difference <- SoilMoistureDataSet["SM"] -   data.frame(EstimatedSM)[1]
  EstimatedSMdf$difference <- difference
  return(EstimatedSMdf)
}
