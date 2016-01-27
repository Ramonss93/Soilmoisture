

#libraries
library(dynatopmodel)
library(raster)
library(rgdal)
library(sp)

rm(list = ls())

setwd("/home/user/git/ScriptingProject")

#Title: Scripting for digital terrain modelling

#Documentation: Must, Should, Could, Would
#Must: get an soil water content map for one catchment, nicely visualized (maybe some mozaiking)

#Should: compare the estimated values of soil moisture with actual soil moisture data (point measurements) and uncertainty map, e.g. with R squared
	
#Could: use for hydrological models
	
#Would: script for every digital elevation model and all raster/vector datasets


#import DEM

##import and preprocess data raster/vector

## what I ask from user: DEM, projection if not known, soil moisture dataset (POINT OR RASTER), idem dito, slopy area
# rastertest 1
DEM <- raster('data/dem5wuest.asc')
#change if different
prj_string <- "+proj=tmerc +lat_0=0 +lon_0=6 +k=1 +x_0=2500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"
#raster test 2
#DEM <- raster('data/tarrawar.asc')
source('Scripts/projection.R')
DEM <- projectinput(DEM, prj_string)

#points SM at a certain time (in certain format), three examples with fields X, Y, SM)
SoilMoistureDataFrame <- read.csv('data/Wuest_all_mod.csv')
SoilMoistureDataFrame <- read.csv('data/SM_points_all.csv')
rasterSM <-  raster('data/MC-20130506-SM.tif')
rasterSM <- projectRaster(rasterSM, crs=prj_string)
#preprocess soil moisture
rastercrop <- crop(rasterSMproj, DEM)

#steps for soil moisture dataset?


#mask for catchment

#EXTENTS?
DEM
##relative length to channel
source('Scripts/ChannelNetwork.R')
lrel <- HydroNetwork(DEM, 40000)
plot(lrel)
# aspect map
aspect <- terrain(DEM, opt='aspect', unit='degrees', neighbors = 8)

# slope map
slope <- terrain(DEM, opt='slope', unit='degrees', neighbors = 8)

# profile curvature
profileCurv <-  raster('data/Wuest_prcurv.tif')
#profileCurv <-  raster('data/Tar_profcurv.tif')
plot(profileCurv)

#plot terrain attributes

#visualize aspect
par(mfrow = (c(1,1)))
colors <- c("lightblue","darkblue","black","white")
labels=c("North","East","South","West")
plot(aspect, col=colors, legend=labels, main="Aspect")
legend("bottomright", fill=colors,legend=labels, title="Legend")

#visualize distance to channel
colfunc <- colorRampPalette(c("darkblue", "white"))
plot(lrel, col=colfunc(10), main="Distance to channel")

#visualize slope
colfunc <- colorRampPalette(c("green","yellow", "red"))
plot(slope, col=colfunc(255), main="Slope")

#visualize profile curvature
colfunc <- colorRampPalette(c("white","black"))
plot(profileCurv, col=colfunc(255),main="Profile curvature")

## Svetlitchnyi's functie


# north or southern hemisphere

# find convex, concave cells
source('Scripts/WeightingRaster.R')
Weighting <- getWeightingRaster(aspect, slope, profileCurv, lrel)
plot(Weighting)

source('Scripts/GetSWC.R')
SM <- getSoilMoistureContent(SoilMoistureDataFrame, Weighting)
writeRaster(SM, 'SM.tif')

#Visualize Soil Moisture
colfunc1 <- colorRampPalette(c("white", "blue"))
plot(SM, col=colfunc1(255))
colfunc2 <- colorRampPalette(c("blue", "darkblue"))


values$soilmoisture <- data.frame(dataframe["DAILY_MEAN"])
head(values)
#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('red','blue'))
head(values[1])
#This adds a column of color values
# based on the y values
values$Col <- rbPal(10)[as.numeric(cut(values[1],breaks = 10))]
points(dataframe, dataframe@DAILY_MEAN, ex=0.5,  pch=16, col=colfunc1(255), add=T)


spplot(dataframe, "DAILY_MEAN", ex=0.5,  pch=16, col=colfunc2(10), add=T)
dataframe[4]
dataframe <- SpatialPointsDataFrame(SoilMoisturePoints, SoilMoistureDataFrame, proj4string=CRS(prj_string))
EstimatedSM <- extract(SM, SoilMoisturePoints,  pch=16, sp=T)
difference <- data.frame(EstimatedSM) - data.frame(dataframe["DAILY_MEAN"])

dataframe["DAILY_MEAN"]
?extract
# for rasters take the pixel, and overlay