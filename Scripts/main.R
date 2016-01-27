#libraries
install.package(dynatopmodel)
library(dynatopmodel)
library(raster)
library(rgdal)
library(spgrass6)
setwd("/home/user/git/ScriptingProject")

#Title: Scripting for digital terrain modelling

#Documentation: Must, Should, Could, Would
#Must: get an soil water content map for one catchment, nicely visualized (maybe some mozaiking)

#Should: compare the estimated values of soil moisture with actual soil moisture data (point measurements) and uncertainty map, e.g. with R squared
	
#Could: use for hydrological models
	
#Would: script for every digital elevation model and all raster/vector datasets

rm(list = ls())

#import DEM
list.files(path ="/data/Master thesis/Datasets/Tarrawarra", pattern = glob2rx('*.dem'))
list <- list.files(path="/media/user/disk/Resultaat/Wuestebach", pattern = glob2rx('*tif'))
list
##import and preprocess data raster/vector

#projection
prj_string <- CRS('+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 

#problems with .dem
DEM <- raster('/media/user/disk/Master thesis/Datasets/Tarrawarra/tarrawar.asc')
#Duits;
#"+proj=tmerc +lat_0=0 +lon_0=6 +k=1 +x_0=2500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"

DEM <- raster('data/dem5wuest.asc')
DEMproj <-  projectRaster(DEM, crs="+proj=tmerc +lat_0=0 +lon_0=6 +k=1 +x_0=2500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7")
DEMproj
DEM
plot(rasterSMproj)
plot(DEM, col="red", add=T)
#points SM
SoilMoistureDataFrame <- read.csv('data/Wuest_all_mod.csv')
#raster SM
rasterSM <-  raster('data/MC-20130506-SM.tif')
projection(rasterSM) <- prj_string
projection(SoilMoisturePoints) <- prj_string
projection(SoilMoisturePoints)
rasterSMproj <- projectRaster(rasterSM, crs="+proj=tmerc +lat_0=0 +lon_0=6 +k=1 +x_0=2500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7")
plot(rasterSMproj)

rastercrop <- crop(rasterSMproj, DEM)
plot(DEM)

colfunc <- colorRampPalette(c("white", "darkblue"))
plot(rastercrop, col=colfunc(10))

DEM
#project CRS
# automate this with input EPSG number

#mask for catchment

#EXTENTS?

##relative length to channel
source('Scripts/ChannelNetwork.R')
lrel <- HydroNetwork(DEM, 60000)

# aspect map
aspect <- terrain(DEM, opt='aspect', unit='degrees', neighbors = 8)

# slope map
slope <- terrain(DEM, opt='slope', unit='degrees', neighbors = 8)

# profile curvature
source(ProfileCurvature)
profileCurv <-  raster('data/Wuest_prcurv.tif')
projection(profileCurv) <- '+proj=tmerc +lat_0=0 +lon_0=6 +k=1 +x_0=2500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs' 

## Svetlitchnyi's function

coordsysangl = 0

#north or southern hemisphere
hemisphere <- "north"
ke <- aspect
# find convex, concave cells
getWeightingRaster <- function(aspect, slope, profileCurv, lrel) { # optional(hemisphere, )
	coordsysangl = 0
	ke <- aspect
	#north/south
	#parameters
	alpha0 <- cellStats(slope, stat='mean', na.rm=TRUE)
	ke[ke<(45+coordsysangl)] <- -0.01 #north
	ke[ke>=(45+coordsysangl) & ke<(135+coordsysangl)] <- 0.002 #east
	ke[ke>=(135+coordsysangl) & ke<(225+coordsysangl)] <- 0.005 #south
	ke[ke>=(225+coordsysangl) & ke<(315+coordsysangl)] <- -0.003 #west
	ke[ke>=(315+coordsysangl)] <- -0.01 #north
	Ka <- (1-ke*slope)/(1-ke*alpha0)
	a <- lrel
	b <- lrel
	a_1 <- calc(lrel, fun=function(lrel){(1-0.2*lrel**0.5)}, filename='a_1', overwrite=TRUE)
	a_2 <- calc(lrel, fun=function(lrel){(0.77+0.43*lrel**1.47)}, filename='a_2') 
	a_3 <- calc(lrel, fun=function(lrel){(0.77+0.70*lrel**4.0)},filename='a_3')
	read.asciigrid()
	a[lrel<=0.167] = a_1
	a[lrel>0.167 & lrel<=0.833] = a_2
	a[lrel>0.833 & lrel<=1] = a_3

	# calculating a
	b[lrel>=0 & lrel<=0.833] <- 1
	b[lrel>0.833] <- 1.5
	
	Kw <- profileCurv
	calcKw <- function(x, y, z, q){
		(x + 0.16*cos(y) + 0.09*sin(y))*z
		(q + 0.14*cos(y) + 0.10*sin(y) - 0.02*cos(2*y))*z
	}
	calc()
	Kw[Kw>0] <-  ((a + 0.16*cos(aspect) + 0.09*sin(aspect))*Ka)#(A+180
	Kw[Kw<=0] <- ((b + 0.14*cos(aspect) + 0.10*sin(aspect) - 0.02*cos(2*aspect))*Ka)
	plot(Kw)
}


getSoilMoistureContent <- function(SoilMoistureDataFrame, Kw){
	SoilMoisturePoints <- SpatialPoints(SoilMoistureDataFrame[,5:6], proj4string=prj_string)
	SoilMoistureDataFrame$Slope <- extract(slope, SoilMoisturePoints, pch=16)
	refSoilMoisture <- SoilMoistureDataFrame$DAILY_MEAN[SoilMoistureDataFrame["Slope"] == min(SoilMoistureDataFrame["Slope"])]
	SM <-	Kw*refSoilMoisture
}
plot(SM)
	#window average could
	# wetness: combine
	#Kw=windowaverage(Kw, 10.0);


ndvi3 <- overlay(x=gewata[[3]], y=gewata[[4]], fun=ndvOver))

Wetness <- getWetnessCoefficient(aspect, slope, profileCurv, lrel)
plot(Wetness, zlim=c(0,1))
# load the sp and rgdal packages
library(sp)
library(rgdal)

# make spatial points object


### visualization


#visualize aspect
par(mfrow = (c(2,2)))
colors <- c("lightblue","darkblue","black","white")
labels=c("North","East","South","West")
plot(ke, col=colors, legend=labels, main="Aspect")
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

plot(Kw)
colfunc <- colorRampPalette(c("white", "darkblue"))
points(SoilMoisturePoints, pch=16,col=colfunc)

plot(rasterSM)
points(SoilMoisturePoints, pch=16,col=colfunc(255))

# for rasters take the pixel, and overlay



#plots
#all
