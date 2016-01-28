#Droesen geo-solutions
#Jan Droesen
#28/01/2016

### Title: Script for downscaling soil moisture based on a Digital elevation model

### Libraries
library(raster)
library(rgdal)
library(sp)
library(graphics)

# Remove items from list
rm(list = ls())

# Set working directory
path = "C:/Users/gebruiker/Documents/Downscaling_script/"
setwd(path)


### User requirements (go through these steps):
      # 1. Should import a DEM (in this script two DEM's are given as example (uncomment one to see results))
      # 2. If projection DEM is unknown, give projection in format proj4
      # 3. Give soil moisture data (raster or points), three examples or given (uncomment one to see results)
      # 4. Last value to be set is the flow accumulation
      # 5. As R does not have a function for profile curvature choose one of the two examples
      

#1. import DEM
DEM <- raster('data/tarrawar.asc')
#DEM <- raster('data/dem5wuest.asc')


#if projection is unknown, set here projection
prj_string <- "+proj=tmerc +lat_0=0 +lon_0=6 +k=1 +x_0=2500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"
source('Scripts/projection.R')
DEM <- projectinput(DEM, prj_string)

plot(DEM, main="Digital elevation model", col=topo.colors(255))

# 3. Soil moisture data (raster or points)
SoilMoistureDataSet <- read.csv('data/SoilMoisturetarra.csv')
#SoilMoistureDataSet <- read.csv('data/SoilMoistureWuest.csv', sep=",")
#SoilMoistureDataSet <-  projectRaster(raster('data/MC-20130429-SM.tif'),crs=prj_string)

# 4. relative distance to channel
source('Scripts/ChannelNetwork.R')
accumulationvalue <- 15000 #give here the specified accumulation for a stream
lrel <- HydroNetwork(DEM, accumulationvalue)

# aspect map
aspect <- terrain(DEM, opt='aspect', unit='degrees', neighbors = 8)

# slope map
slope <- terrain(DEM, opt='slope', unit='degrees', neighbors = 8)

# profile curvature
profileCurv <-  raster('data/Tar_profcurv.tif')
#profileCurv <-  raster('data/Wuest_prcurv.tif')
#not working function for profile curvature:
#source('Scripts/ProfileCurvature.R')
#profileCurv <- DEMderiv(DEM, "prof.curvature", "evans")



### plot terrain attributes

# visualize aspect
par(mfrow = (c(2,2)))
colors <- c("lightblue","darkblue","blue","lightcyan4")
labels=c("North","East","South","West")
plot(aspect, col=colors, legend=labels, main="Aspect")
legend("bottomright", fill=colors,legend=labels, title="Legend")

# visualize distance to channel
colfunc <- colorRampPalette(c("darkblue", "white"))
plot(lrel, col=colfunc(10), main="Distance to channel")

# visualize slope
colfunc <- colorRampPalette(c("green","yellow", "red"))
plot(slope, col=colfunc(255), main="Slope")

# visualize profile curvature
colfunc <- colorRampPalette(c("white","black"))
plot(profileCurv, col=colfunc(255),main="Profile curvature")



### Svetlitchnyi's function

source('Scripts/WeightingRaster.R')
Weighting <- getWeightingRaster(aspect, slope, profileCurv, lrel)

source('Scripts/GetSWC.R')
SM <- getSoilMoistureContent(SoilMoistureDataSet, Weighting, slope)



### Visualizations

# Visualize Soil Moisture (raster)
par(mfrow = (c(1,1)))
colfunc1 <- colorRampPalette(c("white", "blue"))
plot(SM, col=colfunc1(255), main="Soil moisture content",  
     xlim=c((extent(DEM)[1]), (extent(DEM)[2])), ylim=c((extent(DEM)[3]), (extent(DEM)[4])))

# Visualize of soil moisture is a point dataset
rbPal <- colorRampPalette(c("white", "darkblue"))
SoilMoistureDataSet$col <- rbPal(255)[as.numeric(cut(SoilMoistureDataSet$SM, breaks = 255))]
plot(SoilMoistureDataSet[c("X", "Y")], pch=16, col=SoilMoistureDataSet$col,  
     xlim=c((extent(DEM)[1]), (extent(DEM)[2])), ylim=c((extent(DEM)[3]), (extent(DEM)[4])),
     main="Measured soil moisture content")

# Difference estimated and known SM
source('Scripts/DifferentEst.R')
EstimatedSMdf <- difEstReal(SM, SoilMoistureDataSet, prj_string)
difference1 <- as.matrix(EstimatedSMdf$difference)
rbPal <- colorRampPalette(c("red","yellow","green"))
EstimatedSMdf$col <- rbPal(255)[as.numeric(cut(difference1, breaks = 255))]
plot(EstimatedSMdf[c(2, 3)], pch=16, col=EstimatedSMdf$col,  
     xlim=c((extent(DEM)[1]), (extent(DEM)[2])), ylim=c((extent(DEM)[3]), (extent(DEM)[4])),
     main="Difference between estimated and measured SM")

