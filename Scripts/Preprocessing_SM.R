SoilMoistureDataFrame1 <- read.csv('data/Wuest_all_mod.csv')
SoilMoistureDataFrame2 <- read.csv('data/SM_points_all.csv')

#empty dataframe:
SoilMoistureWuest <- data.frame

SoilMoistureWuest$ID <- SoilMoistureDataFrame1["ID"]
SoilMoistureWuest$X <- SoilMoistureDataFrame1["X.COORD"]
SoilMoistureWuest$Y <- SoilMoistureDataFrame1["Y.COORD"]
SoilMoistureWuest$SM <- SoilMoistureDataFrame1["DAILY_MEAN"]
SoilMoistureWuest$Time <- SoilMoistureDataFrame1["TIMESTAMPT"]

data.frame(SoilMoistureWuest)
head(SoilMoistureDataFrame1)


?data.frame()
