# Data Vehicle (four classes)
library(ipred)
library(rpart)
library(adabag)

library(mlbench)
data(Vehicle)
l <- length(Vehicle[,1])
sub <- sample(1:l,2*l/3)
Vehicle.bagging <- bagging(Class ~.,data=Vehicle[sub,-1],mfinal=40, 
	control=rpart.control(maxdepth=5))
Vehicle.bagging.pred <- predict.bagging(Vehicle.bagging,newdata=Vehicle[-sub,-1])
Vehicle.bagging.pred$confusion
Vehicle.bagging.pred$error
