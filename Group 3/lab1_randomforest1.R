require(randomForest)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF) 	# view results
importance(fitKF) # importance of each predictor
#
fitSwiss <- randomForest(Fertility ~ Agriculture + Education + Catholic, data = swiss)
print(fitSwiss) # view results
importance(fitSwiss) # importance of each predictor
varImpPlot(fitSwiss)

plot(fitSwiss)

getTree(fitSwiss,1, labelVar=TRUE)

help(randomForest) # look at all the package contents and the randomForest method options

# look at rfcv - random forest cross-validation - 
help(rfcv)

# other data....
data(imports85)
library(dplyr)

# perform randomForest and other tree methods.....
imports85 <- na.omit(imports85, target.colnames='price')
fit85 <- randomForest(price ~ aspiration + fuelType + bodyStyle,   data=imports85)
print(fit85) 	# view results
importance(fit85) # importance of each predictor


plot(fit85)

getTree(fit85,1, labelVar=TRUE)