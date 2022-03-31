BrooklynData <- read.csv('D:/Files/College_class/2022 RPI/Data Analytics/Assignment 5/rollingsales_brooklyn.csv')
BrooklynData$SALE.PRICE = as.numeric(gsub("\\,", "", sub("\\$","", BrooklynData$SALE.PRICE)))
BrooklynData$GROSS.SQUARE.FEET = as.numeric(gsub(",", "", BrooklynData$GROSS.SQUARE.FEET))
BrooklynData$LAND.SQUARE.FEET = as.numeric(gsub(",", "", BrooklynData$LAND.SQUARE.FEET))

#part a
BrooklynDataTF = is.na(BrooklynData$LAND.SQUARE.FEET) | is.na(BrooklynData$SALE.PRICE) | is.na(BrooklynData$GROSS.SQUARE.FEET)
BrooklynData = BrooklynData[!BrooklynDataTF,]
sum(BrooklynDataTF)

summary(BrooklynData$SALE.PRICE)
hist(BrooklynData$SALE.PRICE, breaks=10)
sum(BrooklynData$SALE.PRICE == 0)
sum(BrooklynData$SALE.PRICE != 0) + sum(BrooklynData$SALE.PRICE == 0)
boxplot(BrooklynData$SALE.PRICE,main="Sales Price Box Plot", ylab = 'Price (Dollars)')
tf <- (BrooklynData$SALE.PRICE < 1000000 & BrooklynData$SALE.PRICE != 0)
summary(BrooklynData$SALE.PRICE[tf])
hist(BrooklynData$SALE.PRICE[tf], breaks = 40, xlab = 'Sales Price', main = "Histogram of Sales Price greater than 0 and less than 1 Million")
tf <- (BrooklynData$SALE.PRICE < 100000000 & BrooklynData$SALE.PRICE != 0 & BrooklynData$GROSS.SQUARE.FEET != 0)

qqplot(BrooklynData$GROSS.SQUARE.FEET[tf],BrooklynData$SALE.PRICE[tf], ylab = 'Sales Price', xlab='Gross Square Feet', main = 'QQ Plot of Gross Sq. Ft. Vs Sales Price, with 0 values removed, Price < 100 Million')

tf <- (BrooklynData$SALE.PRICE < 100000000 & BrooklynData$SALE.PRICE != 0 & BrooklynData$LAND.SQUARE.FEET != 0)

qqplot(BrooklynData$SALE.PRICE[tf], BrooklynData$LAND.SQUARE.FEET[tf], main = 'QQ Plot of Land Sq. Ft. Vs Sales Price, with 0 values removed, Price < 100 Million')


#part b
tf <- (BrooklynData$SALE.PRICE != 0 & BrooklynData$LAND.SQUARE.FEET != 0 & BrooklynData$GROSS.SQUARE.FEET != 0)

tf[is.na(tf)] <- FALSE
BrooklynData <- BrooklynData[tf,]

model1 <- lm(SALE.PRICE~GROSS.SQUARE.FEET + LAND.SQUARE.FEET, data = BrooklynData)
plot(model1,pch =4, which=c(4))
summary(model1)
CooksDist <- round(cooks.distance(model1),5)
#sort(CooksDist, decreasing = TRUE)
influential <- (CooksDist)[(CooksDist > (3* mean(CooksDist, na.rm = TRUE)))]
#\influential <- (CooksDist)[(CooksDist > (0.25/ length(BrooklynData)))]

influential <- names(influential)
influential <- as.numeric(influential[!is.na(influential)])
length(influential)

outliers <- BrooklynData[influential,]
library(dplyr)
BrooklynData_wo_Outliers <- BrooklynData %>% anti_join(outliers)





#part c\
BrooklynData <- read.csv('D:/Files/College_class/2022 RPI/Data Analytics/Assignment 5/rollingsales_brooklyn.csv')
BrooklynData$SALE.PRICE = as.numeric(gsub("\\,", "", sub("\\$","", BrooklynData$SALE.PRICE)))
BrooklynData$GROSS.SQUARE.FEET = as.numeric(gsub(",", "", BrooklynData$GROSS.SQUARE.FEET))
BrooklynData$LAND.SQUARE.FEET = as.numeric(gsub(",", "", BrooklynData$LAND.SQUARE.FEET))
tf <- (BrooklynData$GROSS.SQUARE.FEET != 0 & BrooklynData$SALE.PRICE != 0)
tf[is.na(tf)] <- FALSE
BrooklynData <- BrooklynData[tf,]

#Brooklyn_inf = fivenum(BrooklynData_wo_Outliers$SALE.PRICE, na.rm=TRUE)
#interquartileR <- Brooklyn_inf[4] - Brooklyn_inf[2]
#lower <- Brooklyn_inf[2] - interquartileR*1.5
#upper <- Brooklyn_inf[4] + interquartileR*1.5
#outliers <- BrooklynData_wo_Outliers[BrooklynData_wo_Outliers$SALE.PRICE < lower | BrooklynData_wo_Outliers$SALE.PRICE > upper, ]

#BrooklynData_wo_Outliers <- BrooklynData_wo_Outliers[BrooklynData_wo_Outliers$SALE.PRICE > lower & BrooklynData_wo_Outliers$SALE.PRICE < upper, ]

set.seed(421)
train <- sample(nrow(BrooklynData_wo_Outliers), 0.5*nrow(BrooklynData_wo_Outliers),replace=FALSE)
Trainset <- BrooklynData_wo_Outliers[train,]
Validset <- BrooklynData_wo_Outliers[-train,]
model2 <- lm(SALE.PRICE~GROSS.SQUARE.FEET + LAND.SQUARE.FEET, data=Trainset)
summary(model2)
predTest <- predict(model2, Validset)
mean((predTest - Validset$SALE.PRICE)^2)
cor(predTest,Validset$SALE.PRICE)^2


plot(model2,pch =4, which=c(1))

plot(model1,pch =4, which=c(1))

#part d
BrooklynData_wo_Outliers =BrooklynData
BrooklynData_wo_Outliers$SALE.PRICE <- scale(BrooklynData$SALE.PRICE, center = TRUE, scale = TRUE)
BrooklynData_wo_Outliers$GROSS.SQUARE.FEET <- scale(BrooklynData$GROSS.SQUARE.FEET, center = TRUE, scale = TRUE)
BrooklynData_wo_Outliers$LAND.SQUARE.FEET <- scale(BrooklynData$LAND.SQUARE.FEET, center = TRUE, scale = TRUE)
#Brooklyn_inf = fivenum(BrooklynData$SALE.PRICE, na.rm=TRUE)
#interquartileR <- Brooklyn_inf[4] - Brooklyn_inf[2]
#lower <- Brooklyn_inf[2] - interquartileR*1.5
#upper <- Brooklyn_inf[4] + interquartileR*1.5
#BrooklynData_wo_Outliers <- BrooklynData[BrooklynData$SALE.PRICE > lower & BrooklynData$SALE.PRICE < upper, ]


library(randomForest)
set.seed(100)
train <- sample(nrow(BrooklynData_wo_Outliers), 0.7*nrow(BrooklynData_wo_Outliers),replace=FALSE)
Trainset <- BrooklynData_wo_Outliers[train,]
Validset <- BrooklynData_wo_Outliers[-train,]
model3 <- randomForest(SALE.PRICE~GROSS.SQUARE.FEET + LAND.SQUARE.FEET, data=Trainset,mtry = 1, ntree =400, importance=TRUE)
model3
predTrain <- predict(model3, Trainset)
mean((predTrain - Trainset$SALE.PRICE)^2)

plot(Trainset$SALE.PRICE, predTrain, main='Actual vs Predicted Train')
abline(a=0, b=1)
cor(predTrain, Trainset$SALE.PRICE)^2
predTest <- predict(model3, Validset)
mean((predTest - Validset$SALE.PRICE)^2)
cor(predTest,Validset$SALE.PRICE)^2
plot(Validset$SALE.PRICE, predTest, main='Actual vs Predicted Test')
abline(a=0, b=1)




#PART 2


BrooklynData <- read.csv('D:/Files/College_class/2022 RPI/Data Analytics/Assignment 5/rollingsales_brooklyn.csv')
BrooklynData$SALE.PRICE = as.numeric(gsub("\\,", "", sub("\\$","", BrooklynData$SALE.PRICE)))
BrooklynData$GROSS.SQUARE.FEET = as.numeric(gsub(",", "", BrooklynData$GROSS.SQUARE.FEET))
BrooklynData$LAND.SQUARE.FEET = as.numeric(gsub(",", "", BrooklynData$LAND.SQUARE.FEET))
BrooklynData$TOTAL.UNITS = as.numeric(gsub(",", "", BrooklynData$TOTAL.UNITS))
BrooklynData$YEAR.BUILT = as.numeric(gsub(",", "", BrooklynData$YEAR.BUILT))

tf <- (BrooklynData$GROSS.SQUARE.FEET != 0 & BrooklynData$SALE.PRICE != 0 & !is.na(BrooklynData$TOTAL.UNITS) & !is.na(BrooklynData$YEAR.BUILT))
tf[is.na(tf)] <- FALSE
BrooklynData <- BrooklynData[tf,]

BrooklynData$SALE.PRICE = c(scale(BrooklynData$SALE.PRICE, center = TRUE, scale = TRUE))
BrooklynData$GROSS.SQUARE.FEET = c(scale(BrooklynData$GROSS.SQUARE.FEET, center = TRUE, scale = TRUE))
BrooklynData$LAND.SQUARE.FEET = c(scale(BrooklynData$LAND.SQUARE.FEET, center = TRUE, scale = TRUE))
BrooklynData$YEAR.BUILT = c(scale(BrooklynData$YEAR.BUILT, center = TRUE, scale = TRUE))
BrooklynData$TOTAL.UNITS = c(scale(BrooklynData$TOTAL.UNITS, center = TRUE, scale = TRUE))

train <- sample(nrow(BrooklynData), 0.7*nrow(BrooklynData),replace=FALSE)
Trainset <- BrooklynData[train,]
Validset <- BrooklynData[-train,]



#desiscion tree

library("caret")
library("rpart")
trControl <- trainControl(method="repeatedcv")
grid <- expand.grid(cp = 0.001)

model4 <- train(TOTAL.UNITS~GROSS.SQUARE.FEET + LAND.SQUARE.FEET + SALE.PRICE + YEAR.BUILT, data = Trainset, trControl=trControl,method="rpart", tuneGrid = grid, maxdepth = 30)
model4$results
#model4$resample
varImp(model4)
predTrain <- predict(model4, data=Trainset)
mean((predTrain - Trainset$TOTAL.UNITS)^2)
cor(predTrain,Trainset$TOTAL.UNITS)^2

plot(Trainset$TOTAL.UNITS, predTrain, main='Actual vs Predicted Train')
abline(a=0, b=1)
predTest <- predict(model4, newdata=Validset)
mean((predTest - Validset$TOTAL.UNITS)^2)
cor(predTest,Validset$TOTAL.UNITS)^2
plot(Validset$TOTAL.UNITS, predTest, main='Actual vs Predicted Test')
abline(a=0, b=1)



train <- sample(nrow(BrooklynData), 0.7*nrow(BrooklynData),replace=FALSE)
Trainset <- BrooklynData[train,]
Validset <- BrooklynData[-train,]

#Ridge? not working
library('glmnet')
x_train <- data.frame(c(Trainset$GROSS.SQUARE.FEET), c(Trainset$LAND.SQUARE.FEET), c(Trainset$SALE.PRICE), c(Trainset$YEAR.BUILT))
colnames(x_train) <- c('GROSS.SQUARE.FEET', 'LAND.SQUARE.FEET', 'SALE.PRICE', 'YEAR.BUILT')
lambdas <- 10^seq(2, -3, by = -.01)
model5 <- cv.glmnet(data.matrix(x_train), Trainset$TOTAL.UNITS, lambda = lambdas, alpha =0)
min_lambda <- model5$lambda.min
min_lambda
coef(model5)
plot(model5) 


ridgepred <- predict(model5, s = min_lambda, newx = data.matrix(x_train))
ridgemse <- mean((ridgepred[,1] - Trainset$TOTAL.UNITS)^2)
ridgemse
ridgersq <- cor(ridgepred[,1], Trainset$TOTAL.UNITS) ^ 2
ridgersq
plot.new()
plot(Trainset$TOTAL.UNITS, ridgepred[,1], main='Actual vs Predicted Train')
abline(a=0, b=1)

x_test <- data.frame(Validset$GROSS.SQUARE.FEET, Validset$LAND.SQUARE.FEET, Validset$SALE.PRICE, Validset$YEAR.BUILT)
colnames(x_test) <- c('GROSS.SQUARE.FEET', 'LAND.SQUARE.FEET', 'SALE.PRICE', 'YEAR.BUILT')

ridgepred <- predict(model5, s = min_lambda, newx = data.matrix(x_test))
ridgemse <- mean((ridgepred[,1] - Validset$TOTAL.UNITS)^2)
ridgemse
ridgersq <- cor(ridgepred[,1], Validset$TOTAL.UNITS) ^ 2
ridgersq
plot.new()
plot(Validset$TOTAL.UNITS, ridgepred[,1], main='Actual vs Predicted Test')
abline(a=0, b=1)