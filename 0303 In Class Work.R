mtcars
head(mtcars)
str(mtcars)
model<-lm(mpg~cyl+wt, data=mtcars)
model
plot(model, pch = 18, col = 'red', which =c(4))
cooks.distance(model)
CooksDistance <-cooks.distance(model)
sort(round(CooksDistance, 5))

plot(CooksDistance)

typeof(CooksDistance)

library(ISLR)
head(Hitters)
dim(Hitters)
HittersData <- na.omit(Hitters)
dim(HittersData)
glimpse(HittersData)
SalaryPred <- lm(Salary~., data = HittersData)
summary(SalaryPred)

cooksdist <- cooks.distance(SalaryPred)
influential <- cooksdist[(cooksdist > 3 * mean(cooksdist, na.rm = TRUE))]
influential

names <-