library(dplyr)
multivariate <- read.csv("D:/Files/College_class/2022 RPI/Data Analytics/Lab1_Pt2/multivariate.csv", header = TRUE)
head(multivariate) 		
attach(multivariate)
tf <- is.na(head(multivariate)) # records True values if the value is NA
multivariate <- multivariate[!tf]

mm <-lm(Homeowners~Immigrant)
mm
summary(mm)$coef 

plot(Homeowners~Immigrant)
abline(mm)
abline(mm,col=2,lwd=3)
newImmigrantdata <- data.frame(Immigrant = c(0,  20))
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients


plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data=mtcars)
ggplot(mtcars, aes(x=wt,y=mpg)) +geom_point()

plot(pressure$temperature,pressure$pressure, type ='l')
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure /2, col='red')
points(pressure$temperature,pressure$pressure /2, col='blue')
qplot(pressure$temperature,pressure$pressure, geom='line')
qplot(temperature, pressure, data=pressure, geom='line')

ggplot(pressure, aes(x=temperature, y=pressure)) +geom_line()+geom_point()

barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl))) +geom_bar()

hist(mtcars$mpg)
hist(mtcars$mpg, breaks=5)
hist(mtcars$mpg, breaks=10)
hist(mtcars$mpg, breaks=11)
hist(mtcars$mpg, breaks=12)
qplot(mpg, data=mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth=4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth=5)



