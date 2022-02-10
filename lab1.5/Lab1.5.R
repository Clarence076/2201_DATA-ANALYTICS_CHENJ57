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

plot(ToothGrowth$len)
boxplot(len~supp, data = ToothGrowth)
boxplot(len~supp + dose, data = ToothGrowth)


qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")
qplot(supp, len,data=ToothGrowth, geom="boxplot")
ggplot(ToothGrowth,aes(x=supp, y=len))+geom_boxplot()

qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom="boxplot")
qplot(interaction(supp, dose), len, data=ToothGrowth, geom="boxplot")
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len))+geom_boxplot()


library(gcookbook)
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat = "identity")
BOD
str(BOD)
ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat = "identity")

ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat = "identity")

ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", fill="lightblue", colour = "red")
ggplot(BOD, aes(x=factor(Time), y=demand)) +geom_bar(stat = "identity", fill="orange", colour = "red")

cabbage_exp
ggplot(cabbage_exp, aes(x=Date, fill=Cultivar)) + geom_bar(position = "dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity")
ggplot(diamonds, aes(x=cut)) +geom_bar() 
data("diamonds")
diamonds
ggplot(diamonds,aes(x=carat)) + geom_bar()
ggplot(diamonds, aes(x=carat)) + geom_histogram()

ups <- subset(uspopchange, rank(Change)>40)
ups
ggplot(ups, aes(x=Abb, y= Change, fill=Region)) + geom_bar(stat = "identity")
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) +geom_bin2d()
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) + geom_col()
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", colour= "red") + scale_fill_manual(values=c("#669933", "#FFCC66")) + xlab("US-States")

csub <- subset(climate, source="Berkeley" & Year >= 1900)
csub
csub$pos <-csub$Anomaly10y>= 0
csub
ggplot(csub, aes(x=Year, y=Anomaly10y, fill= pos)) + geom_bar(stat = "identity", position = "identity")


ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", colour="black", size=0.25) + scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)

ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity")
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity", width = 0.5)
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity", width = 0.65)

ggplot(cabbage_exp, aes(x=Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = "dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.7))










