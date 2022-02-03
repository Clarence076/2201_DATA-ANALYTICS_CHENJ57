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



