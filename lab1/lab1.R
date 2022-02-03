EPI_data <- read.csv("D:/Files/College_class/2022 RPI/Data Analytics/Lab1/2010EPI_data.csv", skip = 1,header = TRUE)

View(EPI_data)
#
attach(EPI_data) 	# sets the €˜default€™ object
#fix(EPI_data) 	# launches a simple data editor
EPI 			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array



hist(E, seq(30., 95., 1.0), prob=TRUE)
lines(density(E,na.rm=TRUE,bw=1.)) 
rug(E)
plot(ecdf(E), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(E)
qqline(E)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

BIODIVERSITY
tf <- is.na(BIODIVERSITY)
B <- BIODIVERSITY[!tf]

hist(B, seq(0., 100., 5.0), prob=TRUE)
lines(density(B,na.rm=TRUE,bw=1.)) 
rug(B)
plot(ecdf(B), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(B)
qqline(B)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

CLIMATE
tf <- is.na(CLIMATE)
C <- CLIMATE[!tf]

hist(C, seq(0., 100., 5.0), prob=TRUE)
lines(density(C,na.rm=TRUE,bw=1.)) 
rug(C)
plot(ecdf(C), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(C)
qqline(C)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

DALY
tf <- is.na(DALY)
D <- DALY[!tf]

boxplot(E, D)
qqplot(E, D)
qqplot(E, ENVHEALTH[!(is.na(ENVHEALTH))])


