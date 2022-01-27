EPI_data <- read.csv("D:/Files/College_class/2022 RPI/Data Analytics/Lab1/2010EPI_data.csv", skip = 1,header = TRUE)

View(EPI_data)
#
attach(EPI_data) 	# sets the €˜default€™ object
fix(EPI_data) 	# launches a simple data editor
EPI 			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array



hist(E, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.)) 
rug(EPI)
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(E)
qqline(E)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)