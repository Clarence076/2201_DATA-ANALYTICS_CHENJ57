library(MASS)
names(iris)
dim(iris) # check the dimensions of the iris dataset, you will see 150 rows
#and 5 columns
head(iris)

# Creating the training dataset using the Random sampling using the
# sample() function
# we will allocate half of the dataset to train the model that we are planning
# to build.
# setting the seed value
set.seed(555)
Train <- sample(1:nrow(iris), nrow(iris)/2)
iris_Train <- iris[Train,] # Traning dataset
irist_Test <- iris[-Train,] # Testing dataset

fit1 <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris_Train)
predict1 <- predict(fit1, irist_Test)
predict1_class <- predict1$class


table1 <- table(predict1_class, iris_Train$Species)
table1

sum(diag(table1))/sum(table1)