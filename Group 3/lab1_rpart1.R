require(rpart)
library(dplyr)
head(titanic_train)
Titanic_rpart <- rpart(Survived ~ Sex+Age, data = titanic_train)
plot(Titanic_rpart) # try some different plot options
text(Titanic_rpart) # try some different text options

# build the  tree
fitM <- rpart(Survived ~ Sex+Age, method="anova", data=titanic_train)
printcp(fitM) # display the results
plotcp(fitM)
summary(fitM)
par(mfrow=c(1,2)) 
rsq.rpart(fitM) # visualize cross-validation results
# plot tree
plot(fitM, uniform=TRUE, main="Regression Tree for Titanic ")
text(fitM, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree
pfitM<- prune(fitM, cp=0.01160389) # from cptable??? adjust this to see the effect
# plot the pruned tree
plot(pfitM, uniform=TRUE, main="Pruned Regression Tree for Titanic")
text(pfitM, use.n=TRUE, all=TRUE, cex=.8)
