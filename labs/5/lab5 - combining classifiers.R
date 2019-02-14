################################################################
#
# Combining machine learning algorithms
#
################################################################

#install.packages("CORElearn")
library(CORElearn)

vehicle <- read.table("vehicle.txt", sep=",", header = T)
summary(vehicle)


set.seed(8678686)
sel <- sample(1:nrow(vehicle), size=as.integer(nrow(vehicle)*0.7), replace=F)
learn <- vehicle[sel,]
test <- vehicle[-sel,]


table(learn$Class)
table(test$Class)


CA <- function(observed, predicted)
{
  t <- table(observed, predicted)
  
  sum(diag(t)) / sum(t)
}


#
# Voting
#

modelDT <- CoreModel(Class ~ ., learn, model="tree")
modelNB <- CoreModel(Class ~ ., learn, model="bayes")
modelKNN <- CoreModel(Class ~ ., learn, model="knn", kInNN = 5)

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test$Class, predDT)
caDT

predNB <- predict(modelNB, test, type="class")
caNB <- CA(test$Class, predNB)
caNB

predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test$Class, predKNN)
caKNN

# combine predictions into a data frame
pred <- data.frame(predDT, predNB, predKNN)
pred

# the class with the most votes wins
voting <- function(predictions)
{
  res <- vector()
  
  for (i in 1 : nrow(predictions))  	
  {
    vec <- unlist(predictions[i,])
    res[i] <- names(which.max(table(vec)))
  }
  
  factor(res, levels=levels(predictions[,1]))
}

predicted <- voting(pred)
CA(test$Class, predicted)




#
# Weighted voting
#

predDT.prob <- predict(modelDT, test, type="probability")
predNB.prob <- predict(modelNB, test, type="probability")
predKNN.prob <- predict(modelKNN, test, type="probability")

# combine predictions into a data frame
pred.prob <- caDT * predDT.prob + caNB * predNB.prob + caKNN * predKNN.prob
pred.prob

# pick the class with the highest score
highest <- apply(pred.prob, 1, which.max)
classes <- levels(learn$Class)
predicted <- classes[highest]

CA(test$Class, predicted)




#
# Bagging
#

#install.packages("ipred")
library(ipred)

bag <- bagging(Class ~ ., learn, nbagg=15)
bag.pred <- predict(bag, test, type="class")
CA(test$Class, bag.pred)




#
# Random forest as a variation of bagging
#

# install.packages("randomForest")
library(randomForest)

rf <- randomForest(Class ~ ., learn)
predicted <- predict(rf, test, type = "class")
CA(test$Class, predicted)




#
# Boosting
#

# install.packages("adabag")
library(adabag)

bm <- boosting(Class ~ ., learn)
predictions <- predict(bm, test)
names(predictions)

predicted <- predictions$class
CA(test$Class, predicted)


#
# Cross-validation
#

# the library ipred is needed to perform cross-validation
library(ipred)

# Tell the cross-validation which model to use
mymodel.coremodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
# Tell the cross-validation how to obtain predictions
mypredict.generic <- function(object, newdata){predict(object, newdata, type = "class")}
# force the predict function to return class labels only and also destroy the internal representation of a given model
mypredict.coremodel <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}
cvError <- errorest(Class~., data=learn, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "tree")
1 - cvError$error

#
# Caret package
#

install.packages("caret")
library(caret)
caretModel <- train(Class ~ ., learn, method="xgbLinear", eta=1, verbose=1)
pred <- predict(caretModel, test)
CA(test$Class, pred)

