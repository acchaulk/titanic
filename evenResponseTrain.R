#both train and test set
titanic.eRtrain <- read.csv("evenResponseTrain.csv")
titanic.eRtest <- read.csv("fillAgeWithMeans Test set.csv")

#factor all categorical variables
factorFn <- function(titanic){
  titanic$Embarked <- factor(titanic$Embarked)
  titanic$Pclass <- factor(titanic$Pclass)
  titanic$Sex <- factor(titanic$Sex)
  titanic$Title <- factor(titanic$Title)
  titanic$Survived <- factor(titanic$Survived)
  
  return(titanic)
}
str(factorFn(titanic.eRtrain))
str(factorFn(titanic.eRtest))

titanic.test <- factorFn(titanic.eRtest)
titanic.train <- factorFn(titanic.eRtrain)

titanic.test[, "Survived"]=0
titanic.test$Survived = factor(titanic.test$Survived)

# the ratio of 0 to 1
sum(titanic.train$Survived==0)/sum(titanic.train$Survived==1)
## the ratio is 1.605263
summary(titanic.train$Survived)
#   0   1 
#  549 342 

set.seed(222)
train <- sample(c(TRUE, FALSE, TRUE, TRUE), nrow(titanic.train), replace = TRUE)
test <- !train

titanic.train.needed <- titanic.train[, -c(2,5,7,12)]

# perform k-folds-cross-validation

library(caret)
set.seed(12)
folds <- createFolds(1:nrow(titanic.train.needed))
k = length(folds)

# Logistic regression (doesn't work)
glm.cv.function = function(formula, data, k){
  
  glm.cv.errors <- array(0, dim = k)
  
  for(j in 1:k){
    fit <- glm(formula, data = data[-folds[[j]],], family = binomial)
    probs <- predict(fit, data[folds[[j]],], type = "response")
    pred <- rep(0, length(probs))
    pred[probs>0.5] = 1
    glm.cv.errors[j] = mean(pred!=data[folds[[j]],]$Survived)
  }
  
  mean(glm.cv.errors)
}

glm.cv.function(Survived ~ Age + SibSp + Sex + Pclass + Title, titanic.train.needed, k)


# decision tree
library(rpart)
library(rpart.plot)
dt.titanic.fit <- rpart(Survived~., data = titanic.train.needed, subset = train,control = rpart.control(minsplit = 10, minbucket = 5, cp = 0.02, xval = 10))
rpart.plot(dt.titanic.fit) # only Title, Pclass and FamilySize works when using cp = 0.02
summary(dt.titanic.fit)
dt.titanic.pred <- predict(dt.titanic.fit, titanic.train.needed[test,], type = "class")

mean(dt.titanic.pred!=titanic.train.needed[test,]$Survived)
# mis-class rate = 0.2
table(dt.titanic.pred, titanic.train.needed[test,]$Survived)
# dt.titanic.pred    0   1
#                0  129 45
#                1  10  91

# bagging
library(randomForest)

bag.titanic.fit <- randomForest(Survived~., data = titanic.train.needed, subset = train, mtry = 9, ntree = 1000, importance = T)
bag.pred <- predict(bag.titanic.fit, titanic.train.needed[test,], type = "class")
table(bag.pred, titanic.train.needed[test,]$Survived)
# bag.pred   0   1
#         0 124  17
#         1  15  119
mean(bag.pred!=titanic.train.needed[test,]$Survived)
# mis-class rate = 0.1163636
importance(bag.titanic.fit)
varImpPlot(bag.titanic.fit)


# radndomforest
library(pROC)
rf.titanic.fit <- randomForest(Survived~., data = titanic.train.needed, subset = train, mtry = 5,ntree = 1000, importance = T)
rf.pred <- predict(rf.titanic.fit, titanic.train.needed[test,], type = "class")
rf.prob <- predict(rf.titanic.fit, titanic.train.needed[test,], type = "prob")
table(rf.pred, titanic.train.needed[test,]$Survived)
# rf.pred   0   1
#       0 133  19
#       1  6  117
mean(rf.pred!=titanic.train.needed[test,]$Survived)
# miss-class rate = 0.12 (mtry = 3), 0.1127273(mtry=4), 0.09090909(mtry=5)
importance(rf.titanic.fit)
varImpPlot(rf.titanic.fit)

roc(titanic.train.needed[test,]$Survived, rf.prob[,2], auc = TRUE, plot = TRUE)
# AUC = 0.9518

# boosting
library(adabag)
boost.titanic.fit <- boosting(Survived~., data = titanic.train.needed[train,],boos = FALSE, mfinal = 100)
boost.titanic.pred <- predict(boost.titanic.fit, titanic.train.needed[test,], n.trees = 100, type = "response")
boost.titanic.pred$confusion
mean(boost.titanic.pred$class!=titanic.train.needed[test,]$Survived)
# miss-class rate = 0.1731449

# run boosting on full training set
boost.titanic.full.er <- boosting(Survived~., data = titanic.train.needed,boos = FALSE, mfinal = 100)
boost.titanic.full.er.pred <- predict(boost.titanic.full.er, titanic.test[, -c(2,5,7,12)], type = "response")

write.csv(data.frame(PassengerId=titanic.test$PassengerId, Survived=boost.titanic.full.er.pred$class), "boosting on dataset3.csv", row.names = FALSE)
# kaggle score = 0.74641 

# run model on full training set
rf.titanic.fit.full.er <- randomForest(Survived~., data = titanic.train.needed, ntree = 1000, importance = T)
# run model on original test set

# set levels in test eqaul to that in training
levels(titanic.test$Title) <- levels(titanic.train.needed$Title)

test.rf.pred <- predict(rf.titanic.fit.full.er, newdata = titanic.test[, -c(2,5,7,12)], type = "response")

titanic.test[, "Survived"] = test.rf.pred

submitTest <- titanic.test[, c("PassengerId", "Survived")]
write.csv(submitTest, "FirstSubmitTest.csv")

# 0.76077 on test set, which means even response doesn't work
