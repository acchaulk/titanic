#both train and test set
titanic <- read.csv("fillAgeWithMeans.csv")

titanic[titanic$Survived==2, "Survived"] = NA

#factor all categorical variables
titanic$Embarked <- factor(titanic$Embarked)
titanic$Pclass <- factor(titanic$Pclass)
titanic$Sex <- factor(titanic$Sex)
titanic$Title <- factor(titanic$Title)
titanic$Survived <- factor(titanic$Survived)

titanic.test <- titanic[is.na(titanic$Survived), ]
titanic.train <- titanic[!is.na(titanic$Survived), ]

# the ratio of 0 to 1
sum(titanic.train$Survived==0)/sum(titanic.train$Survived==1)
## the ratio is 1.605263
summary(titanic.train$Survived)
#   0   1 
#  549 342 

set.seed(111)
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
# mis-class rate = 0.1659389
table(dt.titanic.pred, titanic.train.needed[test,]$Survived)
# dt.titanic.pred    0   1
#                0  129  23
#                1  15  62

# bagging
library(randomForest)

bag.titanic.fit <- randomForest(Survived~., data = titanic.train.needed, subset = train, mtry = 9, ntree = 1000, importance = T)
bag.pred <- predict(bag.titanic.fit, titanic.train.needed[test,], type = "class")
table(bag.pred, titanic.train.needed[test,]$Survived)
# bag.pred   0   1
#         0 128  25
#         1  16  60
mean(bag.pred!=titanic.train.needed[test,]$Survived)
# mis-class rate = 0.1790393
importance(bag.titanic.fit)
varImpPlot(bag.titanic.fit)


# radndomforest
library(pROC)
rf.titanic.fit <- randomForest(Survived~., data = titanic.train.needed, subset = train,mtry=3, ntree=500,importance = T)
rf.pred <- predict(rf.titanic.fit, titanic.train.needed[test, ], OOB=TRUE, type = "class")
auc(roc(titanic.train.needed[test, ]$Survived, rf.pred[, 2],levels = c("1", "0")))
table(rf.pred, titanic.train.needed[test,]$Survived)
# result with FamilySize and Title
# rf.pred   0   1
#       0 132  26
#       1  7  109

# result without familySize and Title
# rf.pred   0   1
#       0 127  27
#       1  12 109

# result with Title
# rf.pred   0   1
#       0 130  25
#       1   9 111

# result without sex
# rf.pred   0   1
#       0 131  28
#       1   8 108
mean(rf.pred!=titanic.train.needed[test,]$Survived)
# miss-class rate = 0.12, auc=0.9362 with FamilySize and Title
# miss-class rate = 0.1418182, auc=0.9277 without familySize and Title
# miss-class rate = 0.1236364, auc=0.9303 with title 
# miss-class rate = 0.1309091, auc=0.9287 without sex
importance(rf.titanic.fit)
varImpPlot(rf.titanic.fit)

plot(rf.titanic.fit)

write.csv(cbind(titanic.train.needed[test,], rf.pred), "internal test result.csv")

# boosting
library(adabag)
boost.titanic.fit <- boosting(Survived~., data = titanic.train.needed[train,],boos = TRUE, mfinal = 500)
boost.titanic.pred <- predict(boost.titanic.fit, titanic.train.needed[test,], newmfinal = 500)
table(boost.titanic.pred$class, titanic.train.needed[test,]$Survived)
mean(boost.titanic.pred$class!=titanic.train.needed[test,]$Survived)
auc(roc(titanic.train.needed[test,]$Survived, boost.titanic.pred$prob[,2]))
# test auc = 0.9445

# run boosting model on full training set
boost.titanic.fit.full <- boosting(Survived~., data = titanic.train.needed, boos = TRUE, mfinal = 100)
boost.titanic.pred.full <- predict(boost.titanic.fit.full, titanic.test[, -c(2, 5, 7,12)])
write.csv(data.frame(PassengerId=titanic.test$PassengerId, Survived=boost.titanic.pred.full$class), "boosting submit3.csv", row.names = FALSE)
# online test result = 0.722249

# run randfom forest model on full training set
rf.titanic.fit.full <- randomForest(Survived~., data = titanic.train.needed, ntree = 500, importance = T, nodesize = 1)
rf.titanic.fit.full$confusion
#    0   1 class.error
# 0 479  70   0.1275046
# 1  86 463   0.1566485
# run random forest model on test data
plot(rf.titanic.fit.full)
levels(titanic.test$Title) <- levels(titanic.train.needed$Title)
levels(titanic.test$Survived) <- levels(titanic.train.needed$Survived)



test.rf.preds <- predict(rf.titanic.fit.full, titanic.test[, -c(2,5,7,12)], type = "class")

write.csv(data.frame(PassengerId=titanic.test$PassengerId, Survived=test.rf.preds), "rf submit.csv", row.names = FALSE)

titanic.test[, "Survived"] = test.rf.preds

submitTest2 <- titanic.test[,c("PassengerId", "Survived")]
write.csv(submitTest2, "again FirstSubmitTest.csv")

# turns out with FamilySize and Title performs better