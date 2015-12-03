# for titanic dataset1

titanic.fullset <- read.csv("clearTitanicData.csv")
# factorize some variables
titanic.fullset$Embarked <- factor(titanic.fullset$Embarked)
titanic.fullset$Pclass <- factor(titanic.fullset$Pclass)
titanic.fullset$Sex <- factor(titanic.fullset$Sex)
titanic.fullset$Title <- factor(titanic.fullset$Title)
titanic.fullset$Survived <- factor(titanic.fullset$Survived)

# split the combined data into training and test set
titanic.train.fristMethod <- titanic.fullset[1:891,-1]
titanic.test.fristMethod <- titanic.fullset[892:1309,-1]

# split the training set into local training and validation set
set.seed(11111)
train <- sample(c(TRUE, FALSE, TRUE, TRUE), nrow(titanic.train.fristMethod), replace = TRUE)
test <- !train

library(randomForest)

# bagging
bag.titanic.fristMethod.fit <- randomForest(Survived~., data = titanic.train.fristMethod[train, -c(2, 5, 7, 12)], mtry = 9, ntree = 500, importance = T)
bag.titanic.fristMethod.pred <- predict(bag.titanic.fristMethod.fit, titanic.train.fristMethod[test,-c(2, 5, 7, 12)], type = "class")
table(bag.titanic.fristMethod.pred, titanic.train.fristMethod[test,]$Survived)
mean(bag.titanic.fristMethod.pred!=titanic.train.fristMethod[test,]$Survived)
# miss-class = 0.2105263

# random forest
rf.titanic.fristMethod.fit <- randomForest(Survived~., data = titanic.train.fristMethod[train, -c(2, 5, 7, 12)], ntree = 500, importance = T)
rf.titanic.fristMethod.pred <- predict(rf.titanic.fristMethod.fit, titanic.train.fristMethod[test,-c(2, 5, 7, 12)], type = "class")
table(rf.titanic.fristMethod.pred, titanic.train.fristMethod[test,]$Survived)
mean(rf.titanic.fristMethod.pred!=titanic.train.fristMethod[test,]$Survived)
# miss-class = 0.1973684

# run random forest on full set
rf.titanic.fristMethod.fit.full <- randomForest(Survived~., data = titanic.train.fristMethod[, -c(2, 5, 7, 12)], ntree = 500, importance = T)
rf.titanic.fristMethod.pred.full <- predict(rf.titanic.fristMethod.fit, titanic.test.fristMethod[,-c(2, 5, 7, 12)], type = "class")
write.csv(data.frame(PassengerId=titanic.test.fristMethod$PassengerId, Survived=rf.titanic.fristMethod.pred.full), "rf on dataset1.csv", row.names = FALSE)
# kaggle score = 0.75598

# boosting
library(pROC)
library(adabag)
boost.titanic.firstMethod.fit <- boosting(Survived~., data = titanic.train.fristMethod[train, -c(2, 5, 7, 12)],boos = TRUE, mfinal = 100)
boost.titanic.firstMethod.pred <- predict(boost.titanic.firstMethod.fit, titanic.train.fristMethod[test,-c(2, 5, 7, 12)])
boost.titanic.firstMethod.pred$confusion
# that's not right
#              Observed Class
# Predicted Class   0   1
#               0 126   8
#               1   9  85

mean(boost.titanic.firstMethod.pred$class!=titanic.train.fristMethod[test,]$Survived)
# mis-class = 0.2324561
auc(roc(titanic.train.fristMethod[test,]$Survived, boost.titanic.firstMethod.pred$prob[,2], plot = TRUE))

# run boosting on full set
boost.titanic.firstMethod.fit.full <- boosting(Survived~., data = titanic.train.fristMethod[, -c(2, 5, 7, 12)],boos = TRUE, mfinal = 100)
boost.titanic.firstMethod.pred.full <- predict(boost.titanic.firstMethod.fit.full, titanic.test.fristMethod[,-c(2, 5, 7, 12)])

write.csv(data.frame(PassengerId = titanic.test.fristMethod$PassengerId, Survived = boost.titanic.firstMethod.pred.full$class), "boosting on first dataset.csv", row.names = FALSE)
# kaggle score =0.74641