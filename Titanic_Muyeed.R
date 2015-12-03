titanic <-read.csv("UpdatedTitanicData.csv")
titanic_Survived <- titanic[c(1:891), -c(1)]
summary(titanic_Survived)

#factor all categorical variables
titanic_Survived$Embarked <- factor(titanic_Survived$Embarked)
titanic_Survived$Pclass <- factor(titanic_Survived$Pclass)
titanic_Survived$Sex <- factor(titanic_Survived$Sex)
titanic_Survived$Title <- factor(titanic_Survived$Title)
titanic_Survived$Survived <- factor(titanic_Survived$Survived)
attach(titanic_Survived)
fix(titanic_Survived)

#selecting 668 out of 891 rows, essentiallly 75%
set.seed(777)
train = sample(dim(titanic_Survived)[1], 668) 
#test = !train
titanic.train = titanic_Survived[train, ]
titanic.test = titanic_Survived[-train, ]
train.Survived = Survived[train]
test.Survived = Survived[-train]

#Standardize (mainly for knn)
titanic.Stand = scale(titanic_Survived[, -c(2, 3, 5, 7, 8, 9, 11, 12, 14)]) #standardizing only numerical variables
var(titanic_Survived[, 1])
var(titanic.Stand[, 1])
titanic.StandObj = as.data.frame(titanic.Stand)
titanic.StandObj = data.frame(titanic.StandObj, titanic_Survived$Sex, titanic_Survived$Pclass, titanic_Survived$Title)
summary(titanic.StandObj)
attach(titanic.StandObj)
set.seed(778)
train2 = sample(dim(titanic.StandObj)[1], 668)

#knn
library(class)
train.x = cbind(Age, Fare, titanic_Survived.Sex, titanic_Survived.Pclass, FamilySize)[train2, ]
test.x = cbind(Age, Fare, titanic_Survived.Sex, titanic_Survived.Pclass, FamilySize)[-train2, ]
train.Survived2 = Survived[train2]
test.Survived2 = Survived[-train2]

set.seed(7) 
knn.pred = knn(train.x, test.x, train.Survived2, k = 30) 
mean(knn.pred != test.Survived2) 
table(knn.pred, test.Survived2)
#Mis-class rate = 18%

#QDA (doesn't work with more variables)
library(MASS)
set.seed(300)
qda.fit = qda(Survived ~ Age + Sex + Pclass + SibSp + Parch, data = titanic_Survived, subset = train)
qda.pred = predict(qda.fit, titanic.test)
mean(qda.pred$class != test.Survived)
#Mis-class rate = 17.5%

#Logistic Regression (gives a warning with more variables)
set.seed(200)
glm.fit = glm(Survived ~ Age + Sex + Pclass + Title + FamilySize + Embarked, data = titanic_Survived, family = binomial, subset = train)
summary(glm.fit)
glm.probs = predict(glm.fit, titanic.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != test.Survived)
#Mis-class rate = 16.6% 

#Decision Trees
library(tree)
set.seed(60)
tree.titanic = tree(Survived ~ Age + Embarked + Fare + Pclass + Sex + FamilySize + Title, data = titanic_Survived, subset = train)
plot(tree.titanic)
text(tree.titanic, pretty = 0)
summary(tree.titanic)
tree.pred = predict(tree.titanic, titanic.test, type = "class")
mean(tree.pred != test.Survived)
table(tree.pred, test.Survived)
#Mis-class rate = 17.0%

#Pruning Decision Tree
set.seed(90)
cv.titanicDT = cv.tree(tree.titanic, FUN=prune.misclass)
cv.titanicDT
#Best Size = 4
pruned.titanic = prune.misclass(tree.titanic, best = 4)
plot(pruned.titanic)
text(pruned.titanic, pretty = 0)
pruned.pred = predict(pruned.titanic, titanic.test, type = "class")
table(pruned.pred, test.Survived)
mean(pruned.pred != test.Survived)
#Mis-class rate = 17.0%

#Decision Tree using rpart
library(rattle)
library(rpart.plot)
#Adjusting control parameters - minisplit and cp - leads to different results
set.seed(150)
r.tree.titanic = rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = titanic_Survived, subset = train, method = "class", control = rpart.control(minsplit = 10, cp = 0.02, xval = 10))
plot(r.tree.titanic)
text(r.tree.titanic, pretty = 0)
summary(r.tree.titanic)
r.tree.pred = predict(r.tree.titanic, titanic.test, type = "class")
mean(r.tree.pred != test.Survived)
table(r.tree.pred, test.Survived)
#Mis-class rate = 16.6%

#Bagging
library(randomForest)
set.seed(75)
bag.titanic = randomForest(Survived~.-(Cabin + Name + PassengerId + Ticket), data = titanic_Survived, subset = train, mtry = 9, ntree = 500, importance = T)
bag.pred = predict(bag.titanic, titanic.test, type = "class")
table(bag.pred, test.Survived)
mean(bag.pred != test.Survived)
importance(bag.titanic)
summary(bag.titanic)
plot(bag.titanic)
#Mis-class rate = 15.7%

#RandomForest
set.seed(50)
rf.titanic = randomForest(Survived~.-(Cabin + Name + PassengerId + Ticket), data = titanic_Survived, subset = train, ntree = 500, importance = T)
rf.pred = predict(rf.titanic, titanic.test, type = "class")
table(rf.pred, test.Survived)
mean(rf.pred != test.Survived)
importance(rf.titanic)
plot(rf.titanic)
#Mis-class rate = 14.3%

#Boosting 
# library(gbm)
# boost.titanic = gbm(Survived~ Age + Embarked + Fare + Pclass + Sex + FamilySize + Title + SibSp + Parch, data=titanic_Survived[train, ], distribution="bernoulli", n.trees=5000, interaction.depth=1, shrinkage=0.1)
# boost.pred = predict(boost.titanic, newdata=titanic.test, n.trees=5000)
# table(boost.pred, test.Survived)
# mean(boost.pred != test.Survived)
##Mis-class rate

library(adabag)
boost.titanic.fit <- boosting(Survived~ Age + Embarked + Fare + Pclass + Sex + FamilySize + Title + SibSp + Parch, data = titanic.train, boos = TRUE, mfinal = 100)
boost.titanic.pred <- predict(boost.titanic.fit, titanic.test, newmfinal = 100)
table(boost.titanic.pred$class, test.Survived)
mean(boost.titanic.pred$class!=test.Survived)
##Mis-class rate = 16.1%