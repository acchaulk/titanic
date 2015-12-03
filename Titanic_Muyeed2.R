titanic <-read.csv("Titanic_AgeWithMeans.csv")
titanic_Survived <- titanic[, ]
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
train.x = cbind(Age, Fare, titanic_Survived.Sex, titanic_Survived.Pclass, titanic_Survived.Title, FamilySize)[train2, ]
test.x = cbind(Age, Fare, titanic_Survived.Sex, titanic_Survived.Pclass, titanic_Survived.Title, FamilySize)[-train2, ]
train.Survived2 = Survived[train2]
test.Survived2 = Survived[-train2]

set.seed(7) 
knn.pred = knn(train.x, test.x, train.Survived2, k = 30) 
mean(knn.pred != test.Survived2) 
table(knn.pred, test.Survived2)
#Mis-class rate = 18.0%

#QDA (doesn't work with more variables)
library(MASS)
set.seed(300)
qda.fit = qda(Survived ~ Age + Sex + Pclass + SibSp + Parch, data = titanic_Survived, subset = train)
qda.pred = predict(qda.fit, titanic.test)
mean(qda.pred$class != test.Survived)
#Mis-class rate = 18.0%

#Logistic Regression (gives a warning with more variables)
set.seed(200)
glm.fit = glm(Survived ~ Age + Sex + Pclass + FamilySize + Embarked, data = titanic_Survived, family = binomial, subset = train)
summary(glm.fit)
glm.probs = predict(glm.fit, titanic.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != test.Survived)
#Mis-class rate = 19.7% 
