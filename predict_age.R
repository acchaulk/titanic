titanic <-read.csv("titanic.csv")

titanic.train.origin <- read.csv("train.csv")
titanic.test.origin <- read.csv("test.csv")
titanic.test.origin[, "Survived"] = NA

titanic.origin <- rbind(titanic.train.origin, titanic.test.origin)


#transform num to factor
titanic$Embarked <- factor(titanic$Embarked)
titanic$Pclass <- factor(titanic$Pclass)
titanic$Sex <- factor(titanic$Sex)
#{"Mr": 1, "Miss": 2, "Mrs": 3, "Master": 4, "Dr": 5, "Rev": 6, "Sir": 7, 
#"Col": 8, "Lady": 9, "Mlle": 10, "Ms": 11}
titanic$Title <- factor(titanic$Title) 
titanic$Survived <- factor(titanic$Survived)

#get data to predict Age, and drop Name, Ticket, Cabin, Survived
titanic_age <- titanic[!is.na(titanic$Age),]
titanic_age <- titanic_age[, -c(2, 5,11,12)] 

set.seed(1)
train=sample(c(TRUE,FALSE),nrow(titanic_age),rep=TRUE)
test=(!train)

titanic_age.train <- titanic_age[train,]
titanic_age.test <- titanic_age[test,]

#linear regression, it doesn't work with FamilySize and Title, so I remove it
lm.fit <- lm(Age~., data = titanic_age.train[, -c(5,9,10)])
summary(lm.fit)
lm.pred <- predict(lm.fit, titanic_age.test[, -c(5,9,10)])

mean((lm.pred-titanic_age.test$Age)^2) #test MSE = 144.2868

library(glmnet)
x <- model.matrix(Age~., titanic_age)
y <- titanic_age$Age

train.x <- model.matrix(Age~.,titanic_age.train[, -5])
train.y <- titanic_age.train$Age

test.x <- model.matrix(Age~.,titanic_age.test[, -5])
test.y <- titanic_age.test$Age

#ridge
set.seed(1)
cv.out <- cv.glmnet(train.x, train.y, alpha = 0)
plot(cv.out)
bestlam.ridge <- cv.out$lambda.min
bestlam.ridge
ridge.mod <- glmnet(train.x, train.y, alpha = 0, lambda = bestlam.ridge)
ridge.pred <- predict(ridge.mod, s = bestlam.ridge, newx = test.x)
mean((ridge.pred-test.y)^2) # test MSE = 114.349

#lasso
set.seed(1)
cv.out <- cv.glmnet(train.x, train.y, alpha = 1)
plot(cv.out)
bestlam.lasso <- cv.out$lambda.min
bestlam.lasso
lasso.mod <- glmnet(train.x, train.y, alpha = 1, lambda = bestlam.lasso)
lasso.pred <- predict(lasso.mod, s = bestlam.lasso, newx = test.x)
mean((lasso.pred-test.y)^2) # test MSE = 115.0818

#lasso regression on entire dataset with bestlam.lasso 
out <- glmnet(x, y, alpha = 1)
coef <- predict(out, type = "coefficients", s = bestlam.lasso)[1:22,]
coef
sum(coef==0)
names(coef[coef==0]) # "(Intercept)" "Parch"       "Sex1"

#pcr
#library(pls)
#pcr.fit <- pcr(Age~., data = titanic_age.train[, -5], scale = TRUE, validation = "CV")

#decision tree
library("rpart")
library("rpart.plot")

predicted_age <- rpart(Age ~ ., data=titanic_age.train[, -5], method="anova")
summary(predicted_age)
str(titanic_age.train)
result <- predict(predicted_age, titanic_age.test[,-5])
mean((result-titanic_age.test$Age)^2) # test MSE = 106.1454

summary(result)
plot(result)
hist(result)
sum(result<0)

#randomforest
set.seed(2)
rf_age <- randomForest(Age~., data = titanic_age.train[,-5], mtry=3, importance = TRUE, type = "regression")

rf_age_pred <- predict(rf_age, titanic_age.test[,-5])
mean((rf_age_pred-titanic_age.test$Age)^2) # t=test MSE = 102.4183

# perform randomforest on titanic_age, and predict Age for the rest data
titanic_no_age <- titanic[is.na(titanic["Age"]),]
titanic_no_age <- titanic_no_age[, -c(2, 5,11,12)]

set.seed(2)
rf_age <- randomForest(Age~., data = titanic_age[,-5], mtry=3, importance = TRUE, type = "regression")
titanic$Age[is.na(titanic$Age)] <- predict(rf_age, titanic_no_age[,-5])

titanic$Age = round(titanic$Age)
sum(is.na(titanic$Age))

write.csv(titanic, file = "clearTitanicData.csv")

#SVM regression
library(e1071)
set.seed(1)
svm.fit <- svm(Age~., titanic_age.train[,-5])
svm.pred <- predict(svm.fit, titanic_age.test[,-5])
mean((svm.pred-titanic_age.test$Age)^2) #test MSE = 114.7761
