titanic <- read.csv("fillAgeWithMeans.csv")

titanic[titanic$Survived==2, "Survived"] = NA

#factor all categorical variables
titanic$Embarked <- factor(titanic$Embarked)
titanic$Pclass <- factor(titanic$Pclass)
titanic$Sex <- factor(titanic$Sex)
titanic$Title <- factor(titanic$Title)
titanic$Survived <- factor(titanic$Survived)

titanic$Name <- as.character(titanic$Name)

#strsplit(titanic$Name[1], split='[,.]')[[1]][2]

titanic.surname <- sapply(titanic$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

titanic$FamilyID <- paste(as.character(titanic$FamilySize), titanic.surname, sep = "")

titanic$FamilyID[titanic$FamilySize <= 2] <- "small"

#table(titanic$FamilyID)

famID <- data.frame(table(titanic$FamilyID))
famID <- famID[famID$Freq <= 2,]

titanic$FamilyID[titanic$FamilyID %in% famID$Var1] <- "small"
titanic$FamilyID <- factor(titanic$FamilyID)

#table(titanic$FamilyID)

titanic.im.train <- titanic[1:891,]
titanic.im.test <- titanic[892:1309,]

library(party)
titanic.im.train.cf <- cforest(Survived ~ ., data = titanic.im.train[, -c(2, 5, 7, 12)], controls=cforest_unbiased(ntree=2000, mtry=3))