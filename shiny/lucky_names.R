setwd("/Users/adamchaulk/Desktop/titanic/titanic/shiny")
wd <- getwd()


# The train and test data is stored in the ../input directory
train <- read.csv("train.csv")
train <- read.csv("test.csv")
# train <- (extract(train, Name, c("LastName", "Title", "FirstName"), "([^.]+), ([^.]+). (.*)"))

combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)

strsplit(combi$Name[1], split='[,.]')
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$Firstname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.(]')[[1]][3]})
# combi$Firstname <- sapply(combi$Firstname, FUN=function(x) {strsplit(x, split='[()]')[[1]][1]})
