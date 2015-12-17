library(shiny)
library(randomForest)

shinyServer(function(input, output) {
  # titanic.passenger <-
    
    rb <- reactive ({
      
#       data.frame(
#         Name = c("Integer", 
#                  "Decimal",
#                  "Range",
#                  "Custom Format",
#                  "Animation"),
#         Value = as.character(c(input$name, 
#                                input$title,
#                                input$sex,
#                                input$sibsp,
#                                input$parch)), 
#         stringsAsFactors=FALSE)
      
     titanic.passenger <- data.frame(Pclass = input$pclass,  Sex = input$sex, Age = input$age, 
                                SibSp = input$sibsp, Parch = input$parch, Fare = input$fare, 
                                Embarked = input$embarked, Title = input$title)
     
     titanic.passenger$FamilySize <- titanic.passenger$SibSp + titanic.passenger$Parch + 1
     
     titanic.passenger$FamilySize <- titanic.passenger$FamilySize
     
     titanic.passenger$FamilyID <- paste(as.character(titanic.passenger$FamilySize), input$last, sep = "")
     
     titanic.passenger$Survived <- NA
     
     # build the cforest model
     
     # Conditional Inference forest
     library(party)
    # titanic.im.train.cf <- cforest(Survived ~ ., data = titanic.im.train[, -c(2, 5, 7, 12)], controls=cforest_unbiased(ntree=2000, mtry=3))
     
     
     titanic.passenger$Age <- as.numeric(titanic.passenger$Age)
     
     titanic.passenger$Embarked <- factor(titanic.passenger$Embarked, levels = levels(titanic.im.train$Embarked))
    # levels(titanic.passenger$Embarked) <- levels(titanic.im.train$Embarked)
     
     titanic.passenger$Fare <- as.numeric(titanic.passenger$Fare)
     
     titanic.passenger$Parch <- as.integer(titanic.passenger$Parch)
     
     titanic.passenger$Pclass <- factor(titanic.passenger$Pclass, levels = levels(titanic.im.train$Pclass))
     #levels(titanic.passenger$Pclass) <- levels(titanic.im.train$Pclass)
     
     titanic.passenger$Sex <- factor(titanic.passenger$Sex, levels = levels(titanic.im.train$Sex))
    # levels(titanic.passenger$Sex) <- levels(titanic.im.train$Sex)
     
     titanic.passenger$SibSp <- as.integer(titanic.passenger$SibSp)
     
     titanic.passenger$Survived <- factor(titanic.passenger$Survived, levels = levels(titanic.im.train$Survived))
    # levels(titanic.passenger$Survived) <- levels(titanic.im.train$Survived)
     
     titanic.passenger$FamilySize <- as.integer(titanic.passenger$FamilySize)
     
     titanic.passenger$Title <- factor(titanic.passenger$Title, levels = levels(titanic.im.train$Title))
    # levels(titanic.passenger$Title) <- levels(titanic.im.train$Title)
     
     titanic.passenger$FamilyID <- factor(titanic.passenger$FamilyID, levels = levels(titanic.im.train$FamilyID))
     #levels(titanic.passenger$FamilyID) <- levels(titanic.im.train$FamilyID)
     
   #  testData <- rbind(titanic.im.test[1:10, -c(2, 5, 7, 12)], titanic.passenger)
     
    titanic.passenger.pred <- predict(titanic.im.train.cf, newdata=titanic.passenger, type = "response")
      
     return(titanic.passenger.pred)
    })
  output$text1 <- renderText({ 
    if(input$goButton) {
   # paste(input$first, input$title, input$sex, input$age, input$sibsp, input$parch, input$embarked, input$pclass,input$fare)
    
    #titanic.passenger <- data.frame(Pclass = input$pclass, Name = input$name, Sex = input$sex, Age = input$age, 
     #                               Sibsp = input$sibsp, Parch = input$parch, Ticket = 0, Fare = input$fare, Cabin = 0, Embarked = input$embarked)
     paste(rb())
    }
    })
  
  output$visA <- renderImage({
    # credit: https://www.kaggle.com/slothouber/titanic/traveling-alone-or-with-family
    
    train <- read.csv("train.csv")
    test  <- read.csv("test.csv")
    
    # We can inspect the train data. The results of this are printed in the log tab below
    summary(train)
    
    # Here we will plot the passenger survival by class
    train$Survived <- factor(train$Survived, levels=c(1,0))
    levels(train$Survived) <- c("Survived", "Died")
    train$Pclass <- as.factor(train$Pclass)
    levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")
    
    
    outfile <- tempfile(fileext='.png')
    png(outfile, width=800, height=600)
    mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
               color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
               off=c(0), cex.axis=1.4)
    dev.off()
    
#     # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  
})