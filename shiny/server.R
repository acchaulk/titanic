library(shiny)
library(randomForest)

shinyServer(function(input, output) {
  output$text1 <- renderText({ 
    paste(input$name, input$title, input$sex, input$age, input$sibsp, input$parch, input$embarked, input$pclass,input$fare)
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