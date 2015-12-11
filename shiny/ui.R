library(shiny)

shinyUI(navbarPage("Welcome to the Titanic!",
   tabPanel("Will you survive?",
            titlePanel("Enter your information"),
            sidebarLayout(
              sidebarPanel(
                
                fluidRow(
                  column(6, 
                    textInput("text", label = h4("Name"), 
                              value = "")),
                  
                  column(6, 
                    selectInput("title", label = h4("Title"), 
                                choices = list("Mr" = 1, "Miss" = 2, "Mrs" = 3, "Master" = 4, 
                                              "Dr" = 5, "Rev" = 6, "Major" = 7, "Col" = 8, "Mlle" = 9, 
                                              "Mme" = 10, "Don" = 11, "Lady" = 12, "Countess" = 13,
                                              "Jonkheer" = 14, "Sir" = 15,"Capt" = 16, "Ms" = 17),
                                selected = 1))),
                
                fluidRow(
                  column(6, 
                    radioButtons("sex", label = h4("Gender"), 
                                choices = list("Male" = 1, "Female" = 2),
                                selected = 1)),
                  column(6, 
                    numericInput("age", 
                                label = h4("Age"), 
                                value = 1))),
            
                fluidRow(
                  column(6, 
                    numericInput("sibsp", 
                                 label = h4("Number of Siblings and Spouses Aboard"), 
                                 value = 0)),
                
                  column(6, 
                    numericInput("parch", 
                                 label = h4("Number of Parents and Children Aboard"), 
                                 value = 0))),
                
                fluidRow(
                  column(6, 
                    radioButtons("embarked", label = h4("Point of Embarkation"), 
                                 choices = list("Cherbourg" = 1, "Queenstown" = 2, "Southampton" = 3),
                                 selected = 1)),
      
                  column(6, 
                    radioButtons("pclass", label = h4("Passenger Class"), 
                                 choices = list("Upper" = 1, "Middle" = 2, "Lower" = 3),
                                 selected = 1))),
                
                sliderInput("fare", label = h4("Passenger Fare"), 
                            min = 0, max = 513, value = 50),
                
                submitButton("Submit!")
                
     
              ),
              mainPanel("")
            )
    ),
   navbarMenu("Visualizations",
    tabPanel("Vis A",
      mainPanel("Vis A")
    ),
    tabPanel("Vis B",
       mainPanel("Vis B")
    )
   )
))