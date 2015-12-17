library(shiny)

shinyUI(navbarPage(
  "Welcome to the Titanic!",
  tabPanel(
    "Will you survive?",
    titlePanel("Enter your information"),
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(4,
                 textInput(
                   "first", label = h4("First Name"),
                   value = ""
                 )),
          
          column(4,
                 textInput(
                   "last", label = h4("Last Name"),
                   value = ""
                 )),
          
          column(4,
                 selectInput(
                   "title", label = h4("Title"),
                   choices = list(
                     "Mr" = 1, "Miss" = 2, "Mrs" = 3, "Master" = 4,
                     "Dr" = 5, "Rev" = 6, "Major" = 7, "Col" = 7, "Mlle" = 8,
                     "Mme" = 8, "Don" = 9, "Lady" = 10, "Countess" = 10,
                     "Jonkheer" = 10, "Sir" = 9,"Capt" = 7, "Ms" = 2
                   ),
                   selected = 1
                 ))
        ),
        
        fluidRow(column(
          6,
          radioButtons(
            "sex", label = h4("Gender"),
            choices = list("Male" = 0, "Female" = 1),
            selected = 0
          )
        ),
        column(
          6,
          numericInput(
            "age",
            label = h4("Age"),
            min = 1,
            value = 1
          )
        )),
        
        fluidRow(column(
          6,
          numericInput(
            "sibsp",
            label = h4("Number of Siblings and Spouses Aboard"),
            min = 0,
            value = 0
          )
        ),
        
        column(
          6,
          numericInput(
            "parch",
            label = h4("Number of Parents and Children Aboard"),
            min = 0,
            value = 0
          )
        )),
        
        fluidRow(column(
          6,
          radioButtons(
            "embarked", label = h4("Point of Embarkation"),
            choices = list(
              "Cherbourg" = 1, "Queenstown" = 2, "Southampton" = 0
            ),
            selected = 1
          )
        ),
        
        column(
          6,
          radioButtons(
            "pclass", label = h4("Passenger Class"),
            choices = list(
              "Upper" = 1, "Middle" = 2, "Lower" = 3
            ),
            selected = 1
          )
        )),
        
        sliderInput(
          "fare", label = h4("Passenger Fare"),
          min = 0, max = 513, value = 50
        ),
        actionButton("submit", "Submit")
      ),
      mainPanel(h1(textOutput("survivedText")),
                imageOutput("survivalImage"))
    )
  ),
  navbarMenu(
    "Visualizations",
    tabPanel("Passenger Survival by Class",
             mainPanel(plotOutput("survivalByClass"))),
    tabPanel("Passenger Survival by Gender",
             mainPanel(plotOutput("survivalByGender"))),
    tabPanel("Passenger Survival by Age",
             mainPanel(plotOutput("survivalByAge"))),
    tabPanel("Passenger Survival by Embarkation",
             mainPanel(plotOutput(
               "survivalByEmbarkation"
             ))),
    tabPanel("Lucky Families",
             mainPanel(
               fluidRow(column(6, h4("Lucky Families")),
                        column(6, h4("Unlucky Families"))),
               fluidRow(column(6, tableOutput("luckyFamilies")),
                        column(6, tableOutput("unluckyFamilies")))
             ))
  )
))