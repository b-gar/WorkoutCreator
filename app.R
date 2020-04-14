library(shiny)
library(miniUI)
library(shinycssloaders)
library(DT)
library(twilio)
library(gmailr)
library(dplyr)

kbdf <- read.csv("kbexercises.csv")
fromNumber <- "18646252951"

ui <- miniPage(
  miniTitleBar("Workout Creator"),
  miniTabstripPanel(
    miniTabPanel("Bodyweight", icon = icon("walking"),
                 miniContentPanel(
                   
                 )
    ),
    miniTabPanel("Kettlebell", icon = icon("weight-hanging"),
                 miniContentPanel(padding = 0,
                    radioButtons("kdifficulty", "Select Difficulty", choices = c("Beginner", "Intermediate", "Advanced"),
                                 selected = "Beginner"),
                    sliderInput("kduration", "Select Exercise Duration", min = 5, max = 60, step = 5, value = 20),
                    miniButtonBlock(actionButton("kcreate", "Create Workout", icon = icon("magic"), width = "100%")),
                    withSpinner(tableOutput("kbtable"), type = 7, color = "blue", size = 1)
                   
                 )
    ),
    miniTabPanel("share", icon = icon("share-square"),
                 miniContentPanel(
                   fillCol(
                     fillRow(
                       textInput("phone", "Phone Number", width = "80%"),
                       textInput("email", "Email Address", width = "80%")
                       
                     ),
                     fillRow(
                       actionButton("textMe", "Send"),
                       actionButton("emailMe", "Send")
                     )
                   )
                 )
    ),
    miniTabPanel("Info", icon = icon("question-circle"),
                 miniContentPanel(
                    
                 )
    )
  )
)

gm_auth_configure(path = "credentials/credentials.json")
gm_auth(email = "shiny.workoutcreator@gmail.com", cache = ".secrets")

server <- function(input, output, session) {
  
  # Get Number of Exercises Based Off Inputs
  numExercises <- reactive({
    if (input$kdifficulty == "Beginner"){
      exTot <- floor(input$kduration/2.75)
      return(exTot)
    }
    else if(input$kdifficulty == "Intermediate") {
      exTot <- floor(input$kduration/3.42)
      return(exTot)
    }
    else {
      exTot <- floor(input$kduration/4.42)
      return(exTot)
    }
  })
  
  # Get Number of Seconds for Exercise Based Off Input
  numSeconds <- reactive({
    if(input$kdifficulty=="Beginner"){
      return(20)
    }
    else if(input$kdifficulty=="Intermediate"){
      return(30)
    }
    else {
      return(45)
    }
  })

  # Get Random KB Exercises in DF
  kbexercises <- eventReactive(input$kcreate, {
    kbdf %>% group_by(Focus) %>% sample_n(ceiling(numExercises()/3)) %>% ungroup() %>% sample_n(numExercises()) %>% 
      slice(sample(1:n())) %>% transmute(Exercise = Exercise, Sets = 4, Time = numSeconds(), RestBetweenSets = 10, RestBetweenExercises = 55)
  })
  
  # KB Table Output
  output$kbtable <- renderTable(kbexercises(), width = "100%", digits = 0)
  
  # Text It
  observeEvent(input$textMe,{
    keys <- read.csv("tokens.csv")
    Sys.setenv(TWILIO_SID = keys$sid[1])
    Sys.setenv(TWILIO_TOKEN = keys$token[1])
    tw_send_message(from = fromNumber, to = input$phone, body = "test")
  })
  
  # Email It
  observeEvent(input$emailMe, {

    email <- gm_mime() %>%
      gm_to(input$email) %>%
      gm_from("shiny.workoutcreator@gmail.com") %>%
      gm_text_body("See attached workout") %>%
      gm_attach_file()
    gm_send_message(email)
  })
}

shinyApp(ui, server)