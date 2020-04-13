library(shiny)
library(miniUI)
library(twilio)
library(gmailr)
library(dplyr)

fromNumber <- "18646252951"

ui <- miniPage(
  miniTitleBar("Workout Creator"),
  miniTabstripPanel(
    miniTabPanel("Bodyweight", icon = icon("walking"),
                 miniContentPanel(
                   
                 )
    ),
    miniTabPanel("Kettlebell", icon = icon("weight-hanging"),
                 miniContentPanel(
                   fillRow(
                     fillCol(
                       radioButtons("kdifficulty", "Select Difficulty", choices = c("Beginner", "Intermediate", "Advanced"),
                                    selected = "Beginner"),
                       actionButton("kcreate", "Create Workout", icon = icon("magic"), width = "100%")
                     ),
                     fillCol(
                       sliderInput("kduration", "Select Exercise Duration", min = 5, max = 60, step = 5, value = 20)
                     )
                   )
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
  
  # Text It
  observeEvent(input$textMe,{
    keys <- read.csv("tokens.csv")
    Sys.setenv(TWILIO_SID = keys$sid[1])
    Sys.setenv(TWILIO_TOKEN = keys$token[1])
    tw_send_message(from = fromNumber, to = input$phone, body = "test")
  })
  
  # Email It
  observeEvent(input$emailMe, {

    email <- gm_mime(
      To = input$email,
      From = "wrfb28@gmail.com",
      Subject = "test",
      body = "test"
    )
    gm_send_message(email)
  })
}

shinyApp(ui, server)