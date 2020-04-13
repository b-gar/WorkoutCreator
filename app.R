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
                    fillRow(
                      fillCol(
                        radioButtons("difficulty", "Select Difficulty", choices = c("Beginner", "Intermediate", "Advanced"), 
                                     selected = "Beginner"),
                        textInput("phone", "Phone Number", width = "80%"),
                        actionButton("textMe", "Send")
                        
                      ),
                      fillCol(
                        sliderInput("time", "Select Exercise Duration", min = 5, max = 60, step = 5, value = 20),
                        textInput("email", "Email Address", width = "80%"),
                        actionButton("emailMe", "Send")
                      )
                    )
                 )
    ),
    miniTabPanel("Kettlebell", icon = icon("weight-hanging"),
                 miniContentPanel(
                   
                 )
    ),
    miniTabPanel("Info", icon = icon("question-circle"),
                 miniContentPanel(
                    
                 )
    )
  )
)

gm_auth_configure(path = "credentials/credentials.json")
gm_auth(email = "wrfb28@gmail.com", cache = ".secrets")

server <- function(input, output, session) {
  
  # Get Workout in Correct Format
  
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