library(shiny)
library(miniUI)
library(twilio)
library(dplyr)

keys <- read.csv("tokens.csv")
Sys.setenv(TWILIO_SID = keys$sid[1])
Sys.setenv(TWILIO_TOKEN = keys$token[1])
fromNumber <- "18646252951"

ui <- miniPage(
  miniTitleBar("Workout Creator"),
  miniTabstripPanel(
    miniTabPanel("Home", icon = icon("home"),
                 miniContentPanel(
                    fillRow(
                      radioButtons("difficulty", "Select Difficulty", choices = c("Beginner", "Intermediate", "Advanced")),
                      sliderInput("time", "Select Exercise Duration", min = 5, max = 60, step = 5, value = 20)
                    )
                 )
    ),
    miniTabPanel("Kettlebell", icon = icon("weight-hanging"),
                 miniContentPanel(
                   plotOutput("cars", height = "100%")
                 )
    ),
    miniTabPanel("Info", icon = icon("question-circle"),
                 miniContentPanel(
                    
                 )
    )
  )
)

server <- function(input, output, session) {
  
  # Get Workout in Correct Format
  
  
  #tw_send_message(from = fromNumber, to = input$phone, body = "")

}

runGadget(shinyApp(ui, server))