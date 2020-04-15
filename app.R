library(shiny)
library(miniUI)
library(shinycssloaders)
library(DT)
library(gmailr)
library(tableHTML)
library(dplyr)

df <- read.csv("exercises.csv")

# Function to Validate Email Address
isValidEmail <- function(x) {
  grepl("^\\w+@[a-zA-Z_]+?\\.[a-zA-Z]{2,3}$", as.character(x))
}
ui <- miniPage(
  miniTitleBar("Workout Creator"),
  miniTabstripPanel(
    miniTabPanel("Workout", icon = icon("dumbbell"),
                 miniContentPanel(
                    radioButtons("difficulty", "Select Difficulty", choices = c("Beginner", "Intermediate", "Advanced"),
                                 selected = "Beginner"),
                    radioButtons("equipment", "Select Equipment", choices = c("Bodyweight", "Bodyweight + Kettlebell", "Kettlebell"), selected = "Bodyweight"),
                    sliderInput("duration", "Select Exercise Duration (min)", min = 5, max = 60, step = 5, value = 20),
                    miniButtonBlock(actionButton("create", "Create Workout", icon = icon("magic"), width = "100%")),
                    withSpinner(tableOutput("table"), type = 7, color = "blue", size = 1)
                   
                 )
    ),
    miniTabPanel("share", icon = icon("share-square"),
                 miniContentPanel(
                   fillCol(
                     fillRow(flex = c(2,1),
                       textInput("email", "Email Address", width = "80%"),
                       verbatimTextOutput("emailCheck")
                       
                     ),
                     miniButtonBlock(actionButton("email", "Email Address:", icon = icon("magic"), width = "100%"))
                   )
                 )
    ),
    miniTabPanel("Info", icon = icon("question-circle"),
                 miniContentPanel(
                    h5("The workout table is all in seconds"),
                    h5("The difficulty is based off the following:"),
                    tags$ul(
                      tags$li("Beginner: 20 second duration sets"),
                      tags$li("Intermediate: 30 second duration sets"),
                      tags$li("Advanced: 45 second duration sets")
                    ),
                    h5("This will not be perfect! Please improvise if necessary. Kettlebell weight will be a huge factor in this as well."),
                    h5(HTML(paste0("<strong>", "Understanding the Workout:", "</strong>"))),
                    img(src="example.png"),
                    br(),
                    h5("Start workout from top to bottom. Do the 1st exercise for the time listed, rest for the SetRest duration. 
                       Repeat this for as many times listed under sets. Then rest for the ExRest duration. 
                       Move on to the next exercise and repeat."),
                    h5("So, I would do: "),
                    tags$ol(
                      tags$li("Air Squats for 20 seconds"),
                      tags$li("Rest for 10 seconds"),
                      tags$li("Air Squats for 20 seconds"),
                      tags$li("Rest for 10 seconds"),
                      tags$li("Air Squats for 20 seconds"),
                      tags$li("Rest for 10 seconds"),
                      tags$li("Air Squats for 20 seconds"),
                      tags$li("Rest for 55 seconds"),
                      br(),
                      tags$li("Inchworm for 20 seconds"),
                      tags$li("Rest for 10 seconds"),
                      tags$li("Inchworm for 20 seconds"),
                      tags$li("Rest for 10 seconds"),
                      tags$li("Inchworm for 20 seconds"),
                      tags$li("Rest for 10 seconds"),
                      tags$li("Inchworm for 20 seconds"),
                      tags$li("Rest for 55 seconds"),
                    ),
                    br(),
                    h5("Please send any comments or requests to: shiny.workoutcreator@gmail.com")
                 )
    )
  )
)

gm_auth_configure(path = "credentials/credentials.json")
gm_auth(email = "shiny.workoutcreator@gmail.com", cache = ".secrets")

server <- function(input, output, session) {
  
  # Get Number of Exercises Based Off Inputs
  numExercises <- reactive({
    if (input$difficulty == "Beginner"){
      exTot <- floor(input$duration/2.75)
      return(exTot)
    }
    else if(input$difficulty == "Intermediate") {
      exTot <- floor(input$duration/3.42)
      return(exTot)
    }
    else {
      exTot <- floor(input$duration/4.42)
      return(exTot)
    }
  })
  
  # Get Number of Seconds for Exercise Based Off Input
  numSeconds <- reactive({
    if(input$difficulty=="Beginner"){
      return(20)
    }
    else if(input$difficulty=="Intermediate"){
      return(30)
    }
    else {
      return(45)
    }
  })

  # Get Random Exercises in DF
  exercises <- eventReactive(input$create, {
    if(input$equipment == "Bodyweight"){
      df %>% filter(Equipment == "Bodyweight") %>% group_by(Focus) %>% sample_n(ceiling(numExercises()/3)) %>% ungroup() %>% 
        sample_n(numExercises()) %>% slice(sample(1:n())) %>% 
        transmute(Exercise = Exercise, Sets = 4, Time = numSeconds(), SetRest = 10, ExRest = 55)
    }
    else if (input$equipment == "Bodyweight + Kettlebell") {
      df %>% group_by(Focus) %>% sample_n(ceiling(numExercises()/3)) %>% ungroup() %>% 
        sample_n(numExercises()) %>% slice(sample(1:n())) %>% 
        transmute(Exercise = Exercise, Sets = 4, Time = numSeconds(), SetRest = 10, ExRest = 55)
    }
    else{
      df %>% filter(Equipment == "Kettlebell") %>% group_by(Focus) %>% sample_n(ceiling(numExercises()/3)) %>% ungroup() %>% 
        sample_n(numExercises()) %>% slice(sample(1:n())) %>% 
        transmute(Exercise = Exercise, Sets = 4, Time = numSeconds(), SetRest = 10, ExRest = 55)
    }
  })
  
  # KB Table Output
  output$table <- renderTable(exercises(), spacing = "xs", align = "l", digits = 0)
  
  # Check for Valid Email and Send
  observeEvent(input$emailMe, {
    output$emailCheck <- renderText({
      validate(
        need(isValidEmail(input$email), "Please enter a valid email address")
      )
      "Valid Email"
    })
    
    atchm <- tableHTML(exercises())
    html_bod <- paste0("<p> Your workout: </p>", atchm)
    gm_mime() %>%
      gm_to(input$email) %>%
      gm_from("shiny.workoutcreator@gmail.com") %>%
      gm_subject("Your Workout") %>%
      gm_html_body(html_bod) %>%
      gm_send_message()
  })
}

shinyApp(ui, server)