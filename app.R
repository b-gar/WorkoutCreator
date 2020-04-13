library(shiny)
library(miniUI)
library(twilio)
library(dplyr)

keys <- read.csv("tokens.csv")
Sys.setenv(TWILIO_SID = keys$sid[1])
Sys.setenv(TWILIO_TOKEN = keys$token[1])

ui <- miniPage(
  gadgetTitleBar("Shiny gadget example"),
  miniTabstripPanel(
    miniTabPanel("Home", icon = icon("sliders"),
                 miniContentPanel(
                    
                 )
    ),
    miniTabPanel("Visualize", icon = icon("area-chart"),
                 miniContentPanel(
                   plotOutput("cars", height = "100%")
                 )
    ),
    miniTabPanel("Map", icon = icon("map-o"),
                 miniContentPanel(padding = 0,
                                  leafletOutput("map", height = "100%")
                 ),
                 miniButtonBlock(
                   actionButton("resetMap", "Reset")
                 )
    ),
    miniTabPanel("Data", icon = icon("table"),
                 miniContentPanel(
                   DT::dataTableOutput("table")
                 )
    )
  )
)

server <- function(input, output, session) {
  output$cars <- renderPlot({
    require(ggplot2)
    ggplot(cars, aes(speed, dist)) + geom_point()
  })
  
  output$map <- renderLeaflet({
    force(input$resetMap)
    
    leaflet(quakes, height = "100%") %>% addTiles() %>%
      addMarkers(lng = ~long, lat = ~lat)
  })
  
  output$table <- DT::renderDataTable({
    diamonds
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}

runGadget(shinyApp(ui, server))