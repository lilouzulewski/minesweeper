library(shiny)

startApp <- function() {
  ui <- fluidPage(
      "Hello world"
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}

