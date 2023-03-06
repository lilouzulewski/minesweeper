library(shiny)

startApp <- function() {
  addResourcePath("assets", system.file('assets', package="Minesweeper"))

  ui <- fluidPage(
    titlePanel("Minesweeper"),
    helpText("Sweep them all"),
    sidebarPanel(
      numericInput("nrow", label = "Number of row", value = 10),
      numericInput("ncol", label = "Number of column", value = 10),
      numericInput("nmines", label = "Number of mines", value = 14),
      actionButton("new_game", "New game"),
      width = 2
    ),
    mainPanel(
      minesweeperUI("minesweeper"),
      width=8
    )
  )

  server <- function(input, output) {
    minesweeperServer(
      "minesweeper",
      reactive(input$new_game),
      reactiveNrow = reactive(input$nrow),
      reactiveNcol = reactive(input$ncol),
      reactiveNmines = reactive(input$nmines)
    )
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}

