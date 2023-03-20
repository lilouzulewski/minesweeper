#' @import shiny
library(shiny)
library(shinythemes)

#' Start the application
#' @export
startApp <- function() {
  ui <- fluidPage(
    titlePanel("Minesweeper"),
    theme = shinythemes::shinytheme("darkly"),
    helpText("Left click to reveal cell, right click to flag mine"),
    sidebarPanel(
      numericInput("nrow", label = "Number of rows (max 50)", value = 10),
      numericInput("ncol", label = "Number of columns (max 50)", value = 10),
      numericInput("nmines", label = "Number of mines", value = 14),
      actionButton("new_game", "New game"),
      width = 2
    ),
    mainPanel(
      minesweeperUI("minesweeper"),
      width=10
    )
  )

  server <- function(input, output, session) {
    # set limits of inputs to avoid crash
    reactiveNrow = reactiveVal()
    reactiveNcol= reactiveVal()
    reactiveNmines = reactiveVal()

    observeEvent(input$new_game, {
      enforceLimit <- function(session, inputId, value, min, max, default) {
        value = if (!is.integer(value)) {
          default
        } else if (value < min) {
          min
        } else if (value > max) {
          max
        } else {
          value
        }

        updateNumericInput(session, inputId, value = value)
        value
      }

      reactiveNrow(
        enforceLimit(session, "nrow", input$nrow, 1, 50, 10)
      )

      reactiveNcol(
        enforceLimit(session, "ncol", input$ncol, 1, 50, 10)
      )

      maxMines = reactiveNrow() * reactiveNcol() - 1
      reactiveNmines(
        enforceLimit(session, "nmines", input$nmines, 1, maxMines, 14)
      )
    })

    minesweeperServer(
      "minesweeper",
      reactive(input$new_game),
      reactiveNrow = reactiveNrow,
      reactiveNcol = reactiveNcol,
      reactiveNmines = reactiveNmines
    )
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}

