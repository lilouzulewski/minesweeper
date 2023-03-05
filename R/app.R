library(shiny)
library(shinyjs)

cellUi <- function(i, j, game, state) {
  # helper function to create image tag
  tag_image <- function(name, class="") {
    tags$image(
      href=sprintf("assets/svg/%s.svg", name),
      class=class,
      x=j,
      y=i,
      width=1,
      height=1
    )
  }

  status = if (state$checked[i, j]) {
    tag_image(
      sprintf("checked_%d", game$nearby_mines[i, j])
    )
  } else if (state$flagged[i, j]) {
    tag_image("flag", class="flagged-cell")
  } else {
    tag_image("hidden", class="hidden-cell")
  }
}

inputGameGridUi <- function(inputId, game, state) {
  jscode <- r"(
    // Disable menu display on right click on the grid
    $(document).on("contextmenu", ".minesweeper-grid", function(event) {
      event.preventDefault()
    })

    function getCoords(el) {
      return {
        i: parseInt(el.attr("y"), 10),
        j: parseInt(el.attr("x"), 10),
      }
    }

    function getId(el) {
      return el.closest(".minesweeper-grid").data("input-id")
    }

    $(document).on("click", ".minesweeper-grid .hidden-cell", function(event) {
      const target = $(event.target)
      const id = getId(target)

      Shiny.setInputValue(id, {
        action: "checkCell",
        ...getCoords(target),
      })
    })

    $(document).on("contextmenu", ".minesweeper-grid .hidden-cell", function(event) {
      const target = $(event.target)
      const id = getId(target)

      Shiny.setInputValue(id, {
        action: "flagCell",
        ...getCoords(target),
      })
    })

    $(document).on("contextmenu", ".minesweeper-grid .flagged-cell", function(event) {
      const target = $(event.target)
      const id = getId(target)

      Shiny.setInputValue(id, {
        action: "unflagCell",
        ...getCoords(target),
      })
    })
  )"

  status = gameStatus(game, state)

  if (status == "victory") {
    p("victory")
  } else if (status == "defeat") {
    p("defeat")
  } else if (status == "ongoing") {
    cells = list()
    for (i in 1:game$nrow) {
      for (j in 1:game$ncol) {
        cells = append(cells, list(cellUi(i, j, game, state)))
      }
    }

    tagList(
      singleton(tags$head(tags$script(jscode))),
      tags$svg(`data-input-id` = inputId,
        class = "minesweeper-grid",
        width = "100%",
        viewBox = sprintf("0 0 %d %d", game$ncol, game$nrow),
        tagList(cells)
      )
    )
  }
}

startApp <- function() {
  addResourcePath("assets", system.file('assets', package="Minesweeper"))

  ui <- fluidPage(
    titlePanel("Minesweeper"),
    helpText("Sweep them all"),
    sidebarPanel(
      numericInput("nrow", label = "Number of row", value = 10),
      numericInput("ncol", label = "Number of column", value = 10),
      numericInput("nmines", label = "Number of mines", value = 27),
      actionButton("new_game", "New game"),
      width = 2
    ),
    mainPanel(
      uiOutput("minesweeper_grid_output"),
      width=12
    )
  )

  server <- function(input, output) {
    reactiveState = reactiveVal()
    reactiveGame <- eventReactive(input$new_game, {
      r"(
      mines = createMinesGrid(
        nrow=input$nrow,
        ncol=input$ncol,
        nmines=input$nmines
      )
      )"

      mines = matrix(nrow=6, ncol=12, byrow=TRUE, data=as.logical(c(
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
        0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1,
        0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0,
        0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0,
        0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0
      )))

      game = createGame(mines)
      state = createInitialState(game)

      reactiveState(state)

      game
    })

    observeEvent(input$minesweeper_grid_input, {
      game = reactiveGame()
      state = reactiveState()

      event = input$minesweeper_grid_input
      action = event$action
      i = event$i
      j = event$j

      newState = if (action == "checkCell") {
        checkCell(game, state, i, j)
      } else if (action == "flagCell") {
        flagCell(state, i, j)
      } else if (action == "unflagCell") {
        unflagCell(state, i, j)
      }

      reactiveState(newState)
    })

    output$minesweeper_grid_output <- renderUI({
      game = reactiveGame()
      state = reactiveState()

      inputGameGridUi("minesweeper_grid_input", game, state)
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}

