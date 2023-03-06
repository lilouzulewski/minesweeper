library(shiny)

cellUI <- function(i, j, game, state) {
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

gridUI <- function(inputId, game, state) {
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

    $(document).on("click", ".minesweeper-grid.ongoing .hidden-cell", function(event) {
      const target = $(event.target)
      const id = getId(target)

      Shiny.setInputValue(id, {
        action: "checkCell",
        ...getCoords(target),
      })
    })

    $(document).on("contextmenu", ".minesweeper-grid.ongoing .hidden-cell", function(event) {
      const target = $(event.target)
      const id = getId(target)

      Shiny.setInputValue(id, {
        action: "flagCell",
        ...getCoords(target),
      })
    })

    $(document).on("contextmenu", ".minesweeper-grid.ongoing .flagged-cell", function(event) {
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
        cells = append(cells, list(cellUI(i, j, game, state)))
      }
    }

    tagList(
      singleton(tags$head(tags$script(jscode))),
      tags$svg(
        `data-input-id` = inputId,
        class = sprintf("minesweeper-grid %s", status),
        width = "100%",
        viewBox = sprintf("1 1 %d %d", game$ncol, game$nrow),
        tagList(cells)
      )
    )
  }
}

minesweeperUI <- function(id) {
  ns = NS(id)
  tagList(
    uiOutput(ns("play_grid"))
  )
}

minesweeperServer <- function(
  id,
  reactiveStartGame,
  reactiveNrow,
  reactiveNcol,
  reactiveNmines
) {
  moduleServer(
    id,
    function(input, output, session) {
      reactiveState = reactiveVal()

      reactiveGame = eventReactive(reactiveStartGame(), {
        mines = createMinesGrid(
          nrow=reactiveNrow(),
          ncol=reactiveNcol(),
          nmines=reactiveNmines()
        )

        game = createGame(mines)
        reactiveState(createInitialState(game))

        game
      })

      observeEvent(input$play_grid, {
        game = reactiveGame()
        state = reactiveState()

        event = input$play_grid
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

      output$play_grid <- renderUI({
        ns = session$ns

        game = reactiveGame()
        state = reactiveState()

        gridUI(ns("play_grid"), game, state)
      })
    }
  )
}
