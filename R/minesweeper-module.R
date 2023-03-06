library(shiny)

# helper function to create cells
cellUI <- function(i, j, name, class="") {
  tags$image(
    href = sprintf("assets/svg/%s.svg", name),
    class = class,
    x = j,
    y = i,
    width = 1,
    height = 1
  )
}

hiddenCellUI <- function(i, j) {
  cellUI(i, j, "hidden", class="hidden-cell")
}

flagCellUI <- function(i, j) {
  cellUI(i, j, "flag", class="flagged-cell")
}

wrongFlagCellUI <- function(i, j) {
  tagList(
    cellUI(i, j, "flag"),
    tags$rect(x=j, y=i, width=1, height=1, fill="rgba(255, 0, 0, 0.2)")
  )
}

checkedCellUI <- function(i, j, nearby_mines) {
  cellUI(i, j, sprintf("checked_%d", nearby_mines))
}

mineCellUI <- function(i, j) {
  cellUI(i, j, "mine")
}

wrongCellUI <- function(i, j) {
  cellUI(i, j, "wrong")
}

ongoingGridUI <- function(inputId, game, state) {
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

  cells = list()
  for (i in 1:game$nrow) {
    for (j in 1:game$ncol) {
      cell = if (state$checked[i, j]) {
        checkedCellUI(i, j, game$nearby_mines[i, j])
      } else if (state$flagged[i, j]) {
        flagCellUI(i, j)
      } else {
        hiddenCellUI(i, j)
      }

      cells = append(cells, list(cell))
    }
  }

  tagList(
    singleton(tags$head(tags$script(jscode))),
    tags$svg(
      `data-input-id` = inputId,
      class = "minesweeper-grid ongoing",
      width = "100%",
      viewBox = sprintf("1 1 %d %d", game$ncol, game$nrow),
      tagList(cells)
    )
  )
}

defeatGridUI <- function(game, state) {
  cells = list()
  for (i in 1:game$nrow) {
    for (j in 1:game$ncol) {
      cell = if (state$checked[i, j] && game$mines[i, j]) {
        wrongCellUI(i, j)
      } else if (state$checked[i, j]) {
        checkedCellUI(i, j, game$nearby_mines[i, j])
      } else if (state$flagged[i, j] && !game$mines[i, j]) {
        wrongFlagCellUI(i, j)
      } else if (state$flagged[i, j]) {
        flagCellUI(i, j)
      } else if (game$mines[i, j]) {
        mineCellUI(i, j)
      } else {
        hiddenCellUI(i, j)
      }

      cells = append(cells, list(cell))
    }
  }

  tags$svg(
    class = "play-grid defeat",
    width = "100%",
    viewBox = sprintf("1 1 %d %d", game$ncol, game$nrow),
    tagList(cells)
  )
}

victoryGridUI <- function(game) {
  cells = list()
  for (i in 1:game$nrow) {
    for (j in 1:game$ncol) {
      cell = if (game$mines[i, j]) {
        flagCellUI(i, j)
      } else {
        checkedCellUI(i, j, game$nearby_mines[i, j])
      }

      cells = append(cells, list(cell))
    }
  }

  tags$svg(
    class = "play-grid victory",
    width = "100%",
    viewBox = sprintf("1 1 %d %d", game$ncol, game$nrow),
    tagList(cells)
  )
}

minesweeperUI <- function(id) {
  ns = NS(id)
  tagList(
    textOutput(ns("game_status")),
    textOutput(ns("flag_count")),
    textOutput(ns("timer")),
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
  addResourcePath("assets", system.file('assets', package="Minesweeper"))

  moduleServer(
    id,
    function(input, output, session) {
      elapsedTime = reactiveVal(0)

      reactiveState = reactiveVal()

      reactiveGame = eventReactive(reactiveStartGame(), {
        elapsedTime(-1)

        mines = createMinesGrid(
          nrow=reactiveNrow(),
          ncol=reactiveNcol(),
          nmines=reactiveNmines()
        )

        game = createGame(mines)
        reactiveState(createInitialState(game))

        game
      })

      reactiveGameStatus = reactive({
        game = reactiveGame()
        state = reactiveState()

        gameStatus(game, state)
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
        status = reactiveGameStatus()

        if (status == "victory") {
          victoryGridUI(game)
        } else if (status == "defeat") {
          defeatGridUI(game, state)
        } else if (status == "ongoing") {
          ongoingGridUI(ns("play_grid"), game, state)
        }
      })

      output$game_status <- renderText({
        status = reactiveGameStatus()

        sprintf("Game status: %s", status)
      })

      output$flag_count <- renderText({
        game = reactiveGame()
        state = reactiveState()

        sprintf("Remaining flags: %d", game$nmines - sum(state$flagged & !state$checked))
      })

      output$timer <- renderText({
        status = reactiveGameStatus()

        if (status == "ongoing") {
          invalidateLater(1000)
        }

        isolate({
          elapsedTime(elapsedTime() + 1)
        })

        sprintf("Elapsed time: %02d:%02d", floor(elapsedTime() / 60), elapsedTime() %% 60);
      })
    }
  )
}
