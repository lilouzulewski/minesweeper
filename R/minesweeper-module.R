#' @import shiny
library(shiny)

#' Helper function to create cell element
#'
#' @param i row of the cell
#' @param j column of the cell
#' @param name type of the cell
#' @param class class of the html element
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

#' Create hidden cell element
#'
#' @param i row of the cell
#' @param j column of the cell
hiddenCellUI <- function(i, j) {
  cellUI(i, j, "hidden", class="hidden-cell")
}

#' Create flagged cell element
#'
#' @param i row of the cell
#' @param j column of the cell
flagCellUI <- function(i, j) {
  cellUI(i, j, "flag", class="flagged-cell")
}

#' Create wrongly flagged cell element
#'
#' @param i row of the cell
#' @param j column of the cell
wrongFlagCellUI <- function(i, j) {
  tagList(
    cellUI(i, j, "flag"),
    tags$rect(x=j, y=i, width=1, height=1, fill="rgba(255, 0, 0, 0.2)")
  )
}

#' Create revealed cell element
#'
#' @param i row of the cell
#' @param j column of the cell
#' @param nearby_mines number of nearby mines
revealedCellUI <- function(i, j, nearby_mines) {
  cellUI(i, j, sprintf("revealed_%d", nearby_mines))
}

#' Create mine cell element
#'
#' @param i row of the cell
#' @param j column of the cell
mineCellUI <- function(i, j) {
  cellUI(i, j, "mine")
}

#' Create wrong cell element
#'
#' @param i row of the cell
#' @param j column of the cell
wrongCellUI <- function(i, j) {
  cellUI(i, j, "wrong")
}

#' Helper function to create gridUI
#'
#' @param inputId the id of the input
#' @param nrow number of row
#' @param ncol number of column
#' @param status status of the game
#' @param fun function that take i, j coordinates and return cell element
gridUI <- function(inputId, nrow, ncol, status, fun) {
  cells = apply(expand.grid(1:nrow, 1:ncol), 1, function(index) {
    i = index[1]
    j = index[2]

    fun(i, j)
  })

  tagList(
    singleton(tags$head(tags$script(src="assets/js/minesweeper_grid_ui.js"))),
    tags$svg(
      `data-input-id` = inputId,
      class = sprintf("minesweeper-grid %s", status),
      width = 25 * ncol,
      viewBox = sprintf("1 1 %d %d", ncol, nrow),
      tagList(cells)
    )
  )
}

#' Create grid element of ongoing game
#'
#' @param inputId the input id
#' @param game the game to display
#' @param state the current state of the game
ongoingGridUI <- function(inputId, game, state) {
  gridUI(
    inputId = inputId,
    nrow = game$nrow,
    ncol = game$ncol,
    status = "ongoing",
    function(i, j) {
      if (state$revealed[i, j]) {
        revealedCellUI(i, j, game$nearby_mines[i, j])
      } else if (state$flagged[i, j]) {
        flagCellUI(i, j)
      } else {
        hiddenCellUI(i, j)
      }
    }
  )
}

#' Create defeat grid element
#'
#' @param game the game to display
#' @param state the current state of the game
defeatGridUI <- function(game, state) {
  gridUI(
    inputId = NULL,
    nrow = game$nrow,
    ncol = game$ncol,
    status = "defeat",
    function(i, j) {
      if (state$revealed[i, j] && game$mines[i, j]) {
        wrongCellUI(i, j)
      } else if (state$revealed[i, j]) {
        revealedCellUI(i, j, game$nearby_mines[i, j])
      } else if (state$flagged[i, j] && !game$mines[i, j]) {
        wrongFlagCellUI(i, j)
      } else if (state$flagged[i, j]) {
        flagCellUI(i, j)
      } else if (game$mines[i, j]) {
        mineCellUI(i, j)
      } else {
        hiddenCellUI(i, j)
      }
    }
  )
}

#' Create victory grid element
#'
#' @param game the game to display
victoryGridUI <- function(game) {
  gridUI(
    inputId = NULL,
    nrow = game$nrow,
    ncol = game$ncol,
    status = "defeat",
    function(i, j) {
      if (game$mines[i, j]) {
        flagCellUI(i, j)
      } else {
        revealedCellUI(i, j, game$nearby_mines[i, j])
      }
    }
  )
}

#' Create the full minesweeper ui
#'
#' @param id the id of the minesweeper module
minesweeperUI <- function(id) {
  ns = NS(id)
  tagList(
    uiOutput(ns("game_status")),
    textOutput(ns("flag_count")),
    textOutput(ns("timer")),
    uiOutput(ns("play_grid"))
  )
}

#' Create the status ui
#'
#' @param status current status of the game
statusUI <- function(status) {
  capitalize <- function(str) {
    paste0(toupper(substr(str, 1, 1)), substr(str, 2, nchar(str)))
  }

  color = if (status == "ongoing") {
    "white"
  } else if (status == "victory") {
    "green"
  } else if (status == "defeat") {
    "red"
  }


  tags$b(
    style = sprintf("color: %s;", color),
    capitalize(status)
  )
}

#' Create the minesweeper backend
#'
#' @param id the id of the minesweeper module
#' @param reactiveStartGame reactive value that trigger a new game
#' @param reactiveNrow reactive value that give the number of row
#' @param reactiveNcol reactive value that give the number of column
#' @param reactiveNmines reactive value that give the number of mines
minesweeperServer <- function(
  id,
  reactiveStartGame,
  reactiveNrow,
  reactiveNcol,
  reactiveNmines
) {
  addResourcePath("assets", system.file("www/assets", package="Minesweeper"))

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

        newState = if (action == "revealCell") {
          revealCell(game, state, i, j)
        } else if (action == "flagCell") {
          if (sum(state$flagged) < game$nmines) {
            flagCell(state, i, j)
          } else {
            state
          }
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

      output$game_status <- renderUI({
        status = reactiveGameStatus()

        tags$p(
          "Game status:",
          statusUI(status)
        )
      })

      output$flag_count <- renderText({
        game = reactiveGame()
        state = reactiveState()

        sprintf("Remaining flags: %d", game$nmines - sum(state$flagged & !state$revealed))
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

      observeEvent(reactiveGameStatus(), {
        status = reactiveGameStatus()
        if (status != "ongoing") {
          showModal(modalDialog(
            statusUI(status),
            easyClose = TRUE,
            fade = TRUE
          ))
        }
      })
    }
  )
}
