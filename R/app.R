library(shiny)
library(shinyjs)

cellUi <- function(i, j, game, state) {
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
    tags$image(
      sprintf("checked_%d", game$nearby_mines[i, j])
    )
  } else if (state$flagged[i, j]) {
    tag_image("flag")
  } else {
    tag_image("hidden", class="hidden-cell")
  }
}

inputGameGridUi <- function(inputId, game, state) {
  jscode <- r"(
    $(document).on("click", ".minesweeper-grid .hidden-cell", function(evt) {
      const el = $(evt.target);

      el.trigger("change");
    });


    const minesweeperGrid = new Shiny.InputBinding()
    $.extend(minesweeperGrid, {
      find: function(scope) {
        return $(scope).find(".minesweeper-grid")
      },
      getValue: function(el) {
        return parseInt($(el).text())
      },
      setValue: function(el, value) {
        $(el).text(value)
      },
      subscribe: function(el, callback) {
        $(el).on("change.minesweeperGrid", function(e) {
          callback()
        })
      },
      unsubscribe: function(el) {
        $(el).off(".minesweeperGrid")
      }
    });

    Shiny.inputBindings.register(minesweeperGrid)
  )"

  status = gameStatus(game, state)
  if (status == "victory") {

  } else if (status == "defeat") {

  } else if (status == "ongoing") {
    cells = list()
    for (i in 1:game$nrow) {
      for (j in 1:game$ncol) {
        print(cellUi(i, j, game, state))
        cells = append(cells, list(cellUi(i, j, game, state)))
      }
    }

    tagList(
      singleton(tags$head(tags$script(jscode))),
      tags$svg(id = inputId,
        class = "minesweeper-grid",
        viewBox = sprintf("0 0 %d %d", game$nrow, game$ncol),
        tagList(cells)
      )
    )
  }
}

startApp <- function() {
  addResourcePath("assets", system.file('assets', package="Minesweeper"))

  ui <- fluidPage(
    #useShinyjs(),
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
      width=8
    )
  )

  server <- function(input, output) {
    mines <- eventReactive(input$new_game, {
      createMinesGrid(
        nrow=input$nrow,
        ncol=input$ncol,
        nmines=input$nmines
      )
    })

    output$minesweeper_grid_output <- renderUI({
      game = createGame(mines())
      state = createInitialState(game)
      inputGameGridUi("minesweeper_grid_input", game, state)
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}

