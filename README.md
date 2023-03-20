
# Minesweeper

<!-- badges: start -->
<!-- badges: end -->

This repository host an implementation of Minesweeper written in R and shiny.

## Installation

You can install the development version of Minesweeper from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")

devtools::install_github("lilouzulewski/minesweeper")
```

You can also clone this repository, open the project, install the dependencies and then run:

``` r
# install.packages("devtools")
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("dequer")

devtools::load_all()
```

## Example

You can run the application with:

``` r
library(Minesweeper)

startApp()
```

## Run test and check

To run test you must clone this repository, open and load the project, make sure to install dependencies then run:

``` r
# install.packages("devtools")
# install.packages("shiny")
# install.packages("dequer")

devtools::test()
devtools::check()
```

## Use as library

You can also use this package as a standalone minesweeper implementation library without the UI.

Here is some examples.

``` r
library(Minesweeper)

minesGrid = createMinesGrid(nrow=10, ncol=10, nmines=20)
game = createGame(minesGrid)
state = createInitialState(game)

state = revealCell(game, state, 3, 7)
state = flagCell(state, 2, 4)
state = unflagCell(state, 2, 4)

status = gameStatus(game, state)
```

You can get more information using `help(flagCell)` for example.
