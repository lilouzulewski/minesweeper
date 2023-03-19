
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
