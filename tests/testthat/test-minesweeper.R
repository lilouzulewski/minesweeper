test_that("createGame works", {
  mines = matrix(nrow=6, ncol=12, byrow=TRUE, data=as.logical(c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1,
    0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0,
    0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0
  )))

  game = createGame(mines)

  expect_equal(game$nrow, 6)
  expect_equal(game$ncol, 12)
  expect_equal(game$nmines, 15)

  expect_equal(game$mines, mines)
  expect_equal(dim(game$nearby_mines), c(6, 12))
})

test_that("createMinesGrid works", {
  mines = createMinesGrid(nrow=20, ncol=15, nmines=30)

  expect_equal(dim(mines), c(20, 15))
  expect_equal(sum(mines), 30)
})

test_that("nearbyCoords works", {
  # helper function to make `expect_setequal` works with rows
  f <- function(df) {
    t(unname(df))
  }

  expect_setequal(
    f(nearbyCoords(1, 1, nrow=5, ncol=5)),
    f(expand.grid(1:2, 1:2))
  )

  expect_setequal(
    f(nearbyCoords(3, 2, nrow=5, ncol=5)),
    f(expand.grid(2:4, 1:3))
  )

  expect_setequal(
    f(nearbyCoords(1, 1, nrow=1, ncol=1)),
    f(expand.grid(1:1, 1:1))
  )

  expect_setequal(
    f(nearbyCoords(3, 4, nrow=3, ncol=5)),
    f(expand.grid(2:3, 3:5))
  )
})

test_that("countNearbyMines works", {
  grid = matrix(nrow=6, ncol=12, byrow=TRUE, data=as.logical(c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1,
    0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0,
    0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0
  )))

  expect_equal(countNearbyMines(grid, 1, 1), 1)
  expect_equal(countNearbyMines(grid, 1, 12), 0)
  expect_equal(countNearbyMines(grid, 5, 11), 0)
  expect_equal(countNearbyMines(grid, 5, 1), 1)
  expect_equal(countNearbyMines(grid, 4, 3), 3)
  expect_equal(countNearbyMines(grid, 5, 5), 4)
})


test_that("createInitialState works", {
  mines = createMinesGrid(nrow=20, ncol=15, nmines=30)
  game = createGame(mines)
  state = createInitialState(game)

  expect_equal(dim(state$checked), c(20, 15))
  expect_equal(any(state$checked), FALSE)

  expect_equal(dim(state$flagged), c(20, 15))
  expect_equal(any(state$flagged), FALSE)
})