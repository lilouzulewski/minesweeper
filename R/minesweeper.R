#' Create a game object
#'
#' @export
#' @param mines a boolean matrix with `TRUE` on mines
createGame <- function(mines) {
  list(
    nrow = nrow(mines),
    ncol = ncol(mines),
    nmines = sum(mines),
    mines = mines,
    nearby_mines = createNearbyMinesGrid(mines)
  )
}

#' Create a mines grid
#'
#' @export
#' @param nrow number of rows
#' @param ncol number of columns
#' @param nmines number of mines
createMinesGrid <- function(nrow, ncol, nmines) {
  mines = rep(TRUE, nmines)
  empty = rep(FALSE, ncol * nrow - nmines)
  shuffled_mines = sample(c(mines, empty))

  mines_grid = matrix(shuffled_mines, nrow=nrow, ncol=ncol)

  mines_grid
}

#' Given i, j return a vector containing all nearby coordinates
#'
#' @param i the row coordinate
#' @param j the column coordinate
#' @param nrow the number of rows of the grid
#' @param ncol the number of columns of the grid
nearbyCoords <- function(i, j, nrow, ncol) {
  as.matrix(expand.grid(
    i = max(1, i-1):min(nrow, i+1),
    j = max(1, j-1):min(ncol, j+1)
  ))
}

#' Count the number of nearby mines
#'
#' @param mines the mines grid
#' @param i the row coordinate
#' @param j the column coordinate
countNearbyMines <- function(mines, i, j) {
  coords = nearbyCoords(i, j, nrow=nrow(mines), ncol=ncol(mines))

  sum(apply(coords, 1, function(coord) {
    mines[coord["i"], coord["j"]]
  }))
}

#' Create a matrix that map i, j coordinates to the count of nearby mines
#'
#' @param mines the mines grid
createNearbyMinesGrid <- function(mines) {
  result = matrix(nrow = nrow(mines), ncol = ncol(mines))

  for (i in 1:nrow(mines)) {
    for (j in 1:ncol(mines)) {
      result[i, j] = countNearbyMines(mines, i, j)
    }
  }

  result
}

#' Create initial state of the game
#'
#' @export
#' @param game the game
createInitialState <- function(game) {
  list(
    flagged = createFlaggedGrid(ncol=game$ncol, nrow=game$nrow),
    revealed = createRevealedGrid(ncol=game$ncol, nrow=game$nrow)
  )
}

#' Create flagged grid
#'
#' @param nrow number of rows
#' @param ncol number of columns
createFlaggedGrid <- function(nrow, ncol) {
  matrix(rep(FALSE, nrow * ncol), nrow=nrow, ncol=ncol)
}

#' Create revealed grid
#'
#' @param nrow number of rows
#' @param ncol number of columns
createRevealedGrid <- function(nrow, ncol) {
  matrix(rep(FALSE, nrow * ncol), nrow=nrow, ncol=ncol)
}

#' Given a state and i, j return a new state with the cell i, j flagged
#'
#' @export
#' @param state the state of the game
#' @param i the row coordinate
#' @param j the column coordinate
flagCell <- function(state, i, j) {
  state$flagged[i, j] = TRUE
  state
}

#' Given a state and i, j return a new state with the cell i, j unflagged
#'
#' @export
#' @param state the state of the game
#' @param i the row coordinate
#' @param j the column coordinate
unflagCell <- function(state, i, j) {
  state$flagged[i, j] = FALSE
  state
}

#' Given a game, a state and i, j return a new state with the cell i, j and
#' recursively all its non-ambiguous neighbors revealed
#'
#' @export
#' @param game the game
#' @param state the state of the game
#' @param i the row coordinate
#' @param j the column coordinate
revealCell <- function(game, state, i, j) {
  # since R doesn't allow to pass arguments by reference we can't use the
  # straightforward recursive algorithm because of memory usage issues
  remaining_coords = matrix(ncol=2, data=c(i, j))
  colnames(remaining_coords) <- c("i", "j")

  while (length(remaining_coords) > 0) {
    last = nrow(remaining_coords)

    coords = remaining_coords[last,]
    remaining_coords = remaining_coords[-last,, drop=FALSE]

    i = coords["i"]
    j = coords["j"]

    if (state$revealed[i, j]) {
      next
    }

    state$revealed[i, j] = TRUE

    if (game$nearby_mines[i, j] == 0) {
      remaining_coords = rbind(
        remaining_coords,
        nearbyCoords(i, j, nrow=game$nrow, ncol=game$ncol)
      )
    }
  }

  state
}

#' Given a game and a state return the status of the game
#'
#' @export
#' @param game the game
#' @param state the state of the game
gameStatus <- function(game, state) {
  if (any(game$mines & state$revealed)) {
    return("defeat")
  }

  if (all(xor(game$mines, state$revealed))) {
    return("victory")
  }

  "ongoing"
}
