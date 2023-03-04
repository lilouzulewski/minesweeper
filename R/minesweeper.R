createGame <- function(mines) {
  list(
    nrow = nrow(mines),
    ncol = ncol(mines),
    nmines = sum(mines),
    mines = mines,
    nearby_mines = createNearbyMinesGrid(mines)
  )
}

createMinesGrid <- function(nrow, ncol, nmines) {
  mines = rep(TRUE, nmines)
  empty = rep(FALSE, ncol * nrow - nmines)
  shuffled_mines = sample(c(mines, empty))

  mines_grid = matrix(shuffled_mines, nrow=nrow, ncol=ncol)

  mines_grid
}

nearbyCoords <- function(i, j, nrow, ncol) {
  expand.grid(
    i = max(1, i-1):min(nrow, i+1),
    j = max(1, j-1):min(ncol, j+1)
  )
}

countNearbyMines <- function(mines, i, j) {
  coords = nearbyCoords(i, j, nrow=nrow(mines), ncol=ncol(mines))

  sum(apply(coords, 1, function(coord) {
    mines[coord["i"], coord["j"]]
  }))
}

createNearbyMinesGrid <- function(mines) {
  result = matrix(nrow = nrow(mines), ncol = ncol(mines))

  for (i in 1:nrow(mines)) {
    for (j in 1:ncol(mines)) {
      result[i, j] = countNearbyMines(mines, i, j)
    }
  }

  result
}

createInitialState <- function(game) {
  list(
    flagged = createFlaggedGrid(ncol=game$ncol, nrow=game$nrow),
    checked = createCheckedGrid(ncol=game$ncol, nrow=game$nrow)
  )
}

createFlaggedGrid <- function(nrow, ncol) {
  matrix(rep(FALSE, nrow * ncol), nrow=nrow, ncol=ncol)
}

createCheckedGrid <- function(nrow, ncol) {
  matrix(rep(FALSE, nrow * ncol), nrow=nrow, ncol=ncol)
}

flagMine <- function(state, i, j) {
  state$flagged[i, j] = TRUE
  state
}

unflagMine <- function(state, i, j) {
  state$flagged[i, j] = FALSE
  state
}

gameStatus <- function(game, state) {
  if (any(problem$mines & state$checked)) {
    return("defeat")
  }

  if (all(xor(problem$mines, state$checked))) {
    return("victory")
  }

  "ongoing"
}

checkMine <- function(game, state, i, j) {
  if (state$checked[i, j]) {
    return(state)
  }

  state$checked[i, j] = TRUE

  if (game$nearby_mines[i, j] == 0) {
    for (i in coords$i) {
      for (j in coords$i) {
        state = check_mine(game, state, i, j)
      }
    }
  }

  state
}

display <- function(game, state) {
  ifelse(
    state$checked,
    game$nearby_mines,
    ifelse(
      state$flagged,
      "!",
      "*"
    )
  )
}
