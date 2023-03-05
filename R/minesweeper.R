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

flagCell <- function(state, i, j) {
  state$flagged[i, j] = TRUE
  state
}

unflagCell <- function(state, i, j) {
  state$flagged[i, j] = FALSE
  state
}


checkCell <- function(game, state, i, j) {
  if (state$checked[i, j]) {
    return(state)
  }

  state$checked[i, j] = TRUE

  if (game$nearby_mines[i, j] == 0) {
    coords = nearbyCoords(i, j, nrow=game$nrow, ncol=game$ncol)
    for (i in coords$i) {
      for (j in coords$j) {
        state = checkCell(game, state, i, j)
      }
    }
  }

  state
}

gameStatus <- function(game, state) {
  if (any(game$mines & state$checked)) {
    return("defeat")
  }

  if (all(xor(game$mines, state$checked))) {
    return("victory")
  }

  "ongoing"
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

display2 <- function(game, state) {
  result = mapply(function(checked, flagged, nearby_mines) {
    if (checked) {
      return(nearby_mines)
    }

    if (flagged) {
      return("!")
    }

    return("*")
  }, state$checked, state$flagged, game$nearby_mines)

  matrix(result, nrow=game$nrow, ncol=game$ncol)
}
