#' Pasting a pattern into grid
#'
#' Internal helper function. This function allows to paste a pattern, which is a binary matrix, at a desired row and column, into a larger matrix.
#'
#' @param grid A binary matrix (containing only 0 and 1) of any size into which the desired pattern will be pasted.
#' @param pattern The desired pattern to paste into grid. Must be a valid matrix of smaller size than `grid`.
#' @param pos.row The desired row position of the pattern into the grid.
#' @param pos.col The desired column position of the pattern into the grid.
#'
#' @noRd
.place_pattern <- function(grid, pattern, pos.row, pos.col) {

  # Verify the pattern fit the grid
  if (pos.row + nrow(pattern) - 1 > nrow(grid) || pos.col + ncol(pattern) - 1 > ncol(grid)) {
    stop("pattern is bigger than grid size")
  }

  # Paste pattern into grid
  grid[pos.row:(pos.row + nrow(pattern) - 1), pos.col:(pos.col + ncol(pattern) - 1)] <- pattern

  return(grid)
}

#' Rotating a pattern
#'
#' Internal helper function. This function allows to rotate a pattern clockwise, counter-clockwise and upside-down.
#'
#' @param pattern A matrix to be rotated
#' @param rotation A numeric argument for the desired rotation, in degrees. Rotation must be one of 0, 90, 180 or 270 for no rotation, a clockwise rotation, an upside-down rotation or a counter-clockwise rotation, respectively.
#'
#' @noRd
.rotate_pattern <- function(pattern, rotation) {

  # Rotation in degrees
  rotation <- rotation %% 360

  # Verify rotation is correct
  if (!rotation %in% c(0, 90, 180, 270)) {
    stop("rotation must be one of 0, 90, 180, 270")
  }
  # No rotation
  if (rotation == 0) {
    return(pattern)
  }

  # Clockwise rotation
  if (rotation == 90) {
    return(t(pattern[nrow(pattern):1, ]))
  }

  # Upside-down
  if (rotation == 180) {
    return(pattern[nrow(pattern):1, ncol(pattern):1])
  }

  # Counter_clockwise
  if (rotation == 270) {
    return(t(pattern[, ncol(pattern):1]))
  }
}

#' Pasting one or more patterns into grid and rotating patterns
#'
#' Internal helper function. This function combines the \code{.place_pattern()} and \code{.rotate_pattern()} functions to paste and rotate one or several patterns, which must binary matrices, at desired rows and columns, into a larger matrix.
#'
#' @param grid A binary matrix (containing only 0 and 1) of any size into which the desired patterns will be pasted.
#' @param pattern A character vector of pattern names to paste into grid.
#' @param pos.row The desired row position of the pattern into the grid.
#' @param pos.col The desired column position of the pattern into the grid.
#' @param rotation A numeric argument for the desired rotation, in degrees. Rotation must be one of 0, 90, 180 or 270 for no rotation, a clockwise rotation, an upside-down rotation or a counter-clockwise rotation, respectively.
#'
#' @details Uses the internal library of patterns containing a named list of binary matrices representing a few common Game of Life patterns.
#'
#' @noRd
.place_patterns <- function(grid, pattern, pos.row, pos.col, rotation = 0) {

  # Verify the pattern and positions have the same lengths
  if (length(pattern) != length(pos.row) || length(pattern) != length(pos.col)) {
    stop ("pattern, pos.row, pos.col must have the same length")
  }

  # Verify rotation length
  if (length(rotation) == 1) {
    rotation <- rep(rotation, length(pattern))
  }

  if (length(rotation) != length(pattern)) {
    stop ("rotation must be length 1 or the same length as pattern")
  }

  # Verify pattern names
  for (k in seq_along(pattern)) {
    if (!exists(pattern[k], envir = .patterns_env, inherits = FALSE)) {
      stop ("Unknown pattern: ", pattern[k], "\nPlease choose among: ", paste(ls(.patterns_env), collapse = ", "))
    }

    pat <- .patterns_env[[pattern[k]]]

    # Rotate patterns
    pat <- .rotate_pattern(pat, rotation[k])

    # Paste patterns into grid
    grid <- .place_pattern(grid, pat, pos.row[k], pos.col[k])
  }
  return(grid)
}
