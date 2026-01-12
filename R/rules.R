#' Counting the number of living cells surrounding one cell
#'
#' Internal helper function. This function allows to count the number of living cells among the eight cells surrounding one given cell.
#' In a binary matrix, the function counts the number of '1' surrounding a cell for any row and column.
#' To identify the neighbors of a cell, the function uses the modulo to simulate a torroidal grid.
#'
#' @param grid A binary matrix (containing only 0 and 1) of any size.
#' @param i An integer for row coordinate.
#' @param j An integer for column coordinate.
#'
#' @noRd
.count_neighbors <- function(grid, i, j) {

  if (i < 1 || i > nrow(grid) || j < 1  || j > ncol(grid)) {
    stop("coordinates exceed of grid dimensions")
  }

  if (!all(grid %in% c(0, 1))) {
    stop("grid must be a binary matrix containing only 0 and 1")
  }

  # Use modulo for a torroidal grid
  i_up <- ((i - 2) %% nrow(grid)) + 1
  i_down <- (i %% nrow(grid)) + 1
  j_left <- ((j - 2) %% ncol(grid)) + 1
  j_right <- (j %% ncol(grid)) + 1

  # Count the number of neighbors
  sum <- sum(grid[i_up, j_left],
             grid[i_up, j],
             grid[i_up, j_right],
             grid[i, j_left],
             grid[i, j_right],
             grid[i_down, j_left],
             grid[i_down, j],
             grid[i_down, j_right])
  return(sum)
}




#' Generating next generation grid following Conway's Game of Life rules
#'
#' Internal helper function. This function applies the Conway's Game of Life rules by counting the number of living neighbors (i.e. 1 in a binary matrix) of a given cell, and storing results in a new matrix \code{new.grid}.
#' The Conway's Game of Life rules can be summarised as:
#' If a dead cell (i.e. 0 in a binary matrix) has exactly 3 living neighbors, it becomes alive at the next generation.
#' If a living cell (i.e. 1 in a binary matrix) has less than 2, or more than 3 living neighbors, it dies at the next generation.
#'
#' @param grid A binary matrix (containing only 0 and 1) of any size.
#'
#' @noRd
.next_generation <- function(grid) {
  new.grid <- grid # create a new grid to store results of the next generation

  for (i in seq_len(nrow(grid))) { # iterate across each row
    for (j in seq_len(ncol(grid))) { # iterate across each column
      neighbors <- .count_neighbors(grid, i, j)

      if (grid[i,j] == 0 & neighbors == 3) # if a cell is dead AND has exactly 3 neighbors alive...
        new.grid[i,j] <- 1 #... then the dead cell becomes alive (and we store the result in the new grid)
      if (grid[i,j] == 1 & (neighbors < 2 | neighbors > 3)) # if a cell is alive AND has less than two OR more than three neighbors alive...
        new.grid[i,j] <- 0 #... then the cell dies (the result is stored in the new grid)
    }
  }
  return(new.grid)
}
