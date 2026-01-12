#' Running Conway's Game of Life simulations
#'
#' This function allows to run Conway's Game of Life simulations. The Game of Life occurs in a toroidal grid of chosen size for a chosen generation number.
#' The Conway's Game of Life rules can be summarised as:
#' If a dead cell (i.e. 0 in a binary matrix) has exactly 3 living neighbors, it becomes alive at the next generation.
#' If a living cell (i.e. 1 in a binary matrix) has less than 2, or more than 3 living neighbors, it dies at the next generation.
#'
#' The function allows to simulate a randomly generated grid or to personalize the initial state by choosing the initial position and orientation of one or several built-in patterns.
#' Available built-in pattern names can be obtained with \code{list_patterns()}.
#'
#' The function allows to save each frame of the simulation as PNG files in a desired location and to assemble them in a GIF file.
#'
#' @param nrow An integer specifying the number of rows of the grid to be generated.
#' @param ncol An integer specifying the number of columns of the grid to be generated.
#' @param generations An integer specifying the number of generation through which the simulation is run.
#' @param p A numeric between 0 and 1 specifying the proportion of living cells (i.e. proportion of 1 in a binary matrix containing only 0 and 1). Defaults to `0.5`
#' @param pattern A character vector of pattern names to paste into grid. Defaults to "random", i.e. a randomly generated binary matrix (containing only 0 and 1 with a proportion `p`) of `nrow` x `ncol` dimensions.
#' Available built-in pattern names can be obtained with \code{list_patterns()}.
#' @param pos.row A numeric vector specifying the desired row position of the pattern into the grid.
#' @param pos.col A numeric vector specifying the desired column position of the pattern into the grid.
#' @param rotation A numeric vector for the desired rotation, in degrees. Rotation must be one of `0`, `90`, `180` or `270` for no rotation, a clockwise rotation, an upside-down rotation or a counter-clockwise rotation, respectively.
#' If \code{length(rotation) == 1}, the rotation value is applied to all patterns.
#' @param record Logical. If `TRUE`, the simulation is not plotted but frames are saved and assembled into a GIF file at a desired location. Default is `FALSE`.
#' @param frames_path A character string giving the absolute or relative path to the directory where frames are saved when \code{record = TRUE}.
#' The directory is created if it does not exist.
#' @param frames_prefix A character string for frames name prefix.
#' @param zero_padding An integer defining the zero-padding for the index part of the frames name. Defaults to `5`.
#' @param gif_name A character string for naming resulting GIF. Caution: the name must contain the extension \code{.gif}.
#' @param fps An integer defining the GIF frames per second. Defaults to `20`.
#' @param output_path A character string giving the absolute or relative path to the directory where GIF is saved. Defaults to current working directory, \code{getwd()}.
#'
#' @details Uses the internal library of patterns containing a named list of binary matrices representing a few common Game of Life patterns.
#' Available built-in pattern names can be obtained with \code{list_patterns()}.
#' The patterns library can be temporarily or permanently enriched with \code{add_pattern()}.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effects
#' (plotting the simulation or writing PNG/GIF files).
#'
#' @export
#'
#' @examples
#' # runs a simple simulation on a randomly generated grid (living cells: 30%, 100x100, 20 generations)
#' gameoflife(nrow = 100,
#' ncol = 100,
#' generations = 20,
#' p = 0.3)
#'
#' # runs a simulation with two specific patterns (cf. library). Result is recorded as GIF
#' \dontrun{
#' gameoflife(nrow = 100,
#' ncol = 100,
#' generations = 50,
#' pattern = c("glider", "gosper_gun"),
#' pos.row = c(10, 60),
#' pos.col = c(10, 60),
#' rotation = 90,
#' record = TRUE,
#' frames_prefix = "gosper_glider",
#' zero_padding = 3,
#' gif_name = "gosper_glider.gif",
#' output_path = "animations/")
#' }
gameoflife <- function(nrow, ncol, generations, p=0.5,
                       pattern = "random", pos.row=NULL, pos.col=NULL, rotation = 0,
                       record = FALSE, frames_path=".", frames_prefix="frame", zero_padding=5,
                       gif_name = NULL, fps=20, output_path=".") {
  stopifnot(
    is.numeric(nrow), nrow > 0,
    is.numeric(ncol), ncol > 0,
    is.numeric(generations), generations > 0
  )

  if (p < 0 | p > 1) {
    stop("p must be a probability between 0 and 1")
  }

  # If record = TRUE, frames are assembled in a saved GIF
  if (record && is.null(gif_name)) {
    stop("When record = TRUE, gif_name must be provided")
  }

  # Check which pattern to be plotted
  if (pattern[1] == "random") {
    grid <- .patterns_env[["random"]](nrow, ncol, p)
  } else {
    grid <- matrix(0, nrow, ncol)
    grid <- .place_patterns(grid, pattern, pos.row, pos.col, rotation)
  }

  graphics::par(mar=c(0,0,0,0), oma=c(0,0,0,0)) # no inner and outter margins

  # Run simulation for a generation number g
  for (g in seq_len(generations)) {

    # If record = TRUE, result is not displayed but frames are saved
    if (record) {
      .save_frames(grid, frames_path, frames_prefix, zero_padding, frame_index=g)
    }

    # if record = FALSE, result is displayed
    if (!record) {
      graphics::image(t(grid[nrow(grid):1, ]), axes=F, col=c("black", "grey")) # plot the grid and turn it to have (1,1) at the top left
      Sys.sleep(0.05) # add sleeping time between each iteration to observe changes across generations
    }

    grid <- .next_generation(grid) # new.grid stored back in grid
  }

  if (record) {
    .make_gif(frames_path, frames_prefix, gif_name, fps, output_path, zero_padding)
  }

  invisible(NULL)
}

