#' Saving each generation grid as PNG files
#'
#' Internal helper function. This function allows to save each generation grid as PNG files in a desired location.
#' PNG files are named using the format \code{prefix_index.png}. Users choose their own prefix and the zero-padding. Files are indexed according to the generation number.
#'
#' @param grid A matrix of any size.
#' @param frames_path A character string giving the absolute or relative path
#' to the directory where frames are saved. The directory is created if it does not exist.
#' Defaults to the current working directory, \code{getwd()}.
#' @param frames_prefix A character string for naming frames. Defaults to "frame".
#' @param zero_padding An integer defining the zero-padding for the index part of the frames name. Defaults to 5.
#' @param frame_index An integer defining the index of the frame. This index equals the generation number when run in \code{gameoflife()}.
#'
#' @noRd
.save_frames <- function(grid, frames_path=".", frames_prefix="frame", zero_padding=5, frame_index) {

  if (!is.matrix(grid)) {
    stop("grid must be a matrix")
  }

  if (!dir.exists(frames_path)) dir.create(frames_path, recursive = TRUE) # if the directory decided is not found, it is created, along with parent directories if necessary (recursive = TRUE)

  # Rename file following the format: prefix_00001.png
  file_name <- sprintf("%s_%0*d.png", frames_prefix, zero_padding, frame_index)
  file_path <- file.path(frames_path, file_name)

  grDevices::png(file_path)
  graphics::image(t(grid[nrow(grid):1, ]), axes=F, col=c("black", "grey")) # plot the grid and turn it to have (1,1) at the top left
  grDevices::dev.off()

  invisible(file_path)
}

#' Generating GIF from PNG files
#'
#' Internal helper function. This function allows to generate an animated GIF based on a set of PNG files.
#' GIF is generated based on existing PNG files in a known location, with a known name format (\code{prefix_index.png}).
#' Users choose the number of frames per second (fps), GIF name and output location.
#'
#' @param frames_path A character string giving the absolute or relative path
#' to the directory where frames are located.
#' @param frames_prefix A character string for frames name prefix.
#' @param gif_name A character string for naming resulting GIF. Caution: the name must contain the extension \code{.gif}.
#' @param fps An integer defining the GIF frames per second. Defaults to 20.
#' @param output_path A character string giving the absolute or relative path
#' to the directory where GIF is saved. Defaults to current working directory, \code{getwd()}.
#' @param zero_padding An integer defining the zero-padding for the index part of the frames name. Defaults to 5.
#'
#' @noRd
.make_gif <- function(frames_path, frames_prefix, gif_name, fps=20, output_path=".", zero_padding=5) { # "." means that by default, the output path will be the same localisation from which the script has been opened and run

  # Verify frames_path exist
  if (!dir.exists(frames_path)) {
    stop("frames_path does not exist")
  }

  # Verify if directory is found, otherwise it is created, along with parent directories if necessary (recursive = TRUE)
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # the pattern to be searched for in frame names
  pattern <- paste0("^", frames_prefix, "_[0-9]{", zero_padding, "}\\.png$")
  # "^" states that it is the begining of the file name, _ is the separator, [0-9]{5} means 5 figures (00001, ...), \\.png is the extension, $ states the end of the file name

  # Store frames
  frame_files <- magick::image_read(list.files(frames_path, pattern = pattern, full.names = TRUE))

  # Verify frame_files is not empty
  if (length(frame_files) == 0) {
    stop("No frames matching the expected name pattern were found")
  }

  # Create GIF
  gif <- magick::image_animate(frame_files, fps)

  # Save
  gif_path <- file.path(output_path, gif_name) # create a valid path name from a directory and a file name
  magick::image_write(gif, path = gif_path)

  invisible(gif_path)
}

#' Plotting binary (0, 1) matrices
#'
#' The function plots binary (containing only 0 and 1) matrix
#'
#' @param grid A binary (containing only 0 and 1) matrix or a character string
#' naming a built-in pattern or a list combining such matrices and/or pattern names.
#' Each element is plotted in a separate panel.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effects
#' (plotting the matrix).
#'
#' @details Uses the internal library of patterns containing a named list of binary matrices representing a few common Game of Life patterns.
#' Available built-in pattern names can be obtained with \code{list_patterns()}.
#' The patterns library can be temporarily or permanently enriched with \code{add_pattern()}.
#'
#' @export
#'
#' @examples
#' # direct matrix
#' glider <- matrix(c(0,1,0,
#' 0,0,1,
#' 1,1,1),
#' nrow = 3,
#' byrow = TRUE)
#'
#' plot_grid(glider)
#'
#' # built-in matrix
#' plot_grid("pulsar")
#'
#' # multiple patterns
#' glider <- matrix(c(0,1,0,
#' 0,0,1,
#' 1,1,1),
#' nrow = 3,
#' byrow = TRUE)
#'
#' plot_grid(list("pulsar", glider))
plot_grid <- function(grid) {

  # Normalize input to a list
  if (is.matrix(grid) || (is.character(grid) && length(grid) == 1)) {
    grid <- list(grid)
  } else if (is.character(grid)) {
    grid <- as.list(grid)
  } else if (!is.list(grid)) {
    stop("grid must be a matrix, a pattern name or a list of those")
  }

  # Resolve inputs to matrices
  grids <- vector("list", length(grid))

  for (k in seq_along(grid)) {

    g <- grid[[k]]

    # A unique matrix
    # If character, assume it's a pattern name
    if (is.character(g) && length(g) == 1) {
      if (!exists(g, envir = .patterns_env, inherits = FALSE)) {
        stop("Unknown pattern: ", grid, "\nAvailable patterns: ", paste(ls(.patterns_env), collapse = ", "))
      }

      grids[[k]] <- .patterns_env[[g]]

    } else if (is.matrix(g)) {
      if (!all(g %in% c(0, 1))) {
        stop("grid must be a binary matrix containing only 0 and 1")
      }
      grids[[k]] <- g

    } else {
      stop("Each element must be a matrix or a pattern name")
    }
  }

    n.col <- ceiling(sqrt(length(grid)))
    n.row <- ceiling(length(grid)/n.col)

    old_par <- graphics::par(mfrow=c(n.row,n.col), mar=c(1,1,1,1), oma=c(0,0,0,0))

    # To be applied when the function exits
    on.exit(graphics::par(old_par), add = TRUE)

    for (g in grids) {
      graphics::image(t(g[nrow(g):1, ]), axes=F, col=c("black", "grey"))
    }

    invisible(NULL)
}
