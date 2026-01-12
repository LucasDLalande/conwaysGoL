#' Parsing .rle files into binary (0, 1) matrices
#'
#' The function supports standard Run Length Encoded (RLE) files commonly used
#' to describe Conway's Game of Life patterns.
#' The function reads an \code{.rle} file, extracts the RLE code and converts it
#' into a binary matrix (containing only 0 and 1).
#'
#' @param rle_path A character string for the relative or absolute path to an .rle file.
#'
#' @return A binary matrix (containing only 0 and 1)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' glider <- read_rle("patterns/glider.rle")
#' image(glider)
#' }
read_rle <- function(rle_path) {
  # Import .rle file, suppress readLines benign warnings and remove comments
  rle_file <- suppressWarnings(readLines(rle_path))
  rle_file <- rle_file[!grepl("^\\s*#", rle_file)]

  # extract x/y header line
  header_idx <- grep("x\\s*=", rle_file)

  if (length(header_idx) != 1) {
    stop("Invalid RLE file: cannot find unique x/y header line")
  }

  # extract x as the number of column
  rle_col <- as.integer(sub(".*x\\s*=\\s*([0-9]+).*", "\\1", rle_file[header_idx]))
  # extract y as the number of row
  rle_row <- as.integer(sub(".*y\\s*=\\s*([0-9]+).*", "\\1", rle_file[header_idx]))

  # extract and paste strings relating to rle code, and remove spacings
  rle_code <- rle_file[(header_idx + 1):length(rle_file)]
  rle_code <- paste(rle_code, collapse="")
  rle_code <- sub("\\s+", "", rle_code)

  # recognise end of code
  end_idx <- regexpr("!", rle_code)
  if (end_idx == -1) {
    stop("Invalid RLE file: missing '!' end-of-code marker")
  }
  # extract only the code from its start to its end "!"
  rle_code <- substr(rle_code, 1, end_idx)

  # convert rle code into separated characters
  rle_characters <- unlist(strsplit(rle_code, split=""))

  # empty vector to store repetitions
  repetition <- ""
  # basic grid filled with 0
  rle_grid <- matrix(0, nrow = rle_row, ncol = rle_col)
  # set starting row and column position
  row <- 1
  col <- 1

  # store number of repetition in n, if none n = 1
  for (c in seq_along(rle_characters)) {
    if(grepl("[0-9]", rle_characters[c])) {
      repetition <- paste0(repetition, rle_characters[c])
    } else {
      n <- as.integer(repetition)
      if(is.na(n)) {
        n <- 1
      }
      repetition <- "" # reset repetition

      # fill grid with 0
      if (rle_characters[c] == "b") { # 'b' stands for dead cell
        # verify filling fit grid dimensions
        if (row > rle_row) {
          stop("Filling exceed grid row number")
        }
        if (col + n -1 > rle_col) {
          stop("Filling exceed grid column number")
        }
        rle_grid[row, col:(col + n - 1)] <- 0
        col <- col + n

        # fill grid with 1
      } else if (rle_characters[c] == "o") { # 'o' stands for living cell
        # verify filling fit grid dimensions
        if (row > rle_row) {
          stop("Filling exceed grid row number")
        }
        if (col + n - 1 > rle_col) {
          stop("Filling exceed grid column number")
        }
        rle_grid[row, col:(col + n - 1)] <- 1
        col <- col + n

        # Next row
      } else if (rle_characters[c] == "$") { # '$' means end of line; repetition means multiple empty rows
        col <- 1
        row <- row + n

        # Code ending
      } else if (rle_characters[c] == "!") { # '!' means end of code
        break
      }
    }
  }
  return(rle_grid)
}
