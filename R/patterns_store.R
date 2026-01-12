#' List available Game of Life patterns
#'
#' Returns the names of all built-in and permanently saved patterns available in
#' the package, as well as temporary patterns from the current session.
#'
#' @return A character vector of pattern names.
#' @export
#'
#' @examples
#' list_patterns()
list_patterns <- function() {
  ls(.patterns_env)
}

#' Add a personal pattern to the patterns library
#'
#' The function allows to add temporarily or permanently a new pattern to the patterns library.
#' The names of all already available built-in patterns can be obtained with:
#' \code{list_patterns()}
#'
#' @param name A character string giving the name of the pattern to be added.
#' @param pattern_matrix A binary matrix (containing only 0 and 1) representing the pattern.
#' @param permanent Logical. If `TRUE`, the patterns is added to the library and
#' permanently available for users. Defaults to `FALSE`: the pattern is available for the current session.
#' @param user_path internal
#'
#' @return A character vector of patterns available in the library during the current session.
#' @export
#'
#' @examples
#' # temporary add a pattern built by the user
#' pattern <- matrix(
#' c(1,0,0,
#' 1,1,1,
#' 0,0,1),
#' nrow = 3,
#' byrow = TRUE)
#'
#' add_pattern("my_own_pattern", pattern)
#'
#' # permanently add a pattern extracted from a .rle file
#' \dontrun{
#' cordership <- read_rle("patterns/cordership.rle")
#'
#' add_pattern("cordership", cordership, permanent = TRUE)
#' }
add_pattern <- function(name, pattern_matrix, permanent = FALSE, user_path = NULL) {
# user_path is not documented and is added internally for tests
  if (!is.character(name) || length(name) != 1)
    stop("name must be a single character string.")

  if (!grepl("^[A-Za-z0-9_]+$", name))
    stop("Invalid pattern name: ", name, "\n",
    "name must contain only letters, numbers and underscores.")

  if (exists(name, envir = .patterns_env, inherits = FALSE))
    stop("A pattern with this name already exists.")

  if (!is.matrix(pattern_matrix))
    stop("pattern_matrix must be a matrix.")

  if (!all(pattern_matrix %in% c(0, 1)))
    stop("pattern_matrix must contain only 0 and 1.")

  .patterns_env[[name]] <- pattern_matrix

  if (permanent) {
    # create a data directory in ~/.local/share/R/conwaysGoL/ (Linux), ~/Library/Application Support/R/conwaysGoL/ (macOS), C:/Users/<user>/AppData/Local/R/conwaysGoL/ (Windows)
    path <- user_path %||% file.path(tools::R_user_dir("conwaysGoL", which = "data"), "patterns")
    # %||%: if user_path is not provided, it uses the normal path
    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    # save the object pattern_matrix in the data directory (path) with the name "name.rds"
    saveRDS(pattern_matrix, file.path(path, paste0(name, ".rds")))
  }

  return(ls(.patterns_env))
}

