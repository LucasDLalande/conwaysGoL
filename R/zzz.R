# Internal environment for patterns
.patterns_env <- new.env(parent = emptyenv())

#' Loading user patterns
#'
#' Loads user patterns from the user directory and stores it to the internal environment .patterns_env
#'
#' @noRd
.load_user_patterns <- function(user_path) {
  if (dir.exists(user_path)) {
    files <- list.files(user_path, pattern="\\.rds$", full.names = TRUE) # full.names = TRUE returns full path of the file. '\\.' = '.', '$' string end
    for (f in files) {
      nm <- tools::file_path_sans_ext(basename(f)) # basename() extract only the file name from the full path. file_path_sans_ext() remove the extension from the file
      .patterns_env[[nm]] <- readRDS(f)
    }
  }
  invisible(NULL)
}

#' Loading patterns library into the internal environments
#'
#' Creates an internal environment copy of the library patterns
#'
#' @importFrom stats rbinom
#'
#' @noRd
.onLoad <- function(libname, pkgname) {

  # Load internal patterns from sysdata.rda in the NAMESPACE
  patterns <- get("patterns", envir = asNamespace(pkgname))

  # Copy each pattern into the internal environment
  for (nm in names(patterns)) {
    .patterns_env[[nm]] <- patterns[[nm]]
  }

  # Check whether user patterns have been saved to user directory and add them to .patterns_env
  user_path <- file.path(tools::R_user_dir("conwaysGoL", which = "data"), "patterns")
  .load_user_patterns(user_path)

  # Ensure 'random' is always a function
  if (!"random" %in% names(.patterns_env)) {
    .patterns_env$random <- function(nrow, ncol, p) {
      matrix(rbinom(nrow*ncol, 1, p), nrow = nrow, ncol = ncol)
    }
  }
}
