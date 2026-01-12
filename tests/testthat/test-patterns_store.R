# Test list_pattern()
test_that("list_patterns returns all internal patterns", {
  patterns <- list_patterns()

  # return type
  expect_type(patterns, "character")

  # all listed patterns exist in .patterns_env
  expect_true(all(sapply(patterns, exists, envir = .patterns_env)))
})

# Test add_pattern()
test_that("add_pattern correctly add temporary and permanent pattern", {
  # Temporary
  pattern <- matrix(c(1,1,1,0,0,0,1,1,1), nrow = 3, byrow = TRUE)

  add_pattern("mypattern8", pattern, permanent = FALSE)

  expect_true(exists("mypattern8", envir = .patterns_env))
  expect_equal(.patterns_env$mypattern8, pattern)

  # Permanent
  tmp <- withr::local_tempdir() # create a sandbox disk to simulate data directory

  add_pattern("mypermpat3", pattern, permanent = TRUE, user_path = tmp)

  # check pattern exists in the library
  expect_true(exists("mypermpat3", envir = .patterns_env))

  # check the file .rds has been created
  file <- file.path(tmp, "mypermpat3.rds")
  expect_true(file.exists(file))

  # check that saved pattern is correct
  expect_equal(readRDS(file), pattern)
})

test_that("add_pattern errors to invalid or existing pattern name", {
  pattern <- matrix(c(0,1,0,0,0,1,1,1,1), nrow = 3, byrow = TRUE)

  expect_error(add_pattern("name=1/test", pattern, permanent = FALSE),
               "name must contain only letters, numbers and underscores.")
  expect_error(add_pattern(c("mypattern1", "mypattern2"), pattern, permanent = FALSE),
               "name must be a single character string.")
  expect_error(add_pattern("glider", pattern, permanent = FALSE),
               "A pattern with this name already exists.")
})

test_that("add_pattern errors to invalid matrix", {
  vector <- c(0,1,0,0,0,2,1,1,1)
  matrix <- matrix(c(0,1,0,0,0,1,2,1,1), nrow = 3, byrow = TRUE)

  expect_error(add_pattern("mypattern", vector, permanent = FALSE),
               "pattern_matrix must be a matrix.")
  expect_error(add_pattern("mypattern", matrix, permanent = FALSE),
               "pattern_matrix must contain only 0 and 1.")
})
