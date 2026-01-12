# Test .onLoad()
test_that(".onLoad correctly loads .patterns_env", {
  # Check that all pattern names exist
  patterns <- ls(.patterns_env)

  expect_true(all(sapply(patterns, exists, envir = .patterns_env)))

  # Check that 'random' exists and is a function
  expect_true(is.function(.patterns_env$random))

  # Check that a known pattern is a matrix of 0/1
  expect_true(is.matrix(.patterns_env$glider))
  expect_true(all(.patterns_env$glider %in% c(0,1)))
})

test_that(".onLoad correctly loads user-defined permanent patterns", {
  tmp <- tempdir()

  pattern <- matrix(c(0,1,0,0,0,1,1,1,1), nrow = 3, byrow = TRUE)
  saveRDS(pattern, file.path(tmp, "mypermpat.rds"))

  # Reset .patterns_env
  rm(list = ls(.patterns_env), envir = .patterns_env)

  # Call .onLoad() with the temp dir
  .load_user_patterns(tmp)

  # Check pattern is loaded
  expect_true("mypermpat" %in% ls(.patterns_env))
  expect_equal(.patterns_env$mypermpat, pattern)

})
