# Test .place_pattern()
test_that(".place_pattern pastes pattern at correct position", {
  grid <- matrix(0, nrow=10, ncol=15)
  pattern <- matrix(1, nrow=2, ncol=5)

  res <- .place_pattern(grid = grid, pattern = pattern, pos.row = 2, pos.col = 5)

  expected <- grid
  expected[2:3, 5:9] <- pattern

  expect_equal(res, expected)
})

test_that(".place_pattern errors when pattern exceed grid size", {
  grid <- matrix(0, nrow=10, ncol=15)
  pattern <- matrix(1, nrow=2, ncol=25)

  expect_error(
    .place_pattern(grid, pattern, 1, 1),
    "pattern is bigger than grid size"
  )
})

test_that(".place_pattern works at grid's edge", {
  grid <- matrix(0, nrow=10, ncol=15)
  pattern <- matrix(1, nrow=2, ncol=5)

  res <- .place_pattern(grid = grid, pattern = pattern, pos.row = 9, pos.col = 11)

  expected <- grid
  expected[9:10, 11:15] <- pattern

  expect_equal(res, expected)
})

# Test .rotate_pattern()
test_that(".rotate_pattern rotates pattern by 90° correctly", {
  pattern <- matrix(
    c(0,0,0,0,1,0,
      0,1,1,1,1,1,
      0,0,0,0,1,0),
    nrow=3, byrow=TRUE
  )

  res <- .rotate_pattern(pattern, rotation = 90)

  expected <- matrix(
    c(0,0,0,
      0,1,0,
      0,1,0,
      0,1,0,
      1,1,1,
      0,1,0),
    nrow=6, byrow=TRUE
  )

  expect_equal(res, expected)
})

test_that(".rotate_pattern rotates pattern by 180° correctly", {
  pattern <- matrix(
    c(0,0,0,0,1,0,
      0,1,1,1,1,1,
      0,0,0,0,1,0),
    nrow=3, byrow=TRUE
  )

  res <- .rotate_pattern(pattern, rotation = 180)

  expected <- matrix(
    c(0,1,0,0,0,0,
      1,1,1,1,1,0,
      0,1,0,0,0,0),
    nrow=3, byrow=TRUE
  )

  expect_equal(res, expected)
})

test_that(".rotate_pattern rotates pattern by 270° correctly", {
  pattern <- matrix(
    c(0,0,0,0,1,0,
      0,1,1,1,1,1,
      0,0,0,0,1,0),
    nrow=3, byrow=TRUE
  )

  res <- .rotate_pattern(pattern, rotation = 270)

  expected <- matrix(
    c(0,1,0,
      1,1,1,
      0,1,0,
      0,1,0,
      0,1,0,
      0,0,0),
    nrow=6, byrow=TRUE
  )

  expect_equal(res, expected)
})

test_that(".rotate_pattern does not rotate pattern when rotation = 0", {
  pattern <- matrix(
    c(0,0,0,0,1,0,
      0,1,1,1,1,1,
      0,0,0,0,1,0),
    nrow=3, byrow=TRUE
  )

  res <- .rotate_pattern(pattern, rotation = 0)

  expected <- matrix(
    c(0,0,0,0,1,0,
      0,1,1,1,1,1,
      0,0,0,0,1,0),
    nrow=3, byrow=TRUE
  )

  expect_equal(res, expected)
})

test_that(".rotate_pattern errors when rotation is not one of 0, 90, 180, 270", {
  pattern <- matrix(
    c(0,0,0,0,1,0,
      0,1,1,1,1,1,
      0,0,0,0,1,0),
    nrow=3, byrow=TRUE
  )

  expect_error(
    .rotate_pattern(pattern, rotation = 45),
    "rotation must be one of 0, 90, 180, 270"
  )
})

# test .place_patterns()
test_that(".place_patterns pastes patterns correctly", {
  grid <- matrix(0, nrow = 12, ncol = 15)

  pattern1 <- matrix(
    c(0,0,1,0,0,0,0,1,0,0,
      1,1,0,1,1,1,1,0,1,1,
      0,0,1,0,0,0,0,1,0,0),
    nrow = 3,
    byrow = TRUE
  )

  pattern2 <- matrix(
    c(0,1,0,
      0,0,1,
      1,1,1),
    nrow = 3,
    byrow = TRUE
  )

  patterns <- list(
    pentadecathlon = pattern1,
    glider = pattern2
  )

  res <- .place_patterns(grid = grid, pattern = c("pentadecathlon", "glider"), pos.row = c(1, 7), pos.col = c(3, 2), rotation = 0)

  expected <- grid
  expected[1:3, 3:12] <- pattern1
  expected[7:9, 2:4] <- pattern2

  expect_equal(res, expected)
})

test_that(".place_patterns errors when pattern, pos.row and pos.col have different length", {
  grid <- matrix(0, nrow = 12, ncol = 15)

  pattern1 <- matrix(
    c(0,0,1,0,0,0,0,1,0,0,
      1,1,0,1,1,1,1,0,1,1,
      0,0,1,0,0,0,0,1,0,0),
    nrow = 3,
    byrow = TRUE
  )

  pattern2 <- matrix(
    c(0,1,0,
      0,0,1,
      1,1,1),
    nrow = 3,
    byrow = TRUE
  )

  patterns <- list(
    pentadecathlon = pattern1,
    glider = pattern2
  )

  expect_error(
    .place_patterns(grid = grid, pattern = c("pentadecathlon", "glider"), pos.row = 1, pos.col = c(3, 2), rotation = 0),
    "pattern, pos.row, pos.col must have the same length")
})

test_that(".place_patterns rotates patterns differently if length(rotation) > 1", {
  grid <- matrix(0, nrow = 12, ncol = 15)

  pattern1 <- matrix(
    c(0,0,1,0,0,0,0,1,0,0,
      1,1,0,1,1,1,1,0,1,1,
      0,0,1,0,0,0,0,1,0,0),
    nrow = 3,
    byrow = TRUE
  )

  pattern2 <- matrix(
    c(0,1,0,
      0,0,1,
      1,1,1),
    nrow = 3,
    byrow = TRUE
  )

  patterns <- list(
    pentadecathlon = pattern1,
    glider = pattern2
  )

  res <- .place_patterns(grid = grid, pattern = c("pentadecathlon", "glider"), pos.row = c(1, 7), pos.col = c(3, 2), rotation = c(90, 180))

  rot_pattern1 <- matrix(
    c(0,1,0,
      0,1,0,
      1,0,1,
      0,1,0,
      0,1,0,
      0,1,0,
      0,1,0,
      1,0,1,
      0,1,0,
      0,1,0),
    nrow = 10,
    byrow = TRUE
  )

  rot_pattern2 <- matrix(
    c(1,1,1,
      1,0,0,
      0,1,0),
    nrow = 3,
    byrow = TRUE
  )
  expected <- grid
  expected[1:10, 3:5] <- rot_pattern1
  expected[7:9, 2:4] <- rot_pattern2

  expect_equal(res, expected)
})

test_that(".place_patterns errors if pattern name does not belong to patterns library", {
  grid <- matrix(0, nrow = 12, ncol = 15)

  expect_error(
    .place_patterns(grid = grid, pattern = "unknown", pos.row = 1, pos.col = 3, rotation = 0))
})
