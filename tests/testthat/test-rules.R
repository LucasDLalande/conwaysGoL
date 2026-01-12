# Test .count_neighbors()
test_that(".count_neighbors correctly assesses the number of living cells", {
  grid <- matrix(
    c(0,1,0,1,0,
      0,1,1,1,0,
      1,0,0,0,1,
      1,1,1,0,0),
    nrow = 4,
    byrow = TRUE
  )

  res <- .count_neighbors(grid = grid, i = 3, j = 4)
  expected <- 4

  expect_equal(res, expected)
})

test_that(".count_neighbors correctly assesses the number of living cells when on a grid edge", {
  grid <- matrix(
    c(0,1,0,1,0,
      0,1,1,1,0,
      1,0,0,0,1,
      1,1,1,0,0),
    nrow = 4,
    byrow = TRUE
  )

  res <- .count_neighbors(grid = grid, i = 3, j = 5)
  expected <- 3

  expect_equal(res, expected)
})

test_that(".count_neighbors errors when cell coordinates exceed grid dimensions", {
  grid <- matrix(
    c(0,1,0,1,0,
      0,1,1,1,0,
      1,0,0,0,1,
      1,1,1,0,0),
    nrow = 4,
    byrow = TRUE
  )

  expect_error(
    .count_neighbors(grid = grid, i = 5, j = 3),
    "coordinates exceed of grid dimensions"
  )
})

test_that(".count_neighbors errors when grid contains something else than 0 and 1", {
  grid <- matrix(
    c(0,1,0,1,0,
      0,1,1,-1,0,
      1,0.4,0,0,1,
      1,1,2,0,0),
    nrow = 4,
    byrow = TRUE
  )

  expect_error(
    .count_neighbors(grid, 3, 5),
    "grid must be a binary matrix containing only 0 and 1"
  )
})

# Test .next_generation()
test_that(".next_generation generates the next generation grid correctly", {
  grid <- matrix(
    c(0,0,0,0,0,
      0,1,1,1,0,
      0,0,0,0,0),
    nrow = 3,
    byrow = TRUE
  )

  res <- .next_generation(grid = grid)
  expected <- matrix(
    c(0,0,1,0,0,
      0,0,1,0,0,
      0,0,1,0,0),
    nrow = 3,
    byrow = TRUE
  )

  expect_equal(res, expected)
})

test_that(".next_generation kills a living cell if less than two neighbors", {
  grid <- matrix(
    c(0,0,0,
      0,1,1,
      0,0,0),
    nrow = 3,
    byrow = TRUE
  )

  res <- .next_generation(grid = grid)

  expect_equal(res[2,2], 0)
})

test_that(".next_generation creates a living cell if exactly three neighbors", {
  grid <- matrix(
    c(0,0,1,0,0,
      0,0,1,0,0,
      0,0,1,0,0),
    nrow = 3,
    byrow = TRUE
  )

  res <- .next_generation(grid = grid)

  expect_equal(res[2,4], 1)
})

test_that(".next_generation kills a living cell if more than three neighbors", {
  grid <- matrix(
    c(0,0,0,0,0,
      0,1,1,1,0,
      0,1,1,1,0,
      0,1,0,1,0),
    nrow = 4,
    byrow = TRUE
  )

  res <- .next_generation(grid = grid)

  expect_equal(res[3,3], 0)
})

test_that(".next_generation maintains still-life structures", {
  grid <- matrix(
    c(0,0,0,0,0,
      0,1,1,0,0,
      0,1,1,0,0,
      0,0,0,0,0),
    nrow = 4,
    byrow = TRUE
  )

  res <- .next_generation(grid = grid)

  expected <- matrix(
    c(0,0,0,0,0,
      0,1,1,0,0,
      0,1,1,0,0,
      0,0,0,0,0),
    nrow = 4,
    byrow = TRUE
  )

  expect_equal(res, expected)
})
