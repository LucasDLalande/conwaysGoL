# Test read_rle()
test_that("read_rle correctly convert a RLE code into a binary (0,1) matrix", {
  tmp <- tempfile(fileext = ".rle")
  writeLines(c(
    "x = 3, y = 3",
    "bo$2bo$3o!"
  ), tmp)

  res <- read_rle(tmp)

  expected <- matrix(c(0,1,0,
                       0,0,1,
                       1,1,1),
                     nrow = 3,
                     byrow = TRUE)

  expect_equal(res, expected)
})

test_that("read_rle ignores comments in RLE file", {
  tmp <- tempfile(fileext = ".rle")
  writeLines(c(
    "# first line of comment",
    "#another comment",
    "x = 3, y = 3",
    "bo$2bo$3o!"
  ), tmp)

  res <- read_rle(tmp)

  expected <- matrix(c(0,1,0,
                       0,0,1,
                       1,1,1),
                     nrow = 3,
                     byrow = TRUE)

  expect_equal(res, expected)
})

test_that("read_rle errors when x/y header is missing", {
  tmp <- tempfile(fileext = ".rle")
  writeLines(c(
    "bo$2bo$3o!"
  ), tmp)

  expect_error(read_rle(tmp),
               "Invalid RLE file: cannot find unique x/y header line")
})

test_that("read_rle errors when x/y header is duplicated", {
  tmp <- tempfile(fileext = ".rle")
  writeLines(c(
    "x = 3, y = 3",
    "x = 4, y = 2",
    "bo$2bo$3o!"
  ), tmp)

  expect_error(read_rle(tmp),
               "Invalid RLE file: cannot find unique x/y header line")
})

test_that("read_rle errors when end marker '!' is absent", {
  tmp <- tempfile(fileext = ".rle")
  writeLines(c(
    "x = 3, y = 3",
    "bo$2bo$3o"
  ), tmp)

  expect_error(read_rle(tmp),
               "Invalid RLE file: missing '!' end-of-code marker")
})

test_that("read_rle errors if filling exceeds grid columns", {
  tmp <- tempfile(fileext = ".rle")
  writeLines(c(
    "x = 2, y = 1",
    "3o!"
  ), tmp)

  expect_error(read_rle(tmp),
               "Filling exceed grid column number")
})

test_that("read_rle errors if filling exceeds grid rows", {
  tmp <- tempfile(fileext = ".rle")
  writeLines(c(
    "x = 1, y = 1",
    "b$b!"
  ), tmp)

  expect_error(read_rle(tmp),
               "Filling exceed grid row number")
})
