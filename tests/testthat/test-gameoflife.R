# test gameoflife()
test_that("gameoflife runs silently with valid input", {
  expect_silent(gameoflife(10, 10, 2, p=0.3)) # random grid
  expect_silent(gameoflife(100, 100, 2, pattern="glider", pos.row=1, pos.col=10))
  expect_silent(gameoflife(100, 100, 2, pattern=c("glider", "pulsar"), pos.row=c(1, 50), pos.col=c(1, 50), rotation=90))
})

test_that("gameoflife errors when invalid input for p, nrow, ncol or generations", {
  expect_error(expect_silent(gameoflife(100, 100, 2, pattern="glider", pos.row=-10, pos.col=50, rotation=90)))
  expect_error(expect_silent(gameoflife(100, 100, 2, pattern="glider", pos.row=10, pos.col=-50, rotation=90)))
  expect_error(expect_silent(gameoflife(100, 100, -2, pattern="glider", pos.row=10, pos.col=50, rotation=90)))
  expect_error(gameoflife(10, 10, 2, p=1.3)) # random grid
  expect_error(gameoflife("10", 10, 2, p=1.3)) # random grid
})

test_that("gameoflife errors when invalid pattern names", {
  expect_error(expect_silent(gameoflife(100, 100, 2, pattern="glidennr", pos.row=10, pos.col=50, rotation=90)))
})

test_that("gameoflife requires gif_name when recording", {
  expect_error(gameoflife(10, 10, 1, record = TRUE))
  expect_silent(gameoflife(10, 10, 1, record = TRUE, gif_name = "test.gif"))
})
