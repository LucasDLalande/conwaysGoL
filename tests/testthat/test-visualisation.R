# Test .save_frames()
test_that(".save_frames creates a PNG file with correct name", {
  tmp <- tempdir()

  grid <- matrix(
    c(0,1,0,
      1,0,1,
      0,1,0),
    nrow = 3,
    byrow = TRUE
  )

  .save_frames(
    grid = grid,
    frames_path = tmp,
    frames_prefix = "frame",
    zero_padding = 4,
    frame_index = 12
  )

  expected_file <- file.path(tmp, "frame_0012.png")

  expect_true(file.exists(expected_file))
})

test_that(".save_frames errors when gris is not a matrix", {
  tmp <- tempdir()

  grid <- c(1,0,0,0,1)

  expect_error(
    .save_frames(grid, frames_path = tmp, frames_prefix = "frame", zero_padding = 4, frame_index = 12),
    "grid must be a matrix"
  )
})

test_that(".save_frames creates directory if it does not exist", {
  tmp <- file.path(tempdir(), "frames_test_dir")

  unlink(tmp, recursive = TRUE)

  .save_frames(
    grid = matrix(0, 3, 3),
    frames_path = tmp,
    frame_index = 1
  )

  expect_true(dir.exists(tmp))
})

# Test .make_gif()
test_that(".make_gif generates a GIF file with correct name and location", {
  frames_dir <- withr::local_tempdir("frames") # create a temp directory for the test
  output_dir <- withr::local_tempdir("output")

  # Create fake PNG frames
  for (i in 1:3) {
    png(file.path(frames_dir, sprintf("frame_%05d.png", i)))
    plot.new()
    dev.off()
  }

  gif_path <- .make_gif(
    frames_path = frames_dir,
    frames_prefix = "frame",
    gif_name = "test.gif",
    fps = 5,
    output_path = output_dir,
    zero_padding = 5
  )

  expect_true(file.exists(gif_path))
  expect_equal(gif_path, file.path(output_dir, "test.gif"))
})

# "frames_path does not exist"
test_that(".make_gif errors when frames path does not exist", {
  expect_error(
    .make_gif(
      frames_path = "path/does/not/exist/",
      frames_prefix = "frame",
      gif_name = "test.gif",
      fps = 5,
      output_path = output_dir,
      zero_padding = 5),
    "frames_path does not exist"
  )
})

# create dir if does not exist
test_that(".make_gif creates output directory if does not exist", {
  frames_dir <- withr::local_tempdir("frames") # create a temp directory for the test

  output_dir <- file.path(tempdir(), "gif_output_test")

  unlink(output_dir, recursive = TRUE)

  # Create fake PNG frames
  for (i in 1:3) {
    png(file.path(frames_dir, sprintf("frame_%05d.png", i)))
    plot.new()
    dev.off()
  }

  gif_path <- .make_gif(
    frames_path = frames_dir,
    frames_prefix = "frame",
    gif_name = "test.gif",
    fps = 5,
    output_path = output_dir,
    zero_padding = 5
  )

  expect_true(dir.exists(output_dir))
})

# "No frames matching the expected name pattern were found"
test_that(".make_gif errors when frames name does not match", {
  frames_dir <- withr::local_tempdir("frames") # create a temp directory for the test
  output_dir <- withr::local_tempdir("output")

  # Create fake PNG frames
  for (i in 1:3) {
    png(file.path(frames_dir, sprintf("frame_%05d.png", i)))
    plot.new()
    dev.off()
  }

  expect_error(
    .make_gif(
      frames_path = frames_dir,
      frames_prefix = "wrongname",
      gif_name = "test.gif",
      fps = 5,
      output_path = output_dir,
      zero_padding = 5),
    "No frames matching the expected name pattern were found"
  )
})

# test plot_grid()
test_that("plot_grid correctly plots a single matrix", {
  grid <- matrix(
    c(0,1,0,
      0,0,1,
      1,1,1),
    nrow = 3,
    byrow = TRUE
  )

  expect_silent(plot_grid(grid))
})

test_that("plot_grid correctly plots a built-in pattern", {
  pat <- list_patterns()[3]
  expect_silent(plot_grid(pat))
})

test_that("plot_grid errors when invalid input", {
  expect_error(plot_grid(42),
               "grid must be a matrix, a pattern name or a list of those")
})

test_that("plot_grid errors on unknown pattern name", {
  expect_error(plot_grid("wrong_name"),
               "Unknown pattern")
})

test_that("plot_grid errors when non-binary matrix", {
  grid <- matrix(
    c(0,1,0,
      0,0,2,
      1,1,1),
    nrow = 3,
    byrow = TRUE)

  expect_error(plot_grid(grid),
               "grid must be a binary matrix containing only 0 and 1")
})

test_that("plot_grid errors if list contains invalid elements", {
  grid <- matrix(
    c(0,1,0,
      0,0,1,
      1,1,1),
    nrow = 3,
    byrow = TRUE)

  expect_error(plot_grid(list(grid, 123)),
               "Each element must be a matrix or a pattern name")
})
