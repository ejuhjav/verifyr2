
base   <- "test_outputs/img"
config <- Config$new(FALSE)

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.'",
  "if both files do not exist"
), {
  file1 <- testthat::test_path(base, "nonexisting1.jpg")
  file2 <- testthat::test_path(base, "nonexisting2.jpg")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(config = config)

  expect_equal(result, "File(s) not available; unable to compare.")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare.'",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.jpeg")
  file2 <- testthat::test_path(base, "nonexisting.jpeg")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(config = config)

  expect_equal(result, "File(s) not available; unable to compare.")
})

################################################################################
# comparison
################################################################################

test_that(paste(
  "Returns 'No differences.' for identical files"
), {
  file1 <- testthat::test_path(base, "base.png")
  file2 <- testthat::test_path(base, "copy.png")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(config = config)

  if (requireNamespace("magick", quietly = TRUE)) {
    expect_equal(result, "No differences.")
  } else {
    expect_equal(result, paste(
      "No differences.",
      "Image details comparison disabled."
    ))
  }
})

test_that(paste(
  "Returns 'Different file sizes for compared files.'"
), {
  file1 <- testthat::test_path(base, "base.jpg")
  file2 <- testthat::test_path(base, "modified1.jpg")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(config = config)

  if (requireNamespace("magick", quietly = TRUE)) {
    expect_equal(result, "Different file sizes for compared files.")
  } else {
    expect_equal(result, paste(
      "Different file sizes for compared files.",
      "Image details comparison disabled."
    ))
  }
})

################################################################################
# comparison - with magick library not available
################################################################################

test_that(paste(
  "Returns 'Different file sizes for compared files.",
  "Image details comparison disabled.'"
), {
  file1 <- testthat::test_path(base, "base.jpg")
  file2 <- testthat::test_path(base, "modified1.jpg")

  # mock the magick available method to return false to replicate situation
  # that magick library is not installed.
  local_mocked_bindings(
    check_magick_available = function() FALSE
  )

  config_local <- Config$new(FALSE)
  comparator   <- create_comparator(file1, file2)
  result       <- comparator$vrf_summary(config = config_local)

  expect_equal(result, paste(
    "Different file sizes for compared files.",
    "Image details comparison disabled."
  ))
})
