
base <- "test_outputs/img"
config <- Config$new(FALSE)

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.'",
  "if both of the files do not exist"
), {
  file1 <- testthat::test_path(base, "nonexisting1.jpg")
  file2 <- testthat::test_path(base, "nonexisting2.jpg")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)
  expect_length(result, 1)

  txt_result = result[[1]]
  expect_equal(txt_result$type, "text")
  expect_equal(txt_result$contents, "File(s) not available; unable to compare.")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare.'",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.png")
  file2 <- testthat::test_path(base, "nonexisting.png")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)
  expect_length(result, 1)

  txt_result = result[[1]]
  expect_equal(txt_result$type, "text")
  expect_equal(txt_result$contents, "File(s) not available; unable to compare.")
})

################################################################################
# Simple tests that a S4 object is received for details comparison
################################################################################

test_that(paste(
  "Returns S4 comparison object for two files with same file size"
), {
  file1 <- testthat::test_path(base, "base.jpeg")
  file2 <- testthat::test_path(base, "base.jpeg")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)
  expect_length(result, 1)

  img_result = result[[1]]
  expect_equal(img_result$type, "image")
  expect_equal(typeof(img_result$contents), "list")
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in file size"
), {
  file1 <- testthat::test_path(base, "base.png")
  file2 <- testthat::test_path(base, "modified1.png")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)
  expect_length(result, 1)

  img_result = result[[1]]
  expect_equal(img_result$type, "image")
  expect_equal(typeof(img_result$contents), "list")
})

################################################################################
# comparison - with magick library not available
################################################################################

test_that(paste(
  "Returns 'Image details comparison disabled.'"
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
  result       <- comparator$vrf_details(config = config_local)
  expect_length(result, 1)

  img_result = result[[1]]
  expect_equal(img_result$type, "text")
  expect_equal(img_result$contents, "Image details comparison disabled.")
})
