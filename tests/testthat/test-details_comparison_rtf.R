
base <- "test_outputs/rtf"

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.'",
  "if both of the files do not exist (raw mode)"
), {
  file1 <- testthat::test_path(base, "nonexisting1.rtf")
  file2 <- testthat::test_path(base, "nonexisting2.rtf")

  config <- Config$new(FALSE)
  config$set("rtf.mode", "raw")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(options = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare.'",
  "if one file does not exist (content mode)"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "nonexisting.rtf")

  config <- Config$new(FALSE)
  config$set("rtf.mode", "content")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(options = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

################################################################################
# Simple tests that a S4 object is received for details comparison
################################################################################

test_that(paste(
  "Returns S4 comparison object for two files with same content",
  "(content mode)"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "copy.rtf")

  config <- Config$new(FALSE)
  config$set("rtf.mode", "content")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(options = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content",
  "(content mode)"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "changes_one_row_content.rtf")

  config <- Config$new(FALSE)
  config$set("rtf.mode", "content")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(options = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content",
  "and in embedded images (content mode)"
), {
  file1 <- testthat::test_path(base, "base_with_image.rtf")
  file2 <- testthat::test_path(base, "changes_one_row_content_one_image.rtf")

  config <- Config$new(FALSE)
  config$set("rtf.mode", "content")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(options = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content",
  "(content mode and images disabled)"
), {
  file1 <- testthat::test_path(base, "base_with_image.rtf")
  file2 <- testthat::test_path(base, "changes_one_row_content_one_image.rtf")

  config <- Config$new(FALSE)
  config$set("rtf.mode", "content")
  config$set("rtf.images", "no")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(options = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in footer",
  "(raw mode)"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "changes_one_row_content.rtf")

  config <- Config$new(FALSE)
  config$set("rtf.mode", "raw")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(options = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")
})
