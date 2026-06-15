
base   <- "test_outputs/rtf"
config <- Config$new(FALSE)

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.'",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "nonexisting.rtf")

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
  "Returns S4 comparison object for two files with same content"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "copy.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)
  expect_length(result, 1)

  txt_result = result[[1]]
  expect_equal(txt_result$type, "text")
  expect_equal(typeof(txt_result$contents), "S4")
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "changes_one_row_content.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)
  expect_length(result, 1)

  txt_result = result[[1]]
  expect_equal(txt_result$type, "text")
  expect_equal(typeof(txt_result$contents), "S4")
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content",
  "and in embedded images"
), {
  file1 <- testthat::test_path(base, "base_with_image.rtf")
  file2 <- testthat::test_path(base, "changes_one_row_content_one_image.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)
  expect_length(result, 2)

  txt_result = result[[1]]
  img_result = result[[2]]

  expect_equal(txt_result$type, "text")
  expect_equal(typeof(txt_result$contents), "S4")

  expect_equal(img_result$type, "image")
  expect_equal(typeof(img_result$contents), "list")
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content",
  "(images disabled)"
), {
  file1 <- testthat::test_path(base, "base_with_image.rtf")
  file2 <- testthat::test_path(base, "changes_one_row_content_one_image.rtf")

  config_local <- Config$new(FALSE)
  config_local$set("rtf.images", "no")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config_local)
  expect_length(result, 1)

  txt_result = result[[1]]
  expect_equal(txt_result$type, "text")
  expect_equal(typeof(txt_result$contents), "S4")
})

################################################################################
# CONTENT comparison with images - magick library not available
################################################################################

test_that(paste(
  "Returns S4 comparison object for two files with differences in content",
  "with magick library not available"
), {
  file1 <- testthat::test_path(base, "changes_one_row_content_one_image.rtf")
  file2 <- testthat::test_path(base, "base_with_image.rtf")

  # mock the magick available method to return false to replicate situation
  # that magick library is not installed.
  local_mocked_bindings(
    check_magick_available = function() FALSE
  )

  config_local <- Config$new(FALSE)

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config_local)
  expect_length(result, 1)

  txt_result = result[[1]]
  expect_equal(txt_result$type, "text")
  expect_equal(typeof(txt_result$contents), "S4")
})

################################################################################
# Test logging enabled
################################################################################

test_that(paste(
  "Prints expected logs to console when debugging is enabled"
), {
  file1 <- testthat::test_path(base, "base_with_image.rtf")
  file2 <- testthat::test_path(base, "changes_one_row_content_one_image.rtf")

  config_local <- Config$new(FALSE)
  config_local$set("generic.debug", "yes")

  output <- capture.output({
    comparator <- create_comparator(file1, file2)
    result     <- comparator$vrf_details(config = config_local)
  })

  expected <- "'File::vrf_details, mode: summary' (execution time"
  expect_content(expected, output)

  expected <- "'File 1: test_outputs/rtf/base_with_image.rtf'"
  expect_content(expected, output)

  expected <- "'File 2: test_outputs/rtf/changes_one_row_content_one_image.rtf'"
  expect_content(expected, output)

  expected <- "'TxtWithImages::vrf_details_inner' (execution time"
  expect_content(expected, output)
})
