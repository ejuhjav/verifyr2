
#############################################################################################
# Generic file existence checks
#############################################################################################

test_that("Returns 'File(s) not available; unable to compare' if both of the files do not exist (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/nonexisting1.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/nonexisting2.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_details(comparator, options = options)

  expect_equal(result, "File(s) not available; unable to compare")
})

test_that("Returns 'File(s) not available; unable to compare' if one file does not exist (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/nonexisting.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_details(comparator, options = options)

  expect_equal(result, "File(s) not available; unable to compare")
})

#############################################################################################
# Simple tests that a S4 object is received for details comparison
#############################################################################################

test_that("Returns S4 comparison result object for two files with same content (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_copy.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_details(comparator, options = options)

  expect_equal(typeof(result), "S4")
})

test_that("Returns S4 comparison result object for two files with differences in content (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_changes_one_row_content.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_details(comparator, options = options)

  expect_equal(typeof(result), "S4")
})

test_that("Returns S4 comparison result object for two files with differences in footer (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_changes_one_row_content.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_details(comparator, options = options)

  expect_equal(typeof(result), "S4")
})
