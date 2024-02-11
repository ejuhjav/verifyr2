
#############################################################################################
# Generic file existence checks
#############################################################################################

test_that("Returns 'File(s) not available; unable to compare' if both of the files do not exist", {
  file1 <- testthat::test_path("test_outputs/txt/nonexisting1.txt")
  file2 <- testthat::test_path("test_outputs/txt/nonexisting2.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_details(comparator)

  expect_equal(result, "File(s) not available; unable to compare")
})

test_that("Returns 'File(s) not available; unable to compare' if one file does not exist", {
  file1 <- testthat::test_path("test_outputs/txt/base.txt")
  file2 <- testthat::test_path("test_outputs/txt/nonexisting.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_details(comparator)

  expect_equal(result, "File(s) not available; unable to compare")
})

#############################################################################################
# Simple tests that a S4 object is received for details comparison
#############################################################################################

test_that("Returns S4 comparison result object for two files with same content", {
  file1 <- testthat::test_path("test_outputs/txt/base.txt")
  file2 <- testthat::test_path("test_outputs/txt/base_copy.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_details(comparator)

  expect_equal(typeof(result), "S4")
})

test_that("Returns S4 comparison result object for two files with differences in content", {
  file1 <- testthat::test_path("test_outputs/txt/base.txt")
  file2 <- testthat::test_path("test_outputs/txt/base_changes_one_row.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_details(comparator)

  expect_equal(typeof(result), "S4")
})
