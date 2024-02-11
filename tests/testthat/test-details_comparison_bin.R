
#############################################################################################
# Generic file existence checks
#############################################################################################

test_that("Returns 'File(s) not available; unable to compare' if both of the files do not exist", {
  file1 <- testthat::test_path("test_outputs/bin/nonexisting1.bin")
  file2 <- testthat::test_path("test_outputs/bin/nonexisting2.bin")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_details(comparator)

  expect_equal(result, "File(s) not available; unable to compare")
})

test_that("Returns 'File(s) not available; unable to compare' if one file does not exist", {
  file1 <- testthat::test_path("test_outputs/bin/base.bin")
  file2 <- testthat::test_path("test_outputs/bin/nonexisting.bin")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_details(comparator)

  expect_equal(result, "File(s) not available; unable to compare")
})

#############################################################################################
# Binary files don't support details comparison
#############################################################################################

test_that("Returns 'Binary file without applicable comparator; unable to compare details' for binary files details comparison", {
  file1 <- testthat::test_path("test_outputs/bin/base.bin")
  file2 <- testthat::test_path("test_outputs/bin/base_modified.bin")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_details(comparator)

  expect_equal(result, "Binary file without applicable comparator; unable to compare details")
})
