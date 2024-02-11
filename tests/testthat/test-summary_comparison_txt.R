
#############################################################################################
# Generic file existence checks
#############################################################################################

test_that("Returns 'File(s) not available; unable to compare' if both files do not exist", {
  file1 <- testthat::test_path("test_outputs/txt/nonexisting1.txt")
  file2 <- testthat::test_path("test_outputs/txt/nonexisting2.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "File(s) not available; unable to compare")
})

test_that("Returns 'File(s) not available; unable to compare' if one file does not exist", {
  file1 <- testthat::test_path("test_outputs/txt/base.txt")
  file2 <- testthat::test_path("test_outputs/txt/nonexisting.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "File(s) not available; unable to compare")
})

#############################################################################################
# Text file comparison - WITHOUT omit
#############################################################################################

test_that("Returns 'No differences' for identical files", {
  file1 <- testthat::test_path("test_outputs/txt/base.txt")
  file2 <- testthat::test_path("test_outputs/txt/base_copy.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "No differences")
})

test_that("Returns 'Different number of lines in compared content' for files with additional line in the content", {
  file1 <- testthat::test_path("test_outputs/txt/base.txt")
  file2 <- testthat::test_path("test_outputs/txt/base_addition_one_row.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "Different number of lines in compared content")
})

test_that("Returns 'File content has changes in 1 place(s)' for files with a single different row in content", {
  file1 <- testthat::test_path("test_outputs/txt/base_changes_one_row.txt")
  file2 <- testthat::test_path("test_outputs/txt/base.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "File content has changes in 1 place(s)")
})

test_that("Returns 'File content has changes in 2 place(s)' for files with two different rows in content", {
  file1 <- testthat::test_path("test_outputs/txt/base_changes_two_rows.txt")
  file2 <- testthat::test_path("test_outputs/txt/base.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "File content has changes in 2 place(s)")
})

#############################################################################################
# Text file comparison - WITH omit
#############################################################################################

test_that("Returns 'No differences' for comparison with 'omit' parameter that is not present in either file", {
  file1 <- testthat::test_path("test_outputs/txt/base.txt")
  file2 <- testthat::test_path("test_outputs/txt/base_copy.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Nothing")

  expect_equal(result, "No differences")
})

test_that("No differences' when called with 'omit' parameter catching the one row changed row in content", {
  file1 <- testthat::test_path("test_outputs/txt/base.txt")
  file2 <- testthat::test_path("test_outputs/txt/base_changes_one_row.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Line 2")

  expect_equal(result, "No differences")
})

test_that("Returns 'No differences' when called with 'omit' parameter catching the one additional row in content", {
  file1 <- testthat::test_path("test_outputs/txt/base.txt")
  file2 <- testthat::test_path("test_outputs/txt/base_addition_one_row.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Line 4")

  expect_equal(result, "No differences")
})

test_that("Returns 'No differences' when called with 'omit' parameter catching the two additional rows in content", {
  file1 <- testthat::test_path("test_outputs/txt/base.txt")
  file2 <- testthat::test_path("test_outputs/txt/base_addition_two_rows.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Line 4", options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'Different number of lines in compared' when called with 'omit' parameter catching one of the two additional rows", {
  file1 <- testthat::test_path("test_outputs/txt/base.txt")
  file2 <- testthat::test_path("test_outputs/txt/base_addition_two_rows.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Line 41")

  expect_equal(result, "Different number of lines in compared content")
})

test_that("Returns 'File content has changes in 1 place(s)' when called with 'omit' parameter catching one of the two changed rows in content", {
  file1 <- testthat::test_path("test_outputs/txt/base.txt")
  file2 <- testthat::test_path("test_outputs/txt/base_changes_two_rows.txt")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Line 2")

  expect_equal(result, "File content has changes in 1 place(s)")
})
