
#############################################################################################
# Generic file existence checks
#############################################################################################

test_that("Returns 'File(s) not available; unable to compare' if both files do not exist", {
  file1 <- testthat::test_path("test_outputs/pdf/nonexisting1.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/nonexisting2.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "File(s) not available; unable to compare")
})

test_that("Returns 'File(s) not available; unable to compare' if one file does not exist", {
  file1 <- testthat::test_path("test_outputs/pdf/base.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/nonexisting.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "File(s) not available; unable to compare")
})

#############################################################################################
# Text file comparison - WITHOUT omit
#############################################################################################

test_that("Returns 'No differences' for identical files (one page)", {
  file1 <- testthat::test_path("test_outputs/pdf/base.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_copy.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "No differences")
})

test_that("Returns 'No differences' for identical files (two pages)", {
  file1 <- testthat::test_path("test_outputs/pdf/base_two_pages.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_two_pages_copy.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "No differences")
})

test_that("Returns 'Different number of lines in compared content' for files with additional line in the content (one page)", {
  file1 <- testthat::test_path("test_outputs/pdf/base.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_addition_one_row.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "Different number of lines in compared content")
})

test_that("Returns 'Different number of lines in compared content' for files with additional line in the content (two pages)", {
  file1 <- testthat::test_path("test_outputs/pdf/base_two_pages.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_two_pages_addition_one_row.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "Different number of lines in compared content")
})

test_that("Returns 'File content has changes in 1 place(s)' for files with a single different row in content (one page)", {
  file1 <- testthat::test_path("test_outputs/pdf/base_changes_one_row.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "File content has changes in 1 place(s)")
})

test_that("Returns 'File content has changes in 1 place(s)' for files with a single different row in content (two pages)", {
  file1 <- testthat::test_path("test_outputs/pdf/base_two_pages_changes_one_row.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_two_pages.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "File content has changes in 1 place(s)")
})

test_that("Returns 'File content has changes in 2 place(s)' for files with two different rows in content (two pages)", {
  file1 <- testthat::test_path("test_outputs/pdf/base_two_pages_changes_two_rows.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_two_pages.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "File content has changes in 2 place(s)")
})

#############################################################################################
# Text file comparison - WITH omit
#############################################################################################

test_that("Returns 'No differences' for comparison with 'omit' parameter that is not present in either file (one page)", {
  file1 <- testthat::test_path("test_outputs/pdf/base.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_copy.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Nothing")

  expect_equal(result, "No differences")
})

test_that("Returns 'No differences' for comparison with 'omit' parameter that is not present in either file (two pages)", {
  file1 <- testthat::test_path("test_outputs/pdf/base_two_pages.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_two_pages_copy.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Nothing")

  expect_equal(result, "No differences")
})

test_that("No differences' when called with 'omit' parameter catching the one row changed row in content (one page)", {
  file1 <- testthat::test_path("test_outputs/pdf/base.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_changes_one_row.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "simple example pdf")

  expect_equal(result, "No differences")
})

test_that("No differences' when called with 'omit' parameter catching the one row changed row in content (two pages)", {
  file1 <- testthat::test_path("test_outputs/pdf/base_two_pages.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_two_pages_changes_one_row.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "two paged example pdf")

  expect_equal(result, "No differences")
})

test_that("Returns 'No differences' when called with 'omit' parameter catching the one additional row in content (one page)", {
  file1 <- testthat::test_path("test_outputs/pdf/base.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_addition_one_row.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "one additional row")

  expect_equal(result, "No differences")
})

test_that("Returns 'No differences' when called with 'omit' parameter catching the one additional row in content (two pages)", {
  file1 <- testthat::test_path("test_outputs/pdf/base_two_pages.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_two_pages_addition_one_row.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "additional row")

  expect_equal(result, "No differences")
})

test_that("Returns 'No differences' when called with 'omit' parameter catching the two additional rows in content (one page)", {
  file1 <- testthat::test_path("test_outputs/pdf/base.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_addition_two_rows.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "additional row", options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'No differences' when called with 'omit' parameter catching the two additional rows in content (two pages)", {
  file1 <- testthat::test_path("test_outputs/pdf/base_two_pages.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_two_pages_addition_two_rows.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "additional row", options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'Different number of lines in compared' when called with 'omit' parameter catching one of the two additional rows (one page)", {
  file1 <- testthat::test_path("test_outputs/pdf/base.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_addition_two_rows.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "one additional row")

  expect_equal(result, "Different number of lines in compared content")
})

test_that("Returns 'Different number of lines in compared' when called with 'omit' parameter catching one of the two additional rows (two pages)", {
  file1 <- testthat::test_path("test_outputs/pdf/base_two_pages.pdf")
  file2 <- testthat::test_path("test_outputs/pdf/base_two_pages_addition_two_rows.pdf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "one additional row")

  expect_equal(result, "Different number of lines in compared content")
})
