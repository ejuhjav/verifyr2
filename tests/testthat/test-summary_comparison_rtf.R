
#############################################################################################
# Generic file existence checks
#############################################################################################

test_that("Returns 'File(s) not available; unable to compare' if both files do not exist (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/nonexisting1.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/nonexisting2.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "File(s) not available; unable to compare")
})

test_that("Returns 'File(s) not available; unable to compare' if one file does not exist (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/nonexisting.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "File(s) not available; unable to compare")
})

#############################################################################################
# RAW comparison - WITHOUT omits
#############################################################################################

test_that("Returns 'No differences' for identical files (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_copy.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'Different number of lines in compared content' for files with additional line in the content (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_one_row_content.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "Different number of lines in compared content")
})

test_that("Returns 'Different number of lines in compared content' for files with additional line in the footer (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_one_row_footer.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "Different number of lines in compared content")
})

test_that("Returns 'File content has changes in 3 place(s)' for files with a single different rtf (3 rows in raw text) row in content (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base_changes_one_row_content.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "File content has changes in 3 place(s)")
})

test_that("Returns 'File content has changes in 1 place(s)' for files with a single different row in footer (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_changes_one_row_footer.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "File content has changes in 1 place(s)")
})

test_that("Returns 'File content has changes in 2 place(s)' for files with two different rows in content (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base_changes_two_rows_content.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = list("rtf" = "raw"))

  expect_equal(result, "File content has changes in 2 place(s)")
})

test_that("Returns 'File content has changes in 2 place(s)' for files with two different rows in footer (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_changes_two_rows_footer.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "File content has changes in 2 place(s)")
})

#############################################################################################
# RAW comparison - WITH omits
#############################################################################################

test_that("Returns 'No differences' for comparison with 'omit' parameter that is not present in either file (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_copy.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Nothing", options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'Different number of lines in compared content' when called with 'omit' parameter catching the row name cell in additional row in content (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_one_row_content.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Additional Row", options = options)

  expect_equal(result, "Different number of lines in compared content")
})


test_that("Returns 'No differences' when called with 'omit' parameter catching the single additional row in footer (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_one_row_footer.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Additional footer row", options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'Different number of lines in compared content' when called with 'omit' parameter catching the two row name cells in additional row in content (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_two_rows_content.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Additional Row", options = options)

  expect_equal(result, "Different number of lines in compared content")
})

test_that("Returns 'Different number of lines in compared content' when called with 'omit' parameter catching one of the two row name cells in additional row in content (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_two_rows_content.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Additional Row 1", options = options)

  expect_equal(result, "Different number of lines in compared content")
})

test_that("Returns 'No differences' when called with 'omit' parameter catching both of the additional rows in footer (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_two_rows_footer.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Additional footer row", options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'Different number of lines in compared content' when called with 'omit' parameter caching one of the two additional rows in footer (raw mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_two_rows_footer.rtf")

  options    <- list("rtf" = list("mode" = "raw"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Additional footer row 1", options = options)

  expect_equal(result, "Different number of lines in compared content")
})

#############################################################################################
# CONTENT comparison & WITHOUT omits
#############################################################################################

test_that("Returns 'No differences' for identical files (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'Different number of lines in compared content' for files with additional line in the content (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_one_row_content.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "Different number of lines in compared content")
})

test_that("Returns 'Different number of lines in compared content' for files with additional line in the content (content mode, default)", {
  file1 <- testthat::test_path("test_outputs/rtf/base_addition_one_row_content.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base.rtf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "Different number of lines in compared content")
})

test_that("Returns 'No differences' for files with additional line in the footer (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_one_row_footer.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'No differences' for files with additional line in the footer (content mode, default)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_one_row_footer.rtf")

  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator)

  expect_equal(result, "No differences")
})

test_that("Returns 'File content has changes in 1 place(s)' for files with a single different row in content (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base_changes_one_row_content.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "File content has changes in 1 place(s)")
})

test_that("No differences' for files with a single different row in footer (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_changes_one_row_footer.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'File content has changes in 2 place(s)' for files with two different rows in content (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base_changes_two_rows_content.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "File content has changes in 2 place(s)")
})

test_that("No differences' for files with two different rows in footer (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_changes_two_rows_footer.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, options = options)

  expect_equal(result, "No differences")
})

#############################################################################################
# CONTENT comparison & WITH omits
#############################################################################################

test_that("Returns 'No differences' for comparison with 'omit' parameter that is not present in either file (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_copy.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "Nothing", options = options)

  expect_equal(result, "No differences")
})

test_that("No differences' when called with 'omit' parameter catching the one row name cells in changed rows in content (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_changes_one_row_content.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "setosa", options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'No differences' when called with 'omit' parameter catching the row name cell in additional row in content (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_one_row_content.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "setosa", options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'No differences' when called with 'omit' parameter catching the two row name cells in additional row in content (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_two_rows_content.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "unknown", options = options)

  expect_equal(result, "No differences")
})

test_that("Returns 'Different number of lines in compared' when called with 'omit' parameter catching one of the two row name cells in additional row in content (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_addition_two_rows_content.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "unknown1", options = options)

  expect_equal(result, "Different number of lines in compared content")
})

test_that("Returns 'File content has changes in 1 place(s)' when called with 'omit' parameter catching one of the two row name cells in changed rows in content (content mode)", {
  file1 <- testthat::test_path("test_outputs/rtf/base.rtf")
  file2 <- testthat::test_path("test_outputs/rtf/base_changes_two_rows_content.rtf")

  options    <- list("rtf" = list("mode" = "content"))
  comparator <- create_file_comparator(file1, file2)
  result     <- compare_files_summary(comparator, omit = "setosa", options = options)

  expect_equal(result, "File content has changes in 1 place(s)")
})
