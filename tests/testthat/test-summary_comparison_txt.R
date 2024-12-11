
base <- "test_outputs/txt"

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.'",
  "if both files do not exist"
), {
  file1 <- testthat::test_path(base, "nonexisting1.txt")
  file2 <- testthat::test_path(base, "nonexisting2.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "File(s) not available; unable to compare.")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare.'",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "nonexisting.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "File(s) not available; unable to compare.")
})

################################################################################
# Text file comparison - WITHOUT omit
################################################################################

test_that(paste(
  "Returns 'No differences' for identical files."
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "copy.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'Different number of lines in compared content.'",
  "for files with additional line in the content"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "addition_one_row.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "Different number of lines in compared content.")
})

test_that(paste(
  "Returns 'File content has changes in 1 place(s).'",
  "for files with a single different row in content"
), {
  file1 <- testthat::test_path(base, "changes_one_row.txt")
  file2 <- testthat::test_path(base, "base.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "File content has changes in 1 place(s).")
})

test_that(paste(
  "Returns 'File content has changes in 2 place(s).'",
  "for files with two different rows in content"
), {
  file1 <- testthat::test_path(base, "changes_two_rows.txt")
  file2 <- testthat::test_path(base, "base.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "File content has changes in 2 place(s).")
})

################################################################################
# Text file comparison - WITH omit
################################################################################

test_that(paste(
  "Returns 'No differences.'",
  "for comparison with 'omit' parameter that is not present in either file"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "copy.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "Nothing")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "No differences'",
  "when called with 'omit' catching the one row changed row in content"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "changes_one_row.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "Line 2")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.'",
  "when called with 'omit' catching the one additional row in content"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "addition_one_row.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "Line 4")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.'",
  "when called with 'omit' catching the two additional rows in content"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "addition_two_rows.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "Line 4")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'Different number of lines in compared content.'",
  "when called with 'omit' catching one of the two additional rows"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "addition_two_rows.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "Line 41")

  expect_equal(result, "Different number of lines in compared content.")
})

test_that(paste(
  "Returns 'File content has changes in 1 place(s).'",
  "when called with 'omit' catching one of the two changed rows in content"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "changes_two_rows.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "Line 2")

  expect_equal(result, "File content has changes in 1 place(s).")
})
