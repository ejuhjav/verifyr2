
base <- "test_outputs/pdf"

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if both files do not exist"
), {
  file1 <- testthat::test_path(base, "nonexisting1.pdf")
  file2 <- testthat::test_path(base, "nonexisting2.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "File(s) not available; unable to compare.")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.pdf")
  file2 <- testthat::test_path(base, "nonexisting.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "File(s) not available; unable to compare.")
})

################################################################################
# Text file comparison - WITHOUT omit
################################################################################

test_that(paste(
  "Returns 'No differences.' for identical files",
  "(one page)"
), {
  file1 <- testthat::test_path(base, "base.pdf")
  file2 <- testthat::test_path(base, "copy.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.' for identical files",
  "(two pages)"
), {
  file1 <- testthat::test_path(base, "two_pages.pdf")
  file2 <- testthat::test_path(base, "two_pages_copy.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'Different number of lines in compared content.' ",
  "for files with additional line in the content",
  "(one page)"
), {
  file1 <- testthat::test_path(base, "base.pdf")
  file2 <- testthat::test_path(base, "addition_one_row.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "Different number of lines in compared content.")
})

test_that(paste(
  "Returns 'Different number of lines in compared content.' ",
  "for files with additional line in the content",
  "(two pages)"
), {
  file1 <- testthat::test_path(base, "two_pages.pdf")
  file2 <- testthat::test_path(base, "two_pages_addition_one_row.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "Different number of lines in compared content.")
})

test_that(paste(
  "Returns 'File content has changes in 1 place(s).' ",
  "for files with a single different row in content",
  "(one page)"
), {
  file1 <- testthat::test_path(base, "changes_one_row.pdf")
  file2 <- testthat::test_path(base, "base.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "File content has changes in 1 place(s).")
})

test_that(paste(
  "Returns 'File content has changes in 1 place(s).' ",
  "for files with a single different row in content",
  "(two pages)"
), {
  file1 <- testthat::test_path(base, "two_pages_changes_one_row.pdf")
  file2 <- testthat::test_path(base, "two_pages.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "File content has changes in 1 place(s).")
})

test_that(paste(
  "Returns 'File content has changes in 2 place(s).' ",
  "for files with two different rows in content",
  "(two pages)"
), {
  file1 <- testthat::test_path(base, "two_pages_changes_two_rows.pdf")
  file2 <- testthat::test_path(base, "two_pages.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "File content has changes in 2 place(s).")
})

################################################################################
# Text file comparison - WITH omit
################################################################################

test_that(paste(
  "Returns 'No differences.' ",
  "for comparison with 'omit' parameter that is not present in either file",
  "(one page)"
), {
  file1 <- testthat::test_path(base, "base.pdf")
  file2 <- testthat::test_path(base, "copy.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "Nothing")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.' ",
  "for comparison with 'omit' parameter that is not present in either file",
  "(two pages)"
), {
  file1 <- testthat::test_path(base, "two_pages.pdf")
  file2 <- testthat::test_path(base, "two_pages_copy.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "Nothing.")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.' ",
  "when called with 'omit' catching the one row changed row in content",
  "(one page)"
), {
  file1 <- testthat::test_path(base, "base.pdf")
  file2 <- testthat::test_path(base, "changes_one_row.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "simple example pdf")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Return 'No differences.' ",
  "when called with 'omit' catching the one row changed row in content",
  "(two pages)"
), {
  file1 <- testthat::test_path(base, "two_pages.pdf")
  file2 <- testthat::test_path(base, "two_pages_changes_one_row.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "two paged example pdf")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.' ",
  "when called with 'omit' catching the one additional row in content",
  "(one page)"
), {
  file1 <- testthat::test_path(base, "base.pdf")
  file2 <- testthat::test_path(base, "addition_one_row.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "one additional row")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.' ",
  "when called with 'omit' catching the one additional row in content",
  "(two pages)"
), {
  file1 <- testthat::test_path(base, "two_pages.pdf")
  file2 <- testthat::test_path(base, "two_pages_addition_one_row.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "additional row")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.' ",
  "when called with 'omit' catching the two additional rows in content",
  "(one page)"
), {
  file1 <- testthat::test_path(base, "base.pdf")
  file2 <- testthat::test_path(base, "addition_two_rows.pdf")

  omit       <- "additional row"
  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = omit, options = options)

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.' ",
  "when called with 'omit' catching the two additional rows in content",
  "(two pages)"
), {
  file1 <- testthat::test_path(base, "two_pages.pdf")
  file2 <- testthat::test_path(base, "two_pages_addition_two_rows.pdf")

  omit       <- "additional row"
  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = omit, options = options)

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'Different number of lines in compared content.' ",
  "when called with 'omit' catching one of the two additional rows",
  "(one page)"
), {
  file1 <- testthat::test_path(base, "base.pdf")
  file2 <- testthat::test_path(base, "addition_two_rows.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "one additional row")

  expect_equal(result, "Different number of lines in compared content.")
})

test_that(paste(
  "Returns 'Different number of lines in compared content.' ",
  "when called with 'omit' catching one of the two additional rows",
  "(two pages)"
), {
  file1 <- testthat::test_path(base, "two_pages.pdf")
  file2 <- testthat::test_path(base, "two_pages_addition_two_rows.pdf")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator, omit = "one additional row")

  expect_equal(result, "Different number of lines in compared content.")
})
