
base   <- "test_outputs/pdf"
config <- Config$new(FALSE)

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if both of the files do not exist"
), {
  file1 <- testthat::test_path(base, "nonexisting1.pdf")
  file2 <- testthat::test_path(base, "nonexisting2.pdf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.pdf")
  file2 <- testthat::test_path(base, "nonexisting.pdf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

################################################################################
# Simple tests that a S4 object is received for details comparison
################################################################################

test_that(paste(
  "Returns S4 comparison object for two files with same content."
), {
  file1 <- testthat::test_path(base, "base.pdf")
  file2 <- testthat::test_path(base, "copy.pdf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)[[1]]

  expect_equal(result$type, "text")

  if (requireNamespace("pdftools", quietly = TRUE)) {
    expect_equal(typeof(result$contents), "S4")
  } else {
    expect_equal(result$contents, "Pdf details comparison disabled.")
  }
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content"
), {
  file1 <- testthat::test_path(base, "two_pages.pdf")
  file2 <- testthat::test_path(base, "two_pages_changes_one_row.pdf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)[[1]]

  expect_equal(result$type, "text")

  if (requireNamespace("pdftools", quietly = TRUE)) {
    expect_equal(typeof(result$contents), "S4")
  } else {
    expect_equal(result$contents, "Pdf details comparison disabled.")
  }
})
