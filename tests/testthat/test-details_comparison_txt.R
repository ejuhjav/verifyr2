
base <- "test_outputs/txt"

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if both of the files do not exist"
), {
  file1 <- testthat::test_path(base, "nonexisting1.txt")
  file2 <- testthat::test_path(base, "nonexisting2.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_details(comparator)[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "nonexisting.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_details(comparator)[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

################################################################################
# Simple tests that a S4 object is received for details comparison
################################################################################

test_that(paste(
  "Returns S4 comparison object for two files with same content"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "copy.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_details(comparator)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "changes_one_row.txt")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_details(comparator)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")
})
