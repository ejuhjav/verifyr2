
base <- "test_outputs/bin"

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "'File(s) not available; unable to compare.' ",
  "returned if both of the files do not exist"
), {
  file1 <- testthat::test_path(base, "nonexisting1.bin")
  file2 <- testthat::test_path(base, "nonexisting2.bin")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details()[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

test_that(paste(
  "'File(s) not available; unable to compare.' ",
  "returned if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.bin")
  file2 <- testthat::test_path(base, "nonexisting.bin")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details()[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

################################################################################
# Binary files don't support details comparison
################################################################################

test_that(paste(
  "'Binary file without applicable comparator; unable to compare details.' ",
  "returned for binary files details comparison"
), {
  file1 <- testthat::test_path(base, "base.bin")
  file2 <- testthat::test_path(base, "modified.bin")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details()[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, paste(
    "Binary file without applicable comparator; unable to compare details."
  ))
})
