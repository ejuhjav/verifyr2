
base   <- "test_outputs/xlsx"
config <- Config$new(FALSE)

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if both of the files do not exist"
), {
  file1 <- testthat::test_path(base, "nonexisting1.xlsx")
  file2 <- testthat::test_path(base, "nonexisting2.xlsx")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.xlsx")
  file2 <- testthat::test_path(base, "nonexisting.xlsx")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

################################################################################
# Simple tests that a S4 object is received for details comparison
################################################################################

test_that(paste(
  "Returns S4 comparison object for two files with same content"
), {
  file1 <- testthat::test_path(base, "base.xlsx")
  file2 <- testthat::test_path(base, "copy.xlsx")

  config <- Config$new(FALSE)
  config$set("details.mode", "summary")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content"
), {
  file1 <- testthat::test_path(base, "base.xlsx")
  file2 <- testthat::test_path(base, "modified.xlsx")

  config <- Config$new(FALSE)
  config$set("details.mode", "full")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")
})
