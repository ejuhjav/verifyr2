
base <- "test_outputs/bin"

################################################################################
# Generic file existence checks
################################################################################

test_that("returns null if both of the files do not exist", {
  file1 <- testthat::test_path(base, "nonexisting1.bin")
  file2 <- testthat::test_path(base, "nonexisting2.bin")

  result <- list_files(file1, file2)
  expect_null(result)
})

test_that("returns null if one of the files does not exist", {
  file1 <- testthat::test_path(base, "base.bin")
  file2 <- testthat::test_path(base, "nonexisting.bin")

  result <- list_files(file1, file2)
  expect_null(result)
})

################################################################################
# Successfull cases
################################################################################

test_that("Returns tibble when both files exist", {
  file1 <- testthat::test_path(base, "base.bin")
  file2 <- testthat::test_path(base, "modified.bin")

  result <- list_files(file1, file2)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("file1", "file2"))
  expect_equal(result$file1, file1)
  expect_equal(result$file2, file2)
})
