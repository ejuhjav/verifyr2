
base1 <- "test_outputs/bin"
base2 <- "test_outputs/rtf"

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "'one or both of the folders do not exist' ",
  "printed if one of the folders does not exist"
), {
  folder1 <- testthat::test_path("test_outputs/nonexisting_folder1")
  folder2 <- testthat::test_path("test_outputs/nonexisting_folder2")

  expect_output(
    result <- list_folder_files(folder1, folder2),
    "one or both of the folders do not exist"
  )

  expect_null(result)
})

test_that(paste(
  "'one or both of the folders do not exist' ",
  "printed if both of the folders do not exist"
), {
  folder1 <- testthat::test_path(base1)
  folder2 <- testthat::test_path("test_outputs/nonexisting_folder2")

  expect_output(
    result <- list_folder_files(folder1, folder2),
    "one or both of the folders do not exist"
  )

  expect_null(result)
})

################################################################################
# Successfull cases
################################################################################

test_that("Returns tibble when both folders exist", {
  folder1 <- testthat::test_path(base1)
  folder2 <- testthat::test_path(base2)

  result <- list_folder_files(folder1, folder2)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("file1", "file2"))
})
