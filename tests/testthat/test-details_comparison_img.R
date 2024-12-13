
base <- "test_outputs/img"

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.'",
  "if both of the files do not exist"
), {
  file1 <- testthat::test_path(base, "nonexisting1.jpg")
  file2 <- testthat::test_path(base, "nonexisting2.jpg")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details()[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare.'",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.png")
  file2 <- testthat::test_path(base, "nonexisting.png")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details()[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

################################################################################
# Simple tests that a S4 object is received for details comparison
################################################################################

test_that(paste(
  "Returns S4 comparison object for two files with same file size"
), {
  file1 <- testthat::test_path(base, "base.jpeg")
  file2 <- testthat::test_path(base, "base.jpeg")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details()[[1]]
  contents   <- result$contents

  expect_equal(result$type, "image")
  expect_equal(typeof(contents), "list")
  expect_equal(names(contents), c("image1", "image2", "image3"))
  expect_equal(contents$image1, contents$image2)
  expect_false(isTRUE(all.equal(contents$image1, contents$image3)))
  expect_false(isTRUE(all.equal(contents$image2, contents$image3)))
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in file size"
), {
  file1 <- testthat::test_path(base, "base.png")
  file2 <- testthat::test_path(base, "modified1.png")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details()[[1]]
  contents   <- result$contents

  expect_equal(result$type, "image")
  expect_equal(typeof(contents), "list")
  expect_equal(names(contents), c("image1", "image2", "image3"))
  expect_false(isTRUE(all.equal(contents$image1, contents$image2)))
  expect_false(isTRUE(all.equal(contents$image1, contents$image3)))
  expect_false(isTRUE(all.equal(contents$image2, contents$image3)))
})
