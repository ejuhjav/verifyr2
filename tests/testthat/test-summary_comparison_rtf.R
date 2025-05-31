
base <- "test_outputs/rtf"

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.'",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "nonexisting.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "File(s) not available; unable to compare.")
})

################################################################################
# WITHOUT omits
################################################################################

test_that(paste(
  "Returns 'No differences.' for identical files"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "base.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'Different number of lines in compared content.'",
  "for files with additional line in the content"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "addition_one_row_content.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "Different number of lines in compared content.")
})

test_that(paste(
  "Returns 'Different number of lines in compared content.'",
  "for files with additional line in the content"
), {
  file1 <- testthat::test_path(base, "addition_one_row_content.rtf")
  file2 <- testthat::test_path(base, "base.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "Different number of lines in compared content.")
})

test_that(paste(
  "Returns 'No differences.'",
  "for files with additional line in the footer"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "addition_one_row_footer.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.'",
  "for files with additional line in the footer"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "addition_one_row_footer.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'File content has changes in 1 place(s).'",
  "for files with a single different row in content"
), {
  file1 <- testthat::test_path(base, "changes_one_row_content.rtf")
  file2 <- testthat::test_path(base, "base.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "File content has changes in 1 place(s).")
})

test_that(paste(
  "Returns 'No differences.'",
  "for files with a single different row in footer"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "changes_one_row_footer.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'File content has changes in 2 place(s).'",
  "for files with two different rows in content"
), {
  file1 <- testthat::test_path(base, "changes_two_rows_content.rtf")
  file2 <- testthat::test_path(base, "base.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "File content has changes in 2 place(s).")
})

test_that(paste(
  "Return 'No differences.'",
  "for files with two different rows in footer"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "changes_two_rows_footer.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "No differences.")
})

################################################################################
# WITH omits
################################################################################

test_that(paste(
  "Returns 'No differences.'",
  "with 'omit' parameter that is not present in either file"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "copy.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(omit = "Nothing")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Return 'No differences.'",
  "with 'omit' catching the one row cells in changed rows in content"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "changes_one_row_content.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(omit = "setosa")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.'",
  "with 'omit' catching the row name cell in additional row in content"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "addition_one_row_content.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(omit = "setosa")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.'",
  "with 'omit' catching the two row cells in additional row in content"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "addition_two_rows_content.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(omit = "unknown")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'Different number of lines in compared content.'",
  "with 'omit' catching one of the row cells in additional row in content"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "addition_two_rows_content.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(omit = "unknown1")

  expect_equal(result, "Different number of lines in compared content.")
})

test_that(paste(
  "Returns 'File content has changes in 1 place(s).'",
  "with 'omit' catching one of the row cells in changed rows in content"
), {
  file1 <- testthat::test_path(base, "base.rtf")
  file2 <- testthat::test_path(base, "changes_two_rows_content.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(omit = "setosa")

  expect_equal(result, "File content has changes in 1 place(s).")
})

################################################################################
# CONTENT comparison with images
################################################################################

test_that(paste(
  "Returns 'No differences. No differences in embedded images.'",
  "for identical files"
), {
  file1 <- testthat::test_path(base, "base_with_image.rtf")
  file2 <- testthat::test_path(base, "base_with_image.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "No differences. No differences in embedded images.")
})

test_that(paste(
  "Returns 'File content has changes in 1 place(s). 1/1 embedded images",
  "have differences.' for files with a single different row in content",
  "and differences in the single embedded image (images enabled)"
), {
  file1 <- testthat::test_path(base, "changes_one_row_content_one_image.rtf")
  file2 <- testthat::test_path(base, "base_with_image.rtf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, paste(
    "File content has changes in 1 place(s).",
    "1/1 embedded images have differences."
  ))
})

test_that(paste(
  "Returns 'File content has changes in 1 place(s)' for files with",
  "with a single different row in content and differences in the single",
  "embedded image (images disabled)"
), {
  file1 <- testthat::test_path(base, "changes_one_row_content_one_image.rtf")
  file2 <- testthat::test_path(base, "base_with_image.rtf")

  options <- Config$new(FALSE)
  options$set("rtf.images", "no")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(options = options)

  expect_equal(result, "File content has changes in 1 place(s).")
})
