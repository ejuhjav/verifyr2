
base   <- "test_outputs/txt"
config <- Config$new(FALSE)

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if both of the files do not exist"
), {
  file1 <- testthat::test_path(base, "nonexisting1.txt")
  file2 <- testthat::test_path(base, "nonexisting2.txt")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)[[1]]

  expect_equal(result$type, "text")
  expect_equal(result$contents, "File(s) not available; unable to compare.")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "nonexisting.txt")

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
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "copy.txt")

  config_local <- Config$new(FALSE)
  config_local$set("details.mode", "summary")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config_local)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")

  html_output <- as.character(result$contents)
  html_output <- paste(html_output, collapse = "\n")
  expect_false(grepl("='insert'><span class='diffobj-trim'>\\[", html_output))
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "changes_one_row.txt")

  config_local <- Config$new(FALSE)
  config_local$set("details.mode", "full")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config_local)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")

  html_output <- as.character(result$contents)
  html_output <- paste(html_output, collapse = "\n")
  expect_true(grepl("='insert'><span class='diffobj-trim'>\\[", html_output))
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content",
  "spacing (generic.spaces option = 'no')"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "additional_spaces_and_tabs.txt")

  config_local <- Config$new(FALSE)
  config_local$set("generic.spaces", "no")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config_local)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")

  html_output <- as.character(result$contents)
  html_output <- paste(html_output, collapse = "\n")
  expect_true(grepl("='insert'><span class='diffobj-trim'>\\[", html_output))
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content",
  "spacing (generic.spaces option = 'yes')"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "additional_spaces_and_tabs.txt")

  config_local <- Config$new(FALSE)
  config_local$set("generic.spaces", "yes")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config_local)[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")

  html_output <- as.character(result$contents)
  html_output <- paste(html_output, collapse = "\n")
  expect_false(grepl("='insert'><span class='diffobj-trim'>\\[", html_output))
})

test_that(paste(
  "Invokes the custom diffObj finalizer with full mode and omit"
), {
  file1 <- testthat::test_path(base, "base.txt")
  file2 <- testthat::test_path(base, "addition_one_row.txt")

  config <- Config$new(FALSE)
  config$set("details.mode", "full")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config, omit = "Line 4")[[1]]

  expect_equal(result$type, "text")
  expect_equal(typeof(result$contents), "S4")

  # get the result contents to invoke the finalizer callback used for printing
  html_output <- as.character(result$contents)
  html_output <- paste(html_output, collapse = "\n")
  expect_true(grepl("class='ignore'", html_output))
})
