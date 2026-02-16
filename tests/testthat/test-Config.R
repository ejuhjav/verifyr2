
base <- "test_configs"

################################################################################
# Test configuration json loading
################################################################################

test_that("Configuration json file loaded successfully", {
  path   <- testthat::test_path(base, "config1.json")
  config <- Config$new(config_path = path)

  expect_equal(config$get("generic.debug"), "yes")
  expect_equal(config$get("generic.spaces"), "yes")
  expect_equal(config$get("generic.level"), "row")
  expect_equal(config$get("rtf.images"), "no")
  expect_equal(config$get("details.mode"), "full")
  expect_equal(config$get("pdf.details"), "yes")
})

test_that("Partial configuration successfully updated with defaults", {
  path   <- testthat::test_path(base, "config2.json")
  config <- Config$new(config_path = path)

  expect_equal(config$get("generic.debug"), "no")
  expect_equal(config$get("generic.spaces"), "no")
  expect_equal(config$get("generic.level"), "word")
  expect_equal(config$get("rtf.images"), "no")
  expect_equal(config$get("details.mode"), "summary")
  expect_equal(config$get("pdf.details"), "yes")
})

################################################################################
# Test configuration invalid value in saved file
################################################################################

test_that("Configuration json file loaded successfully", {
  path   <- testthat::test_path(base, "config1.json")

  # mock the magick available method to return false to replicate situation
  # that magick library is not installed.
  local_mocked_bindings(
    check_magick_available = function() FALSE
  )

  config <- Config$new(config_path = path)

  expect_equal(config$get("generic.debug"), "yes")
  expect_equal(config$get("generic.images"), "no")
  expect_equal(config$get("rtf.images"), "no")
  expect_equal(config$get("details.mode"), "full")
  expect_equal(config$get("pdf.details"), "yes")
})

################################################################################
# Test configuration saving
################################################################################

test_that("Configuration json file loaded successfully", {
  file_path <- tempfile(fileext = ".json")
  config    <- Config$new(load_config = FALSE, config_path = file_path)

  config$save()

  expect_true(file.exists(file_path))
  unlink(file_path)
})
