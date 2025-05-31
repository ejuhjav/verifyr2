
base <- "test_configs"

################################################################################
# Test configuration json loading
################################################################################

test_that("Configuration json file loaded successfully", {
  path   <- testthat::test_path(base, "config1.json")
  config <- Config$new(config_path = path)

  expect_equal(config$get("generic.debug"), "yes")
  expect_equal(config$get("rtf.images"), "no")
  expect_equal(config$get("details.mode"), "full")
})

test_that("Partial configuration successfully updated with defaults", {
  path   <- testthat::test_path(base, "config2.json")
  config <- Config$new(config_path = path)

  expect_equal(config$get("generic.debug"), "no")
  expect_equal(config$get("rtf.images"), "no")
  expect_equal(config$get("details.mode"), "summary")
})

################################################################################
# Test configuration saving
################################################################################

test_that("Configuration json file loaded successfully", {
  file_name <- paste0(as.integer(Sys.time()), ".json")
  file_path <- testthat::test_path("test_tmp", file_name)
  config    <- Config$new(load_config = FALSE)

  config$path <- file_path
  config$save()

  expect_equal(file.exists(file_path), TRUE)
  unlink(file_path)
})
