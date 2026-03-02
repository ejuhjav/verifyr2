library(shinytest2)

#' helper method for creating the app for testing
create_app <- function(name) {
  app_path <- system.file("shiny_examples/app", package = "verifyr2")
  app <- AppDriver$new(app_path, name = name, height = 544, width = 941)
  app
}

#' test folders and files separately as the app & snapshot contain full
#' resolved paths that differ depending on user machine.
custom_expect_values <- function(app) {
  expect_true(grepl(
    "base_files",
    app$get_value(input = "folder1")
  ))

  expect_true(grepl(
    "compare_files",
    app$get_value(input = "folder2")
  ))

  expect_true(grepl(
    "file2_additional_rows.rtf",
    app$get_value(input = "file1")
  ))

  expect_true(grepl(
    "file3_changed_rows.rtf",
    app$get_value(input = "file2")
  ))

  vals    <- app$get_values()
  exclude <- c("folder1", "folder2", "file1", "file2")

  app$expect_values(
    input = setdiff(names(vals$input), exclude)
  )
}

test_that("{shinytest2} recording: startup", {
  skip_on_cran()
  app <- create_app("startup")

  custom_expect_values(app)
  app$stop()
})

test_that("{shinytest2} recording: folder_compare", {
  skip_on_cran()
  app <- create_app("folder_compare")

  app$click("go")

  app$wait_for_idle(500)
  custom_expect_values(app)
  app$stop()
})

test_that("{shinytest2} recording: file_compare", {
  skip_on_cran()
  app <- create_app("file_compare")

  app$set_inputs(compare_tabs = "tabs_file")
  app$click("go")

  app$wait_for_idle(500)
  custom_expect_values(app)
  app$stop()
})
