library(shinytest2)

test_that("{shinytest2} recording: startup", {
  skip_on_cran()

  app_path <- system.file("shiny_examples/app", package = "verifyr2")
  app <- AppDriver$new(app_path, name = "startup", height = 544, width = 941)

  # test folders and files separately as the app & snapshot contain full
  # resolved paths that differ depending on user machine.
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
})
