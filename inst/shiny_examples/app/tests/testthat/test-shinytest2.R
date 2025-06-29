library(shinytest2)

test_that("{shinytest2} recording: app_startup generic", {
  app <- AppDriver$new(
    variant = platform_variant(),
    name = "app_startup",
    height = 777,
    width = 1598
  )
  app$expect_screenshot()
})

test_that("{shinytest2} recording: app_startup input fields", {
  app <- AppDriver$new(
    name = "app_startup2",
    height = 777,
    width = 1598
  )
  # manually test the file & folder input values
  path_val <- app$get_value(input = "file1")
  expect_true(grepl("file2_additional_rows.rtf$", path_val), info = path_val)

  path_val <- app$get_value(input = "file2")
  expect_true(grepl("file3_changed_rows.rtf$", path_val), info = path_val)

  path_val <- app$get_value(input = "folder1")
  expect_true(grepl("base_files$", path_val), info = path_val)

  path_val <- app$get_value(input = "folder2")
  expect_true(grepl("compare_files$", path_val), info = path_val)
})
