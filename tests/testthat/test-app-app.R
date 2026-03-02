library(shinytest2)

#' helper method for creating the app for testing
create_app <- function(name) {
  app_path <- system.file("shiny_examples/app", package = "verifyr2")
  app <- AppDriver$new(app_path, name = name, height = 544, width = 941)
  app
}

compare_button <- function(index) {
  button <- paste0(
    "<button class=\"btn btn-default action-button process_button\" ",
    "id=\"process_",
    index,
    "\" onclick=\"Shiny.setInputValue(&#39;process_row&#39;, ",
    index,
    ")\" type=\"button\">\n  ",
    "<span class=\"action-label\">Compare</span>\n</button>"
  )
  button
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

test_that("{shinytest2} recording: options_reset", {
  skip_on_cran()
  app <- create_app("options_reset")

  app$click("configure")
  app$click("reset_config_modal")

  custom_expect_values(app)
  app$stop()
})

test_that("{shinytest2} recording: comment_persistence", {
  skip_on_cran()
  app <- create_app("comment_persistence")

  cur <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  all <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

  app$click("go")
  app$set_inputs(summary_out_rows_current = cur, allow_no_input_binding_ = TRUE)
  app$set_inputs(summary_out_rows_all = all, allow_no_input_binding_ = TRUE)
  app$set_inputs(
    summary_out_state = c(
      1772479993305,
      0,
      10,
      "",
      TRUE,
      FALSE,
      TRUE,
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(FALSE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE)
    ),
    allow_no_input_binding_ = TRUE
  )

  # click compare on row 2
  app$set_inputs(process_row = 2, allow_no_input_binding_ = TRUE)
  app$set_inputs(
    summary_out_cell_clicked = c("2", "7", compare_button("2")),
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$wait_for_idle(500)

  # check that comment field is empty
  expect_equal(app$get_value(input = "details_out_comments"), "")

  # set and store comment
  app$set_inputs(details_out_comments = "comment1")
  app$click("save_comments")
  expect_equal(app$get_value(input = "details_out_comments"), "comment1")

  app$set_inputs(summary_out_state = c(
    1772480019284,
    0,
    10,
    "",
    TRUE,
    FALSE,
    TRUE,
    c(TRUE, "", TRUE, FALSE, TRUE),
    c(TRUE, "", TRUE, FALSE, TRUE),
    c(TRUE, "", TRUE, FALSE, TRUE),
    c(TRUE, "", TRUE, FALSE, TRUE),
    c(TRUE, "", TRUE, FALSE, TRUE),
    c(TRUE, "", TRUE, FALSE, TRUE),
    c(FALSE, "", TRUE, FALSE, TRUE),
    c(TRUE, "", TRUE, FALSE, TRUE)
  ), allow_no_input_binding_ = TRUE)

  # click compare on row 1
  app$set_inputs(process_row = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(
    summary_out_cell_clicked = c("1", "7", compare_button("1")),
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$wait_for_idle(500)

  # check that comment field is empty
  expect_equal(app$get_value(input = "details_out_comments"), "")

  # click compare on row 2
  app$set_inputs(process_row = 2, allow_no_input_binding_ = TRUE)
  app$set_inputs(
    summary_out_cell_clicked = c("2", "7", compare_button("2")),
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$wait_for_idle(500)

  # check that comment field contains value "comment1"
  expect_equal(app$get_value(input = "details_out_comments"), "comment1")
})
