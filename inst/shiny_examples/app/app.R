#' Call for shiny example where the user can test verifyr2 package functions
#'
#' @include Config.R
#'
#' \code{verifyr2::run_example} returns simple Shiny App where user can see how
#' the verifyr2 functions work

if (!"verifyr2" %in% loadedNamespaces()) {
  library(verifyr2)
}

# initialize the configuration and possible debug override option
config <- verifyr2::Config$new()
debug  <- getOption("verifyr2.debug", default = FALSE)

if (isTRUE(debug)) {
  config$set("generic.debug", "yes")
}

# the datatable contents with summary comparisons and comments
dt_file_list <- NULL

# executed details comparison values for quick access
dt_comparators_list <- list()

# currently selected datatable row index
row_index <- NULL

current_mode <- shiny::reactiveVal(NULL)
current_omit <- NULL

# ==============================================================================
# Custom input functions
# ==============================================================================

vrf_folder_input <- function(input_id, label, value = "", ...) {
  label_class  <- "control-label"
  input_class  <- "shiny-input-text form-control"
  folder_value <- system.file(value, package = "verifyr2")

  shiny::div(class = "form-group form-group-custom shiny-input-container",
    shinyFiles::shinyDirButton(
      paste0(input_id, "_select"),
      NULL,
      "Select folder",
      icon = shiny::icon("folder-open")
    ),

    shiny::tags$label(
      label,
      `for` = input_id,
      class = label_class
    ),

    shiny::tags$input(
      id    = input_id,
      type  = "text",
      class = input_class,
      value = folder_value,
      ...
    ),
  )
}

vrf_file_input <- function(input_id, label, value = "", ...) {
  label_class <- "control-label"
  input_class <- "shiny-input-text form-control"
  file_value  <- system.file(value, package = "verifyr2")

  shiny::div(class = "form-group form-group-custom shiny-input-container",
    shinyFiles::shinyFilesButton(
      paste0(input_id, "_select"),
      NULL,
      "Select file",
      FALSE,
      icon = shiny::icon("folder-open")
    ),

    shiny::tags$label(
      label,
      `for` = input_id,
      class = label_class
    ),

    shiny::tags$input(
      id    = input_id,
      type  = "text",
      class = input_class,
      value = file_value,
      ...
    ),
  )
}

# ==============================================================================
# Interface definition
# ==============================================================================

javascript_additions <- function() {
  shiny::tags$head(
    shiny::tags$script(htmltools::HTML("
      Shiny.addCustomMessageHandler('highlightRow', function(message) {
        $('.dataTable tr').removeClass('row_highlighted');
        $('.dataTable #process_' + message.row_id)
          .closest('tr')
          .addClass('row_highlighted');
      });
    "))
  )
}

page_icon <- function() {
  shiny::tags$head(
    shiny::tags$link(rel = "icon", type = "image/x-icon", href = "verifyr2.ico")
  )
}

search_container <- function() {
  shiny::div(
    shiny::headerPanel("File content comparison"),
    shiny::wellPanel(
      shiny::tabsetPanel(id = "compare_tabs",
        shiny::tabPanel("Compare folder contents", value = "tabs_folder",
          shiny::fluidRow(
            shiny::column(6,
              vrf_folder_input(
                "folder1",
                "Folder 1",
                "/extdata/base_files"
              ),

              vrf_folder_input(
                "folder2",
                "Folder 2",
                "/extdata/compare_files"
              ),
            ),
            shiny::column(6,
              shiny::textInput(
                "omit_rows",
                "Omit rows with text",
                "versicolor"
              ),

              shiny::textInput(
                "file_name_pattern",
                "File Name Pattern"
              ),
            ),
          ),
        ),
        shiny::tabPanel("Compare specific files", value = "tabs_file",
          shiny::fluidRow(
            shiny::column(6,
              vrf_file_input(
                "file1",
                "File 1",
                "/extdata/base_files/file2_additional_rows.rtf"
              ),

              vrf_file_input(
                "file2",
                "File 2",
                "/extdata/compare_files/file3_changed_rows.rtf"
              ),
            ),
            shiny::column(6,
              shiny::textInput(
                "omit_file_rows",
                "Omit rows with text",
                "versicolor"
              ),
            ),
          ),
        ),
      ),
      shiny::fluidRow(
        shiny::column(12,
          shiny::actionButton("go", "Go"),
          shiny::actionButton("configure", "Configure"),
        ),
      ),
    ),
  )
}

summary_container <- function() {
  shiny::column(12,
    shiny::h2("Summary comparison (vrf_summary):"),
    DT::dataTableOutput("summary_out"),
    shiny::textOutput("summary_text_output"),
    shiny::downloadButton(
      "download_csv",
      "Download comparison results as CSV"
    ),
  )
}

details_container <- function() {
  shiny::column(12,
    shiny::h2("Details comparison (vrf_details):"),
    shiny::fluidRow(
      shiny::column(6,
        shiny::downloadLink(
          "file1_link",
          shiny::textOutput("file1_link_output")
        ),
      ),
      shiny::column(6,
        shiny::downloadLink(
          "file2_link",
          shiny::textOutput("file2_link_output")
        ),
      ),
    ),
    shiny::tabsetPanel(id = "details_tabs",
      shiny::tabPanel("Summary contents", value = "tabs_details_summary",
        shiny::htmlOutput("details_out_summary"),
      ),
      shiny::tabPanel("Full file contents", value = "tabs_details_full",
        shiny::htmlOutput("details_out_full"),
      ),
    ),
    shiny::htmlOutput("details_out_generic"),
    shiny::textOutput("details_text_output"),
    shiny::fluidRow(
      id = "comparison_comments_container",
      shiny::column(12,
        shiny::textAreaInput(
          "details_out_comments",
          "Comments",
          width = "100%"
        ),

        shiny::actionButton("save_comments", "Save comments"),
        shiny::actionButton("clear_comments", "Clear comments"),
      ),
    ),
  )
}

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::includeCSS("styles.css"),
  javascript_additions(),
  page_icon(),
  search_container(),
  shiny::fluidRow(
    summary_container(),
    details_container(),
  ),
)

# ==============================================================================
# Server helper functions
# ==============================================================================

update_download_links <- function(output, row, file1_link, file2_link) {
  file1_link("")
  file2_link("")

  if (!is.na(row$file1)) {
    filename <- basename(row$file1)
    file1_link(paste0("Open ", row$file1))
    output$file1_link <- shiny::downloadHandler(
      filename = function() {
        paste0("old_", filename)
      },
      content = function(file) {
        file.copy(paste0(row$file1), file)
      }
    )
  }

  if (!is.na(row$file2)) {
    filename <- basename(row$file2)
    file2_link(paste0("Open ", row$file2))
    output$file2_link <- shiny::downloadHandler(
      filename = function() {
        paste0("new_", filename)
      },
      content = function(file) {
        file.copy(paste0(row$file2), file)
      }
    )
  }
}

# Helper method for getting the comparator related to specific row index
# or creating a new comparator instance if one is not found from local
# storage.
get_comparator <- function(row_index, file1, file2) {
  row_index_str <- as.character(row_index)

  if (row_index_str %in% names(dt_comparators_list)) {
    return(dt_comparators_list[[row_index_str]])
  }

  comparator <- verifyr2::create_comparator(file1, file2)
  dt_comparators_list[[row_index_str]] <- comparator
  dt_comparators_list <<- dt_comparators_list

  comparator
}

update_details_comparison <- function(
  input,
  output,
  session,
  config_param,
  row,
  row_index
) {
  file1 <- paste0(row[1])
  file2 <- paste0(row[2])

  options <- config_param$clone(deep = TRUE)
  options$set("details.mode", current_mode())

  # empty and hide the display elements by default before redrawing the contents
  set_visibility("details_tabs", FALSE)

  output$details_out_summary <- shiny::renderUI({
    shiny::HTML("")
  })

  output$details_out_full <- shiny::renderUI({
    shiny::HTML("")
  })

  output$details_out_generic <- shiny::renderUI({
    shiny::HTML("")
  })

  shiny::withProgress(
    message = "Processing comparison details...",
    value = 0,
    {
      comparator <- get_comparator(row_index, file1, file2)
      details <- comparator$vrf_details(
        omit   = current_omit,
        config = options
      )

      shiny::incProgress(1)
    }
  )

  lapply(seq_along(details), function(index) {
    instance_data <- details[[index]]

    if ("text" == instance_data$type) {
      set_visibility("details_tabs", TRUE)

      if ("full" == current_mode()) {
        output$details_out_full <- shiny::renderUI({
          shiny::HTML(
            as.character(
              instance_data$content
            )
          )
        })
      } else {
        output$details_out_summary <- shiny::renderUI({
          shiny::HTML(
            as.character(
              instance_data$content
            )
          )
        })
      }
    }
  })

  output$details_out_generic <- shiny::renderUI({
    lapply(seq_along(details), function(index) {
      instance_data <- details[[index]]

      if ("image" == instance_data$type) {
        shiny::tags$div(
          style = "padding: 9.5px;",
          class = "custom-img-diffobj-wrapper",
          shiny::tags$div(
            style = "display: flex;",
            class = "custom-img-diffobj-container top",
            shiny::tags$div(
              style = "display: inline-block; flex: 0 0 33.3333%;",
              shiny::tags$div(
                class = "custom-img-diffobj-image custom-img-diffobj-image-1",
                shiny::tags$span("Image version 1")
              )
            ),
            shiny::tags$div(
              style = "display: inline-block; flex: 0 0 33.3333%;",
              shiny::tags$div(
                class = "custom-img-diffobj-image custom-img-diffobj-image-2",
                shiny::tags$span("Image version 2")
              )
            ),
            shiny::tags$div(
              style = "display: inline-block; flex: 0 0 33.3333%;",
              shiny::tags$div(
                class = "custom-img-diffobj-image custom-img-diffobj-image-3",
                shiny::tags$span("Image difference")
              )
            )
          ),
          shiny::tags$div(
            style = "display: flex;",
            class = "custom-img-diffobj-container bottom",
            shiny::tags$div(
              style = "display: inline-block; flex: 0 0 33.3333%;",
              shiny::tags$div(
                class = "custom-img-diffobj-image-display",
                shiny::tags$img(
                  src   = instance_data$content$image1,
                  alt   = "Image1",
                  style = "width: 100%;"
                )
              )
            ),
            shiny::tags$div(
              style = "display: inline-block; flex: 0 0 33.3333%;",
              shiny::tags$div(
                class = "custom-img-diffobj-image-display",
                shiny::tags$img(
                  src   = instance_data$content$image2,
                  alt   = "Image2",
                  style = "width: 100%;"
                )
              )
            ),
            shiny::tags$div(
              style = "display: inline-block; flex: 0 0 33.3333%;",
              shiny::tags$div(
                class = "custom-img-diffobj-image-display",
                if (!is.null(instance_data$content$image3)) {
                  shiny::tags$img(
                    src   = instance_data$content$image3,
                    alt   = "Difference Image",
                    style = "width: 100%;"
                  )
                } else {
                  shiny::tags$div(
                    style = paste0(
                      "background-color: #fff;",
                      "justify-content: center;",
                      "align-items: center;",
                      "display: flex;",
                      "height: 100%;"
                    ),
                    shiny::tags$div(
                      style = "text-align: center",
                      "No differences"
                    )
                  )
                }
              )
            )
          )
        )
      }
    })
  })
}

update_folder_selections <- function(input, session, roots) {
  if (!is.integer(input$folder1_select)) {
    shiny::updateTextInput(session, "folder1",
      NULL,
      shinyFiles::parseDirPath(roots, input$folder1_select)
    )
  }

  if (!is.integer(input$folder2_select)) {
    shiny::updateTextInput(session, "folder2",
      NULL,
      shinyFiles::parseDirPath(roots, input$folder2_select)
    )
  }
}

update_file_selections <- function(input, session, roots) {
  if (!is.integer(input$file1_select)) {
    shiny::updateTextInput(session, "file1",
      NULL,
      paste0(shinyFiles::parseFilePaths(roots, input$file1_select)$datapath)
    )
  }

  if (!is.integer(input$file2_select)) {
    shiny::updateTextInput(session, "file2",
      NULL,
      paste0(shinyFiles::parseFilePaths(roots, input$file2_select)$datapath)
    )
  }
}

set_visibility <- function(id, visible) {
  if (visible) {
    del <- "'custom_hiddon'"
    add <- "'custom_visible'"
  } else {
    del <- "'custom_visible'"
    add <- "'custom_hidden'"
  }

  element <- paste0("$('#", id, "')")
  shinyjs::runjs(paste0(element, ".removeClass(", del, ").addClass(", add, ")"))
}

set_reactive_text <- function(reactive_id, text, class = "") {
  do.call(reactive_id, list(text))
}

# ==============================================================================
# Server definition
# ==============================================================================

server <- function(input, output, session) {

  # ============================================================================
  # Element initializations
  # ============================================================================

  roots  <- c(
    Home = fs::path_home(),
    Examples = fs::path_package("verifyr2", "extdata")
  )

  params <- list(
    roots = roots,
    session = session,
    restrictions = system.file(package = "base"),
    allowDirCreate = FALSE
  )

  do.call(shinyFiles::shinyDirChoose, c(list(input, "folder1_select"), params))
  do.call(shinyFiles::shinyDirChoose, c(list(input, "folder2_select"), params))

  params <- list(
    roots = roots,
    session = session,
    restrictions = system.file(package = "base")
  )

  do.call(shinyFiles::shinyFileChoose, c(list(input, "file1_select"), params))
  do.call(shinyFiles::shinyFileChoose, c(list(input, "file2_select"), params))

  default1 <- paste0(
    "Select the compared file folders and execute the summary",
    "comparison by clicking on the 'Go' button."
  )

  default2 <- paste0(
    "Click on a row in the summary comparison result to view",
    "the side-by-side details comparison."
  )

  summary_text <- shiny::reactiveVal(default1)
  details_text <- shiny::reactiveVal(default2)
  file1_link <- shiny::reactiveVal("")
  file2_link <- shiny::reactiveVal("")

  dt_proxy <- DT::dataTableProxy("summary_out")

  output$summary_text_output <- shiny::renderText(summary_text())
  output$details_text_output <- shiny::renderText(details_text())

  output$file1_link_output <- shiny::renderText(file1_link())
  output$file2_link_output <- shiny::renderText(file2_link())

  output$download_csv <- shiny::downloadHandler(
    filename = function() {
      paste0(
        "verifyr2_comparison_",
        format(Sys.time(), "%Y%m%d_%H%M"),
        ".csv"
      )
    },
    content = function(file) {
      col_exclude <- c("comments", "process_button")
      dt_subset   <- dt_file_list[, !(names(dt_file_list) %in% col_exclude)]
      write.csv(dt_subset, file, row.names = FALSE)
    }
  )

  output$summary_out <- DT::renderDataTable({
    shiny::req(summary_verify())
    dt_file_list <- summary_verify()

    options <- list(
      columnDefs = list(
        list(visible = FALSE, targets = c("comments_details")),
        list(orderable = FALSE, targets = c("process_button"))
      )
    )

    colnames <- c(
      names(dt_file_list)[-ncol(dt_file_list)],
      "Actions"
    )

    DT::datatable(
      dt_file_list,
      selection = "none",
      escape    = FALSE,
      options   = options,
      colnames  = colnames
    )
  })

  # ============================================================================
  # Reactive elements and observe triggers
  # ============================================================================

  list_of_files <- shiny::eventReactive(input$go, {
    dt_comparators_list <<- list()
    list_files(input, summary_text)
  })

  shiny::observeEvent(input$details_tabs, {
    if (!is.null(current_mode())) {
      mode <- sub("^tabs_details_", "", input$details_tabs)

      if (mode != current_mode()) {
        current_mode(mode)

        new_row_index <- input$process_row
        row <- summary_verify()[new_row_index, ]

        update_details_comparison(
          input,
          output,
          session,
          config,
          row,
          new_row_index
        )
      }
    } else {
      current_mode(config$get("details.mode"))
    }
  })

  shiny::observeEvent(input$save_comments, {
    if (is.null(dt_file_list)) {
      dt_file_list <<- summary_verify()
    }

    comment <- input$details_out_comments

    dt_file_list[row_index, "comments_details"] <- comment
    dt_file_list[row_index, "comments"] <- ifelse(comment != "", "yes", "no")
    dt_file_list <<- dt_file_list

    DT::replaceData(dt_proxy, dt_file_list, resetPaging = FALSE)
    shinyjs::delay(100, {
      session$sendCustomMessage(
        "highlightRow",
        list(row_id = row_index)
      )
    })
  })

  shiny::observeEvent(input$clear_comments, {
    if (!is.null(dt_file_list)) {
      if ("" != dt_file_list[row_index, "comments_details"]) {
        dt_file_list[row_index, "comments_details"] <- ""
        dt_file_list[row_index, "comments"] <- "no"
        dt_file_list <<- dt_file_list

        DT::replaceData(dt_proxy, dt_file_list, resetPaging = FALSE)
        shinyjs::delay(100, {
          session$sendCustomMessage(
            "highlightRow",
            list(row_id = row_index)
          )
        })
      }
    }

    shiny::updateTextAreaInput(session, "details_out_comments", value = "")
  })

  shiny::observeEvent(input$configure, {
    ui_elems <- generate_config_ui_grouped(config$schema, config)

    shiny::showModal(shiny::modalDialog(
      shiny::tags$h2("Comparison configuration"),
      shiny::tagList(ui_elems),

      footer = shiny::tagList(
        shiny::actionButton("reset_config_modal", "Reset"),
        shiny::actionButton("submit_apply", "Apply"),
        shiny::actionButton("submit_apply_save", "Apply and Save"),
        shiny::modalButton("Cancel")
      )
    ))
  })

  shiny::observeEvent(input$submit_apply, {
    apply_config_form_inputs(input, config$schema, config, save = FALSE)
  })

  shiny::observeEvent(input$submit_apply_save, {
    apply_config_form_inputs(input, config$schema, config, save = TRUE)
  })

  shiny::observeEvent(input$reset_config_modal, {
    keys <- names(generate_config_ui_inputs(config$schema, config))
    for (key in keys) {
      shiny::updateSelectInput(session, key, selected = config$get(key))
    }
  })

  summary_verify <- shiny::reactive({
    shiny::req(list_of_files())

    dt_file_list <- tibble::tibble(list_of_files()) |>
      dplyr::mutate(
        omitted = current_omit,
        comparison = NA_character_,
        comments = "no",
        comments_details = "",
        process_button = purrr::pmap_chr(
          .l = list(row_id = dplyr::row_number()),
          .f = function(row_id) {
            as.character(
              shiny::actionButton(
                inputId = paste0("process_", row_id),
                label   = "Compare",
                class   = "process_button",
                onclick = sprintf(
                  "Shiny.setInputValue('process_row', %d)", row_id
                )
              )
            )
          }
        )
      )

    shiny::withProgress(
      message = "Processing comparison summaries...",
      value = 0,
      {
        dt_file_list <- dt_file_list |>
          dplyr::mutate(
            comparison = purrr::pmap_chr(
              .l = list(
                file1     = dt_file_list$file1,
                file2     = dt_file_list$file2,
                omitted   = dt_file_list$omitted,
                row_index = seq_along(dt_file_list$file1)
              ),
              .f = function(file1, file2, omitted, row_index) {
                shiny::setProgress(detail = file1)

                # Process a single row
                comparator <- get_comparator(row_index, file1, file2)
                result <- comparator$vrf_summary(
                  omit   = omitted,
                  config = config
                )

                # Update progress
                shiny::incProgress(1 / nrow(dt_file_list))
                result
              }
            )
          )
      }
    )

    dt_file_list
  })

  shiny::observe({
    # handle changes in folder selections
    update_folder_selections(input, session, roots)
    update_file_selections(input, session, roots)

    shiny::req(input$process_row)
    new_row_index <- input$process_row

    # clear/initialize comparison specific comment value when selecting a row
    if (!is.null(row_index)) {
      if (row_index != new_row_index) {
        row_comment <- paste0(dt_file_list[new_row_index, "comments_details"])
        shiny::updateTextAreaInput(
          session,
          "details_out_comments",
          value = row_comment
        )
      }
    }

    set_visibility("comparison_comments_container", TRUE)
    set_visibility("details_tabs", TRUE)

    row_index <<- new_row_index
    row <- summary_verify()[new_row_index, ]

    # list side-by-side comparison
    set_reactive_text("details_text", "")
    update_details_comparison(
      input,
      output,
      session,
      config,
      row,
      new_row_index
    )

    # set up the file download links for the compared files
    update_download_links(output, row, file1_link, file2_link)

    # set the compared row as highlighted in the table
    session$sendCustomMessage(
      "highlightRow",
      list(row_id = new_row_index)
    )
  })

  set_reactive_text <- function(reactive_id, text, class = "") {
    do.call(reactive_id, list(text))
  }
}

list_files <- function(input, summary_text) {
  if (input$compare_tabs == "tabs_folder") {
    current_omit <<- input$omit_rows

    if (file.exists(input$folder1) && file.exists(input$folder2)) {
      set_visibility("comparison_comments_container", FALSE)
      shinyjs::runjs("$('#download_csv').css('display', 'inline-block');")
      set_reactive_text(summary_text, "")

      verifyr2::list_folder_files(
        input$folder1,
        input$folder2,
        input$file_name_pattern
      )
    } else {
      set_reactive_text(
        summary_text,
        "No folder selected or folders do not exist"
      )
      NULL
    }
  } else {
    current_omit <<- input$omit_file_rows

    if (file.exists(input$file1) && file.exists(input$file2)) {
      set_visibility("comparison_comments_container", FALSE)
      shinyjs::runjs("$('#download_csv').css('display', 'inline-block');")
      set_reactive_text(summary_text, "")

      verifyr2::list_files(
        input$file1,
        input$file2
      )
    } else {
      set_reactive_text(
        summary_text,
        "No files selected or files do not exist"
      )
      NULL
    }
  }
}

generate_config_ui_inputs <- function(schema, config, prefix = "") {
  inputs <- list()

  for (key in names(schema)) {
    full_key <- if (prefix == "") key else paste(prefix, key, sep = ".")
    entry    <- schema[[key]]

    if (is.list(entry)) {
      if (!is.null(entry$options) && !is.null(entry$description)) {
        inputs[[full_key]] <- shiny::selectInput(
          inputId  = full_key,
          label    = entry$description,
          choices  = entry$options,
          selected = config$get(full_key)
        )
      } else {
        sub_inputs <- generate_config_ui_inputs(entry, config, full_key)
        inputs     <- c(inputs, sub_inputs)
      }
    }
  }
  inputs
}

generate_config_ui_grouped <- function(schema, config, prefix = "") {
  groups <- list()

  for (group in names(schema)) {
    entries    <- schema[[group]]
    group_desc <- group

    if (!is.null(entries$description)) {
      group_desc <- entries$description
    }

    # Exclude "description" key itself from leaf scanning
    keys <- setdiff(names(entries), "description")

    inputs <- list()
    for (key in keys) {
      full_key <- paste(group, key, sep = ".")
      entry    <- entries[[key]]

      # Recurse if it's nested
      if (is.list(entry)) {
        if (!is.null(entry$options) && !is.null(entry$description)) {
          inputs[[full_key]] <- shiny::selectInput(
            inputId  = full_key,
            label    = entry$description,
            choices  = entry$options,
            selected = config$get(full_key)
          )
        } else {
          sub_inputs <- generate_config_ui_inputs(entry, config, full_key)
          inputs     <- c(inputs, sub_inputs)
        }
      }
    }

    groups[[group]] <- shiny::tagList(
      shiny::tags$h4(group_desc),
      inputs
    )
  }
  groups
}

apply_config_form_inputs <- function(input, schema, config, save = FALSE) {
  keys <- names(generate_config_ui_inputs(schema, config))

  for (key in keys) {
    val <- input[[key]]
    if (!is.null(val)) {
      config$set(key, val)
    }
  }

  if (save) {
    config$save()
    shiny::showNotification("Configuration saved and applied", type = "message")
  } else {
    shiny::showNotification("Configuration applied", type = "message")
  }

  shiny::removeModal()
}

shiny::shinyApp(ui, server)
