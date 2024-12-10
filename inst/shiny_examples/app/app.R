#' Call for shiny example where the user can test verifyr2 package functions
#'
#' \code{verifyr2::run_example} returns simple Shiny App where user can see how
#' the verifyr2 functions work

# the datatable contents with summary comparisons and comments
dt_file_list <- NULL

# executed details comparison values for quick access
dt_details_list <- list()

# currently selected datatable row index
row_index <- NULL

current_mode <- shiny::reactiveVal(NULL)

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

javascript_additions <- function() {
  shiny::tags$head(
    shiny::tags$script(htmltools::HTML("
      Shiny.addCustomMessageHandler('highlightRow', function(message) {
        $('.dataTable tr').removeClass('row_highlighted');
        $('.dataTable tr:nth-child(' + message.row_id + ')').addClass('row_highlighted');
      });
    "))
  )
}

page_icon <- function() {
  shiny::tags$head(
    shiny::tags$link(rel = "icon", type = "image/x-icon", href = "verifyr2.ico")
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

# helper method for storing details comparison into global list
store_details <- function(row_index, mode, details) {
  if ("NA" == verifyr2::get_nested(dt_details_list, row_index)) {
    dt_details_list[[row_index]] <- list()
  }

  dt_details_list[[row_index]][[mode]] <- details
  dt_details_list <<- dt_details_list
}

update_details_comparison <- function(input, output, session, config, row, row_index) {
  file1 <- paste0(row[1])
  file2 <- paste0(row[2])

  options <- config$configuration
  options$details$mode <- current_mode()

  output$details_out_generic <- shiny::renderUI({
    shiny::HTML("")
  })

  shiny::withProgress(
    message = "Processing comparison details...",
    value = 0,
    {
      comparator <- verifyr2::vrf_comparator(file1, file2)
      details <- verifyr2::get_nested(dt_details_list, as.character(row_index), current_mode())

      if (is.character(details) && "NA" == details) {
        details <- verifyr2::vrf_details(
          comparator,
          omit = input$omit_rows,
          options = options
        )

        shiny::incProgress(1)
        store_details(as.character(row_index), current_mode(), details)
      }
    }
  )

  lapply(seq_along(details), function(index) {
    instance_data <- details[[index]]

    if ("text" == instance_data$type) {
      print("------------------")
      print("------------------")
      print("------------------")
      #set_visibility("details_tabs", TRUE)
      #output$details_out_generic <- shiny::renderUI({
        #shiny::HTML("")
      #})

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

    if ("image" == instance_data$type) {
      print("............................")
      print("............................")
      print("............................")
      print("............................")
      print("............................")
      #output$details_out_summary <- shiny::renderUI({
        #shiny::HTML("")
      #})
      #output$details_out_full <- shiny::renderUI({
        #shiny::HTML("")
      #})
      #set_visibility("details_tabs", FALSE)

      details_out_generic_addition <- shiny::tags$div(
        style = "padding: 9.5px; display: flex;",
        class = "custom-img-diffobj-container",
        shiny::tags$div(
          style = "display: inline-block; flex: 0 0 33.3333%;",
          shiny::tags$div(
            class = "custom-img-diffobj-image custom-img-diffobj-image-1",
            shiny::tags$span("Image version 1")
          ),
          shiny::tags$div(
            class = "custom-img-diffobj-image-display",
            shiny::tags$img(src = instance_data$content$image1, alt = "Image1", style = "width: 100%;")
          )
        ),
        shiny::tags$div(
          style = "display: inline-block; flex: 0 0 33.3333%;",
          shiny::tags$div(
            class = "custom-img-diffobj-image custom-img-diffobj-image-2",
            shiny::tags$span("Image version 2")
          ),
          shiny::tags$div(
            class = "custom-img-diffobj-image-display",
            shiny::tags$img(src = instance_data$content$image2, alt = "Image2", style = "width: 100%;")
          )
        ),
        shiny::tags$div(
          style = "display: inline-block; flex: 0 0 33.3333%;",
          shiny::tags$div(
            class = "custom-img-diffobj-image custom-img-diffobj-image-3",
            shiny::tags$span("Image difference")
          ),
          shiny::tags$div(
            class = "custom-img-diffobj-image-display",
            shiny::tags$img(src = instance_data$content$image3, alt = "Difference Image", style = "width: 100%;")
          )
        )
      )

      insertUI(
        selector = "#details_out_generic",
        where = "beforeEnd",
        ui = details_out_generic_addition
      )
    }
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
    shinyjs::runjs(paste0("$('#", id, "').removeClass('custom_hidden').addClass('custom_visible')"))
  } else {
    shinyjs::runjs(paste0("$('#", id, "').removeClass('custom_visible').addClass('custom_hidden')"))
  }
}

# ==============================================================================
# Server definition
# ==============================================================================

server <- function(input, output, session) {

  # ============================================================================
  # Element initializations
  # ============================================================================

  config_file <- paste0(fs::path_package("/config.json", package = "verifyr2"))
  config_json <- jsonlite::fromJSON(config_file)

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

  default1 <- paste0("Select the compared file folders and execute the summary",
                     "comparison by clicking on the 'Go' button.")

  default2 <- paste0("Click on a row in the summary comparison result to view",
                     "the side-by-side details comparison.")

  config <- shiny::reactiveValues(configuration = as.list(config_json))
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
      dt_subset <- dt_file_list[, !(names(dt_file_list) %in% "comments")]
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
    dt_details_list <<- list()

    if (input$compare_tabs == "tabs_folder") {
      if (file.exists(input$folder1) && file.exists(input$folder2)) {
        set_visibility("comparison_comments_container", FALSE)
        shinyjs::runjs("$('#download_csv').css('display', 'inline-block');")
        set_reactive_text("summary_text", "")

        verifyr2::list_folder_files(
          input$folder1,
          input$folder2,
          input$file_name_pattern
        )
      } else {
        set_reactive_text(
          "summary_text",
          paste0(
            "No folder selected or",
            "folders do not exist"
          )
        )
        NULL
      }
    } else {
      if (file.exists(input$file1) && file.exists(input$file2)) {
        set_visibility("comparison_comments_container", FALSE)
        shinyjs::runjs("$('#download_csv').css('display', 'inline-block');")
        set_reactive_text("summary_text", "")

        verifyr2::list_files(
          input$file1,
          input$file2
        )
      } else {
        set_reactive_text(
          "summary_text",
          paste0(
            "No files selected or",
            "files do not exist"
          )
        )
        NULL
      }
    }
  })

  shiny::observeEvent(input$details_tabs, {
    if (!is.null(current_mode())) {
      mode <- sub("^tabs_details_", "", input$details_tabs)

      if (mode != current_mode()) {
        current_mode(mode)

        new_row_index <- input$process_row
        row <- summary_verify()[new_row_index, ]

        update_details_comparison(input, output, session, config, row, new_row_index)
      }
    } else {
      current_mode(config$configuration$details$mode)
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

    DT::replaceData(dt_proxy, dt_file_list)
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

        DT::replaceData(dt_proxy, dt_file_list)
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
    shiny::showModal(shiny::modalDialog(
      shiny::tags$h2("Comparison configuration"),
      shiny::selectInput(
        "rtf_mode",
        "RTF comparison mode",
        choices = c("raw", "content"),
        selected = config$configuration$rtf$mode
      ),

      shiny::selectInput(
        "details_mode",
        "Details comparison mode",
        choices = c("full", "summary"),
        selected = config$configuration$details$mode
      ),

      shiny::p(
        paste0(
          "The default configuration values can be",
          "initialized in the inst/config.json file."
        )
      ),

      footer = shiny::tagList(
        shiny::actionButton("submit", "Save"),
        shiny::modalButton("Cancel")
      )
    ))
  })

  shiny::observeEvent(input$submit, {
    shiny::removeModal()
    config$configuration$rtf$mode <- input$rtf_mode
    config$configuration$details$mode <- input$details_mode
  })

  summary_verify <- shiny::reactive({
    shiny::req(list_of_files())

    dt_file_list <- tibble::tibble(list_of_files()) %>%
      dplyr::mutate(
        omitted = input$omit_rows,
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
                onclick = sprintf("Shiny.setInputValue('process_row', %d)", row_id)
              )
            )
          }
        )
      )

    shiny::withProgress(
      message = "Processing comparison summaries...",
      value = 0,
      {
        dt_file_list <- dt_file_list %>%
          dplyr::mutate(
            comparison = purrr::pmap_chr(
              .l = list(
                file1   = dt_file_list$file1,
                file2   = dt_file_list$file2,
                omitted = dt_file_list$omitted
              ),
              .f = function(file1, file2, omitted) {

                # Process a single row
                result <- verifyr2::vrf_summary(
                  verifyr2::vrf_comparator(file1, file2),
                  omit    = omitted,
                  options = config$configuration
                )

                # Update progress
                shiny::incProgress(1 / nrow(dt_file_list), detail = file1)
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
    if (!is.null(row_index) && row_index != new_row_index) {
      row_comment <- paste0(dt_file_list[new_row_index, "comments_details"])
      shiny::updateTextAreaInput(
        session,
        "details_out_comments",
        value = row_comment
      )
    }

    set_visibility("comparison_comments_container", TRUE)
    set_visibility("details_tabs", TRUE)

    row_index <<- new_row_index
    row <- summary_verify()[new_row_index, ]

    # list side-by-side comparison
    set_reactive_text("details_text", "")
    update_details_comparison(input, output, session, config, row, new_row_index)

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

shiny::shinyApp(ui, server)
