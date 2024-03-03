#' Call for shiny example where the user can test verifyr2 package functions
#'
#' \code{verifyr2::run_example} returns simple Shiny App where user can see how the verifyr2 functions work
#'

custom_folder_input <- function(input_id, label, value = "", ...) {
  label_class  <- "control-label"
  input_class  <- "shiny-input-text form-control shiny-bound-input"
  folder_value <- system.file(value, package = "verifyr2")

  shiny::div(class = "form-group form-group-custom shiny-input-container",
    shinyFiles::shinyDirButton(paste0(input_id, "_select"), NULL, "Select folder", icon = shiny::icon("folder-open")),
    shiny::tags$label(label, `for` = input_id, class = label_class),
    shiny::tags$input(id = input_id, type = "text", class = input_class, value = folder_value, ...),
  )
}

custom_file_input <- function(input_id, label, value = "", ...) {
  label_class  <- "control-label"
  input_class  <- "shiny-input-text form-control shiny-bound-input"
  file_value <- system.file(value, package = "verifyr2")

  shiny::div(class = "form-group form-group-custom shiny-input-container",
    shinyFiles::shinyFilesButton(paste0(input_id, "_select"), NULL, "Select file", FALSE, icon = shiny::icon("folder-open")),
    shiny::tags$label(label, `for` = input_id, class = label_class),
    shiny::tags$input(id = input_id, type = "text", class = input_class, value = file_value, ...),
  )
}

dt_global_file_list <- NULL
sel_row_index <- NULL

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::includeCSS("styles.css"),
  shiny::headerPanel("File content comparison"),
  shiny::wellPanel(
    shiny::tabsetPanel(id = "compare_tabs",
      shiny::tabPanel("Compare folder contents", value = "tabs_folder",
        shiny::fluidRow(
          shiny::column(6,
            custom_folder_input("folder1", "Folder 1", "/extdata/base_files"),
            custom_folder_input("folder2", "Folder 2", "/extdata/compare_files"),
          ),
          shiny::column(6,
            shiny::textInput("omit_rows", "Omit rows with text", "versicolor"),
            shiny::textInput("file_name_pattern", "File Name Pattern"),
          ),
        ),
      ),
      shiny::tabPanel("Compare specific files", value = "tabs_file",
        shiny::fluidRow(
          shiny::column(6,
            custom_file_input("file1", "File 1", "/extdata/base_files/file2_additional_rows.rtf"),
            custom_file_input("file2", "File 2", "/extdata/compare_files/file3_changed_rows.rtf"),
          ),
          shiny::column(6,
            shiny::textInput("omit_file_rows", "Omit rows with text", "versicolor"),
          ),
        ),
      ),
    ),
    shiny::fluidRow(
      shiny::column(12,
        shiny::actionButton("go", "Go"),
        shiny::actionButton("config", "Configure"),
      ),
    ),
  ),
  shiny::fluidRow(
    shiny::column(12,
      h2("Summary comparison (comparator compare_files_summary):"),
      DT::dataTableOutput("summary_out"),
      shiny::textOutput("summary_text_output"),
      shiny::downloadButton("download_csv", "Download comparison results as CSV"),
    ),
    shiny::column(12,
      h2("Details comparison (comparator compare_files_details):"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::downloadLink("open_folder1_file_link", shiny::textOutput("open_folder1_file_link_output")),
        ),
        shiny::column(6,
          shiny::downloadLink("open_folder2_file_link", shiny::textOutput("open_folder2_file_link_output")),
        ),
      ),
      shiny::tabsetPanel(id = "details_tabs",
        shiny::tabPanel("Full file contents", value = "tabs_details_full",
          shiny::htmlOutput("details_out_full"),
        ),
        shiny::tabPanel("Summary contents", value = "tabs_details_summary",
          shiny::htmlOutput("details_out_summary"),
        ),
      ),
      shiny::textOutput("details_text_output"),
      shiny::fluidRow(
        id = "comparison_comments_container",
        shiny::column(12,
          shiny::textAreaInput("details_out_comments", "Comments", width = "100%"),
          shiny::actionButton("save_comments", "Save comments"),
          shiny::actionButton("clear_comments", "Clear comments"),
        ),
      ),
    ),
  ),
)
server <- function(input, output, session) {

  # ===============================================================================================
  # Element initializations
  # ===============================================================================================

  config_file <- paste0(fs::path_package("/config.json", package = "verifyr2"))
  config_json <- jsonlite::fromJSON(config_file)

  roots  <- c(Home = fs::path_home(), Examples = fs::path_package("verifyr2", "extdata"))
  params <- list(roots = roots, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)

  do.call(shinyFiles::shinyDirChoose, c(list(input, "folder1_select"), params))
  do.call(shinyFiles::shinyDirChoose, c(list(input, "folder2_select"), params))

  params <- list(roots = roots, session = session, restrictions = system.file(package = "base"))

  do.call(shinyFiles::shinyFileChoose, c(list(input, "file1_select"), params))
  do.call(shinyFiles::shinyFileChoose, c(list(input, "file2_select"), params))

  default1 <- "Select the compared file folders and execute the summary comparison by clicking on the 'Go' button."
  default2 <- "Click on a row in the summary comparison result to view the side-by-side details comparison."

  configuration <- shiny::reactiveValues(config = as.list(config_json))
  summary_text  <- shiny::reactiveVal(default1)
  details_text  <- shiny::reactiveVal(default2)
  open_folder1_file_link <- shiny::reactiveVal("")
  open_folder2_file_link <- shiny::reactiveVal("")

  dt_proxy <- DT::dataTableProxy("summary_out")

  output$summary_text_output <- shiny::renderText(summary_text())
  output$details_text_output <- shiny::renderText(details_text())

  output$open_folder1_file_link_output <- shiny::renderText(open_folder1_file_link())
  output$open_folder2_file_link_output <- shiny::renderText(open_folder2_file_link())

  output$download_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("verifyr2_comparison_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      dt_subset <- dt_global_file_list[, !(names(dt_global_file_list) %in% "comments")]
      write.csv(dt_subset, file, row.names = FALSE)
    }
  )

  output$summary_out <- DT::renderDataTable({
    shiny::req(summary_verify())
    options <- list(columnDefs = list(list(visible = FALSE, targets = c("comments_details"))))
    DT::datatable(summary_verify(), selection = "single", options = options)
  })

  # ===============================================================================================
  # Reactive elements and observe triggers
  # ===============================================================================================

  list_of_files <- shiny::eventReactive(input$go, {
    if (input$compare_tabs == "tabs_folder") {
      if (file.exists(input$folder1) && file.exists(input$folder2)) {
        set_visibility("comparison_comments_container", FALSE)
        shinyjs::runjs("$('#download_csv').css('display', 'inline-block');")
        set_reactive_text("summary_text", "")
        verifyr2::list_folder_files(input$folder1, input$folder2, input$file_name_pattern)
      } else {
        set_reactive_text("summary_text", "No folder selected or folders do not exist")
        NULL
      }
    } else {
      if (file.exists(input$file1) && file.exists(input$file2)) {
        set_visibility("comparison_comments_container", FALSE)
        shinyjs::runjs("$('#download_csv').css('display', 'inline-block');")
        set_reactive_text("summary_text", "")
        verifyr2::list_files(input$file1, input$file2)
      } else {
        set_reactive_text("summary_text", "No files selected or files do not exist")
        NULL
      }
    }
  })

  shiny::observeEvent(input$save_comments, {
    if (is.null(dt_global_file_list)) {
      dt_global_file_list <<- summary_verify()
    }

    dt_global_file_list[sel_row_index, "comments_details"] <- input$details_out_comments
    dt_global_file_list[sel_row_index, "comments"] <- ifelse(input$details_out_comments != "", "yes", "no")
    dt_global_file_list <<- dt_global_file_list

    DT::replaceData(dt_proxy, dt_global_file_list)
    DT::selectRows(dt_proxy, sel_row_index)
  })

  shiny::observeEvent(input$clear_comments, {
    if (!is.null(dt_global_file_list)) {
      if ("" != dt_global_file_list[sel_row_index, "comments_details"]) {
        dt_global_file_list[sel_row_index, "comments_details"] <- ""
        dt_global_file_list[sel_row_index, "comments"] <- "no"
        dt_global_file_list <<- dt_global_file_list

        DT::replaceData(dt_proxy, dt_global_file_list)
        DT::selectRows(dt_proxy, sel_row_index)
      }
    }

    shiny::updateTextAreaInput(session, "details_out_comments", value = "")
  })

  shiny::observeEvent(input$config, {
    shiny::showModal(shiny::modalDialog(
      shiny::tags$h2("Comparison configuration"),
      shiny::selectInput("rtf_mode", "RTF comparison mode", choices = c("raw", "content"), selected = configuration$config$rtf$mode),
      shiny::selectInput("details_mode", "Details comparison mode", choices = c("full", "summary"), selected = configuration$config$details$mode),
      shiny::p("The default configuration values can be set in the config.json file."),
      footer = shiny::tagList(
        shiny::actionButton("submit", "Save"),
        shiny::modalButton("Cancel")
      )
    ))
  })

  shiny::observeEvent(input$submit, {
    shiny::removeModal()
    configuration$config$rtf$mode <- input$rtf_mode
    configuration$config$details$mode <- input$details_mode
  })

  summary_verify <- shiny::reactive({
    shiny::req(list_of_files())
    dt_global_file_list <<- tibble::tibble(list_of_files()) %>%
      dplyr::mutate(omitted = input$omit_rows) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(comparison = verifyr2::compare_files_summary(verifyr2::create_file_comparator(file1, file2), omit = omitted, options = configuration$config)) %>% # nolint
      dplyr::mutate(comments = "no") %>%
      dplyr::mutate(comments_details = "")
  })

  shiny::observe({
    # handle changes in folder selections
    update_folder_selections()
    update_file_selections()

    # handle changes related to selecting a comparison row
    shiny::req(input$summary_out_rows_selected)
    new_row_index <- input$summary_out_rows_selected

    # clear/initialize the comparison specific comment value when selecting a new row
    if (!is.null(sel_row_index) && sel_row_index != new_row_index) {
      row_comment <- paste0(dt_global_file_list[new_row_index, "comments_details"])
      shiny::updateTextAreaInput(session, "details_out_comments", value = row_comment)
    }

    set_visibility("comparison_comments_container", TRUE)
    set_visibility("details_tabs", TRUE)

    sel_row_index <<- new_row_index
    sel_row <- summary_verify()[new_row_index, ]

    # list side-by-side comparison
    update_details_comparison(sel_row)

    # set up the file download links for the compared files
    update_download_links(sel_row)

  })

  # ===============================================================================================
  # Helper functions
  # ===============================================================================================

  set_visibility <- function(id, visible) {
    visible_text <- ifelse(visible, "show()", "hide()")
    shinyjs::runjs(paste0("$('#", id, "').", visible_text))
  }

  set_reactive_text <- function(reactive_id, text, class = "") {
    do.call(reactive_id, list(text))
  }

  update_folder_selections <- function() {
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

  update_file_selections <- function() {
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

  update_details_comparison <- function(sel_row) {
    set_reactive_text("details_text", "")
    file1 <- paste0(sel_row[1])
    file2 <- paste0(sel_row[2])

    options1 <- configuration$config
    options1$details$mode <- "full"

    output$details_out_full <- shiny::renderUI({
      shiny::HTML(
        as.character(
          verifyr2::compare_files_details(verifyr2::create_file_comparator(file1, file2), omit = input$omit_rows, options = options1)
        )
      )
    })

    options2 <- configuration$config
    options2$details$mode <- "summary"

    output$details_out_summary <- shiny::renderUI({
      shiny::HTML(
        as.character(
          verifyr2::compare_files_details(verifyr2::create_file_comparator(file1, file2), omit = input$omit_rows, options = options2)
        )
      )
    })

    if (configuration$config$details$mode == "summary") {
      updateTabsetPanel(session, "details_tabs", selected = "tabs_details_summary")
    } else {
      updateTabsetPanel(session, "details_tabs", selected = "tabs_details_full")
    }
  }

  update_download_links <- function(sel_row) {
    open_folder1_file_link("")
    open_folder2_file_link("")

    if (!is.na(sel_row$file1)) {
      filename <- basename(sel_row$file1)
      open_folder1_file_link(paste0("Open ", sel_row$file1))
      output$open_folder1_file_link <- shiny::downloadHandler(
        filename = function() {
          paste0("old_", filename)
        },
        content = function(file) {
          file.copy(paste0(sel_row$file1), file)
        }
      )
    }

    if (!is.na(sel_row$file2)) {
      filename <- basename(sel_row$file2)
      open_folder2_file_link(paste0("Open ", sel_row$file2))
      output$open_folder2_file_link <- shiny::downloadHandler(
        filename = function() {
          paste0("new_", filename)
        },
        content = function(file) {
          file.copy(paste0(sel_row$file2), file)
        }
      )
    }
  }
}

shiny::shinyApp(ui, server)
