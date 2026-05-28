#' XlsxFileComparator.R
#'
#' Specialised comparator for Excel file comparison.
#' This comparator contains the custom handling for reading Excel file
#' contents for comparison.
#'
#' @import readxl
#'
#' @include TxtFileComparator.R
#'
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::create_comparator that will automatically create
#' # the correct comparator instance based on the file types.
#'
#' file1 <- 'my_file1.xlsx'
#' file2 <- 'my_file2.xlsx'
#' comparator <- verifyr2::create_comparator(file1, file2)
#'
#' # If needed, an explicit comparator can be created as well.
#'
#' file1 <- 'my_file1.xlsx'
#' file2 <- 'my_file2.xlsx'
#' comparator <- XlsxFileComparator$new(file1, file2)
#'
#' @export
#'
XlsxFileComparator <- R6::R6Class(
  "XlsxFileComparator",
  inherit = TxtFileComparator,
  public = list(

    #' @description
    #' Method for getting the single file contents for the comparison. The
    #' method returns the file contents in two separate vectors inside a list.
    #' The first vector is the file contents and the second one is the file
    #' contents with the rows matching the omit string excluded. This method
    #' is intended to be called only by the comparator classes in the processing
    #' and shouldn't be called directly by the user.
    #'
    #' For XlsxFileComparator, each sheet is read and rows are converted to
    #' tab-separated strings for text-based comparison.
    #'
    #' @param file   file for which to get the contents
    #' @param config configuration values
    #' @param omit   string pattern to omit from the comparison
    #'
    vrf_contents = function(file, config, omit) {
      self$vrf_open_debug("Xlsx::vrf_contents", config)

      sheets   <- readxl::excel_sheets(file)
      contents <- character(0)

      for (sheet in sheets) {
        data <- readxl::read_excel(
          file,
          sheet     = sheet,
          col_names = TRUE,
          col_types = "text"
        )

        # Add sheet header
        contents <- c(contents, paste0("[Sheet: ", sheet, "]"))

        if (nrow(data) > 0) {
          # Include column headers
          contents <- c(contents, paste(names(data), collapse = "\t"))

          # Convert each row to a tab-separated string
          rows <- apply(data, 1, function(r) {
            paste(ifelse(is.na(r), "", r), collapse = "\t")
          })
          contents <- c(contents, rows)
        }
      }

      result <- self$vrf_contents_inner(contents, config, omit)

      self$vrf_close_debug()
      result
    }
  )
)
