#' PdfFileComparator.R
#'
#' Specialiced comparator for PDF file comparison.
#' This comparator contains the custom handling for handling only PDF content
#' part for the comparison.
#'
#' @import pdftools
#'
#' @include TxtFileComparator.R
#'
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::create_comparator that will automatically create
#' # the correct comparator instance based on the file types.
#'
#' file1 <- 'my_file1.pdf'
#' file2 <- 'my_file2.pdf'
#' comparator <- verifyr2::create_comparator(file1, file2)
#'
#' # If needed, an explicit comparator can be created as well.
#'
#' file1 <- 'my_file1.pdf'
#' file2 <- 'my_file2.pdf'
#' comparator <- PdfFileComparator$new(file1, file2)
#'
#' @export
#'
PdfFileComparator <- R6Class(
  "PdfFileComparator",
  inherit = TxtFileComparator,
  public = list(

    #' @description
    #' Method for getting the single file contents for the comparison. The method
    #' returns the file contents in two separate vectors inside a list. The first
    #' vector is the file contents and the second one is the file contents with the
    #' rows matching the omit string excluded. This method can be overwritten by
    #' more specialized comparator classes. This method is intended to be called
    #' only by the comparator classes in the processing and shouldn't be called
    #' directly by the user.
    #'
    #' For PdfComparator, the file contents are returned based on the mode
    #' parameter if available. "text" mode is the only supported option initally.
    #' "text" mode will return only the text content part of the PDF file.
    #'
    #' @param file    file for which to get the contents
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_contents = function(file, omit, options) {
      content <- pdftools::pdf_text(file)
      content <- paste(content, collapse = "")
      content <- strsplit(content, "\n")[[1]]

      return(self$vrf_contents_inner(content, omit, options))
    }
  )
)
