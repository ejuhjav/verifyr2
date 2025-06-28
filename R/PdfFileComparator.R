#' PdfFileComparator.R
#'
#' Specialiced comparator for PDF file comparison.
#' This comparator contains the custom handling for handling only PDF content
#' part for the comparison.
#'
#' @include BinaryFileComparator.R
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
    #' Method for getting the single file contents for the comparison. The
    #' method returns the file contents in two separate vectors inside a list.
    #' The first vector is the file contents and the second one is the file
    #' contents with the rows matching the omit string excluded. This method
    #' can be overwritten by more specialized comparator classes. This method
    #' is intended to be called only by the comparator classes in the processing
    #' and shouldn't be called directly by the user.
    #'
    #' @param file    file for which to get the contents
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_contents = function(file, omit, options) {
      self$vrf_open_debug("Pdf::vrf_contents", options)

      if ("no" == super$vrf_option_value(options, "pdf.details")) {
        result <- super$vrf_contents(file, omit, options)

        self$vrf_close_debug()
        return(result)
      }

      content <- pdftools::pdf_text(file)
      content <- paste(content, collapse = "")
      content <- strsplit(content, "\n")[[1]]

      result <- self$vrf_contents_inner(content, omit, options)

      self$vrf_close_debug()
      return(result)
    },

    #' @description
    #' Inherited method for indicating whether detailed comparison is available
    #' with the current comparator. Returns an empty string if the comparator is
    #' is supported, otherwise a string that will be concatenated with the
    #' summary string.
    #'
    #' @param options additional comparator parameters
    #'
    vrf_details_supported = function(options) {
      if ("no" == super$vrf_option_value(options, "pdf.details")) {
        return("Pdf details comparison disabled.")
      }
      return(super$vrf_details_supported(options))
    }
  )
)
