#' PdfFileComparator.R
#'
#' Specialiced comparator for PDF file comparison.
#' This comparator contains the custom handling for handling only PDF content
#' part for the comparison.
#'
#' @include TxtFileComparator.R
#'
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::vrf_comparator, but if needed, an explicit
#' # comparator can be instantiated directly as well.
#' comparator <- new("PdfFileComparator")
#'
#' @export

setClass("PdfFileComparator",
         contains = "TxtFileComparator",
         slots = list(file1 = "ANY", file2 = "ANY"))

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
#' @param comparator comparator instance used for the comparison
#' @param file       file for which to get the contents
#' @param omit       all lines containing the omit string will be excluded from
#'                   the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#'
#' @keywords internal

setMethod("vrf_contents", "PdfFileComparator", function(comparator, file, omit, options) {
  content <- pdftools::pdf_text(file)
  content <- paste(content, collapse = "")
  content <- strsplit(content, "\n")[[1]]

  return(vrf_contents_inner(comparator, content, omit, options))
})
