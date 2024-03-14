#' RtfFileComparator.R
#'
#' Specialiced comparator for RTF file comparison.
#' This comparator contains the custom handling for handling only RTF content
#' part for the comparison.
#'
#' @include TxtFileComparator.R
#'
#' @importFrom methods callNextMethod
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::vrf_comparator, but if needed, an explicit
#' # comparator can be instantiated directly as well.
#' comparator <- new("RtfFileComparator")
#'
#' @export

setClass("RtfFileComparator", contains = "TxtFileComparator", slots = list(file1 = "ANY", file2 = "ANY"))

#' Method for getting the single file contents for the comparison. The method
#' returns the file contents in two separate vectors inside a list. The first
#' vector is the file contents and the second one is the file contents with the
#' rows matching the omit string excluded. This method can be overwritten by
#' more specialized comparator classes. This method is intended to be called
#' only by the comparator classes in the processing and shouldn't be called
#' directly by the user.
#'
#' For RtfComparator, the file contents are returned based on the mode
#' parameter if available. "raw" meaning that the rtf file contents are
#' returned as normal raw text contents, and "content" meaning that only rtf
#' file content parts are returned.
#'
#' @param comparator comparator instance used for the comparison
#' @param file       file for which to get the contents
#' @param omit       all lines containing the omit string will be excluded from
#'                   the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#' @param ...        additional parameters
#'
#' @keywords internal

setMethod("vrf_contents", "RtfFileComparator", function(comparator, file, omit, options, ...) {
  if (!is.null(options) &&
        "rtf" %in% names(options) &&
        "mode" %in% names(options$rtf) &&
        "raw" == options$rtf$mode) {
    return(callNextMethod(comparator, file, omit, options, ...))
  } else {
    return(vrf_contents_inner(comparator,
                              striprtf::read_rtf(file = file),
                              omit,
                              options, ...))
  }
})
