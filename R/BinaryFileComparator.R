#' BinaryFileComparator.R
#'
#' Fallback comparator for binary files without any specific definied
#' comparator.
#'
#' @include FileComparator.R
#'
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::vrf_comparator, but if needed, an explicit
#' # comparator can be instantiated directly as well.
#' comparator <- new("BinaryFileComparator")
#'
#' @export

setClass("BinaryFileComparator",
         contains = "FileComparator",
         slots = list(file1 = "ANY", file2 = "ANY"))

#' Generic for getting the single file contents for the comparison. The method
#' returns the file contents in two separate vectors inside a list. The first
#' vector is the file contents and the second one is the file contents with the
#' rows matching the omit string excluded. This method is intended to be called
#' only by the comparator classes in the processing and shouldn't be called
#' directly by the user.
#'
#' @param comparator comparator instance used for the comparison
#' @param file       file for which to get the contents
#' @param omit       all lines containing the omit string will be excluded from
#'                   the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#'
#' @keywords internal

setGeneric("vrf_contents", function(comparator, file, omit, options) standardGeneric("vrf_contents"))

#' Generic for getting the inner part for the file contents query. This method
#' can be overwritten by more specialized comparator classes. This method is
#' intended to be called only by the comparator classes in the processing and
#' shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison
#' @param contents   file contents
#' @param omit       all lines containing the omit string will be excluded
#'                   from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#'
#' @keywords internal

setGeneric("vrf_contents_inner", function(comparator, contents, omit, options) standardGeneric("vrf_contents_inner"))

#' Method for comparing the inner part for the details query. The method
#' returns the file contents in two separate vectors inside a list. The first
#' vector is the file contents and the second one is the file contents with the
#' rows matching the omit string excluded. This method can be overwritten by
#' more specialized comparator classes. This method is intended to be called
#' only by the comparator classes in the processing and shouldn't be called
#' directly by the user.
#'
#' @param comparator comparator instance used for the comparison
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from
#'                   the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#'
#' @keywords internal

setMethod("vrf_summary_inner", "BinaryFileComparator", function(comparator, file1, file2, omit, options) {
  file_info1 <- file.info(file1)
  file_info2 <- file.info(file2)

  if (file_info1$size != file_info2$size) {
    return("Different file sizes for compared files.")
  }

  file1_contents_list <- vrf_contents(comparator, file1, omit, options)
  file2_contents_list <- vrf_contents(comparator, file2, omit, options)

  file1_contents_omit <- file1_contents_list[[2]]
  file2_contents_omit <- file2_contents_list[[2]]

  if (!identical(file1_contents_omit, file2_contents_omit)) {
    return("Different content in compared files.")
  }

  return("No differences.")
})

#' Method for comparing the inner part for the details query. This method can be
#' overwritten by more specialized comparator classes. This method is intended
#' to be called only by the comparator classes in the processing and shouldn't
#' be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from
#'                   the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#'
#' @keywords internal

setMethod("vrf_details_inner", "BinaryFileComparator", function(comparator, file1, file2, omit, options) {
  result <- list(
    type = "text",
    contents = "Binary file without applicable comparator; unable to compare details."
  )
  return(list(result))
})

#' Method for getting the single file contents for the comparison. The method
#' returns the file contents in two separate vectors inside a list. The first
#' vector is the file contents and the second one is the file contents with the
#' rows matching the omit string excluded. This method can be overwritten by
#' more specialized comparator classes. This method is intended to be called
#' only by the comparator classes in the processing and shouldn't be called
#' directly by the user.
#'
#' @param comparator comparator instance used for the comparison
#' @param file       file for which to get the contents
#' @param omit       all lines containing the omit string will be excluded from
#'                   the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#'
#' @keywords internal

setMethod("vrf_contents", "BinaryFileComparator", function(comparator, file, omit, options) {
  contents <- readLines(file, warn = FALSE)
  return(vrf_contents_inner(comparator, contents, omit, options))
})

#' Generic for getting the inner part for the file contents query. The method
#' returns the file contents in two separate vectors inside a list. The first
#' vector is the file contents and the second one is the file contents with the
#' rows matching the omit string excluded. This method can be overwritten by
#' more specialized comparator classes. This method is intended to be called
#' only by the comparator classes in the processing and shouldn't be called
#' directly by the user.
#'
#' @param comparator comparator instance used for the comparison
#' @param contents   file contents
#' @param omit       all lines containing the omit string will be excluded
#'                   from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#'
#' @keywords internal

setMethod("vrf_contents_inner", "BinaryFileComparator", function(comparator, contents, omit, options) {
  return(list(contents, contents))
})
