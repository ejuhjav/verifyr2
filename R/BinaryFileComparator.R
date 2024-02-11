#' BinaryFileComparator.R
#'
#' Fallback comparator for binary files without any specific definied comparator.
#'
#' @include FileComparator.R
#'
#' @examples
#'
#' The normal way for creating a comparator would be to call the generic factory
#' method \code{verifyr2::create_file_comparator}, but if needed, an explicit comparator
#' can be instantiated directly as well.
#' \code{comparator <- new("BinaryFileComparator")}
#'
#' @export

setClass("BinaryFileComparator", contains = "FileComparator", slots = list(file1 = "ANY", file2 = "ANY"))

#' Generic for getting the single file contents for the comparison. This method is intended to be called only by the comparator classes in the
#' processing and shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file       file for which to get the contents
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters

setGeneric("compare_files_get_contents", function(comparator, file, omit, options, ...) standardGeneric("compare_files_get_contents"))

#' Generic for getting the inner part for the file contents query. This method can be overwritten by more specialized comparator classes. This
#' method is intended to be called only by the comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator    comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file_contents first file to compare
#' @param omit          all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options       additional comparator parameters

setGeneric("compare_files_get_contents_inner", function(comparator, file_contents, omit, options, ...) standardGeneric("compare_files_get_contents_inner"))

#' Method for comparing the inner part for the details query. This method can be overwritten by more specialized comparator classes. This
#' method is intended to be called only by the comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters

setMethod("compare_files_summary_inner", "BinaryFileComparator", function(comparator, file1, file2, omit, options, ...) {
  file_info1 <- file.info(file1)
  file_info2 <- file.info(file2)

  if (file_info1$size != file_info2$size) {
    return("Different file sizes for compared files")
  }

  file1_contents <- compare_files_get_contents(comparator, file1, omit, options)
  file2_contents <- compare_files_get_contents(comparator, file2, omit, options)

  if (!identical(file1_contents, file2_contents)) {
    return("Different content in compared files")
  }

  return("No differences")
})

#' Method for comparing the inner part for the details query. This method can be overwritten by more specialized comparator classes. This
#' method is intended to be called only by the comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters

setMethod("compare_files_details_inner", "BinaryFileComparator", function(comparator, file1, file2, omit, options, ...) {
  return("Binary file without applicable comparator; unable to compare details")
})

#' Method for getting the single file contents for the comparison. This method can be overwritten by more specialized comparator classes. This
#' method is intended to be called only by the comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file       file for which to get the contents
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters

setMethod("compare_files_get_contents", "BinaryFileComparator", function(comparator, file, omit, options, ...) {
  return(compare_files_get_contents_inner(comparator, readLines(file, warn = FALSE), omit, options))
})

#' Generic for getting the inner part for the file contents query. This method can be overwritten by more specialized comparator classes. This
#' method is intended to be called only by the comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator    comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file_contents first file to compare
#' @param omit          all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options       additional comparator parameters

setMethod("compare_files_get_contents_inner", "BinaryFileComparator", function(comparator, file_contents, omit, options, ...) {
  return(file_contents)
})
