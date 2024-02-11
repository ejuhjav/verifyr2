#' TextFileComparator.R
#'
#' Fallback comparator for text files without any specific definied comparator.
#' This comparator contains the methods for doing basic comparisons on raw texct contents.
#'
#' @include BinaryFileComparator.R
#'
#' @examples
#'
#' The normal way for creating a comparator would be to call the generic factory
#' method \code{verifyr2::create_file_comparator}, but if needed, an explicit comparator
#' can be instantiated directly as well.
#' \code{comparator <- new("TxtFileComparator")}
#'
#' @export

setClass("TxtFileComparator", contains = "BinaryFileComparator", slots = list(file1 = "ANY", file2 = "ANY"))

#' Method for comparing the inner part for the details query. This method can be overwritten by more specialized comparator classes. This
#' method is intended to be called only by the comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters

setMethod("compare_files_summary_inner", "TxtFileComparator", function(comparator, file1, file2, omit, options, ...) {
  file1_contents <- compare_files_get_contents(comparator, file1, omit, options)
  file2_contents <- compare_files_get_contents(comparator, file2, omit, options)

  difference <- all.equal(file1_contents, file2_contents)
  result     <- "File content comparison failed!"
  pattern    <- "Lengths \\((\\d+), (\\d+)\\) differ \\(string compare on first"

  if (typeof(difference) == "logical") {
    # all.equal returns logical vector if there are no differences
    result <- "No differences"
  } else if (length(difference) >= 1 && grepl(pattern, difference[1])) {
    # all.equal returns length 1/2 vector with first element comtaining text matching the pattern
    result <- "Different number of lines in compared content"
  } else if (length(difference) == 1) {
    # all.equal returns length 1 vector if the number of rows are the same but there are differences
    count  <- as.numeric(gsub("[^[:digit:].]", "", difference))
    result <- paste0("File content has changes in ", count, " place(s)")
  }

  return(result)
})

#' Method for comparing the inner part for the details query. This method can be overwritten by more specialized comparator classes. This
#' method is intended to be called only by the comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters

setMethod("compare_files_details_inner", "TxtFileComparator", function(comparator, file1, file2, omit, options, ...) {
  file1_contents <- compare_files_get_contents(comparator, file1, omit, options)
  file2_contents <- compare_files_get_contents(comparator, file2, omit, options)

  diff_style <- list(html.output = "diff.w.style")
  diff_print <- diffobj::diffPrint(file1_contents, file2_contents, color.mode = "rgb", format = "html", style = diff_style)

  return(diff_print)
})

#' Generic for getting the inner part for the file contents query. This method can be overwritten by more specialized comparator classes. This
#' method is intended to be called only by the comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator    comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file_contents first file to compare
#' @param omit          all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options       additional comparator parameters

setMethod("compare_files_get_contents_inner", "TxtFileComparator", function(comparator, file_contents, omit, options, ...) {
  if (!is.null(omit) && "" != paste0(omit)) {
    file_contents <- stringr::str_subset(string = file_contents, pattern = paste0(omit), negate = TRUE)
  }

  return(file_contents)
})
