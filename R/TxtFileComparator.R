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
  file1_contents_list <- compare_files_get_contents(comparator, file1, omit, options)
  file1_contents_omit <- file1_contents_list[[2]]

  file2_contents_list <- compare_files_get_contents(comparator, file2, omit, options)
  file2_contents_omit <- file2_contents_list[[2]]

  difference <- all.equal(file1_contents_omit, file2_contents_omit)
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
  file1_contents_list   <- compare_files_get_contents(comparator, file1, omit, options)
  file1_contents_whole  <- file1_contents_list[[1]]

  file2_contents_list   <- compare_files_get_contents(comparator, file2, omit, options)
  file2_contents_whole  <- file2_contents_list[[1]]

  my_equalizer_with_omit <- function(x, x.chr) {
    my_finalizer(x, x.chr, omit)
  }

  style <- diffobj::StyleHtmlLightRgb(html.output = "diff.w.style", finalizer = my_equalizer_with_omit)
  diff_print <- diffobj::diffPrint(file1_contents_whole, file2_contents_whole, style = style)

  # TODO: add parameter to defined context
  #diff_print <- diffobj::diffPrint(file1_contents_whole, file2_contents_whole, context = -1, style = style)

  return(diff_print)
})


#' Generic for getting the inner part for the file contents query. The method returns the file contents in two separate
#' vectors inside a list. The first vector is the file contents and the second one is the file contents with the rows
#' matching the omit string excluded. This method can be overwritten by more specialized comparator classes. This
#' method is intended to be called only by the comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator    comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file_contents first file to compare
#' @param omit          all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options       additional comparator parameters

setMethod("compare_files_get_contents_inner", "TxtFileComparator", function(comparator, file_contents, omit, options, ...) {
  file_contents_omit <- file_contents

  if (!is.null(omit) && "" != paste0(omit)) {
    file_contents_omit <- stringr::str_subset(string = file_contents, pattern = paste0(omit), negate = TRUE)
  }

  return(list(file_contents, file_contents_omit))
})

#' Custom finalizer method for diffobj html content finalizing. This method is used to modify the
#' diff html output so that omitted rows have their own special styling and gutters.
#'
#' @param x    comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param x.chr character text representation of x, typically generated with the as.character
#' @param omit  all lines containing the omit string will be excluded from the comparison (detaulf = NULL)

my_finalizer <- function(x, x.chr, omit) {

  split <- strsplit(x.chr, "<div class='diffobj-row'>")[[1]]

  for (i in seq_along(split)) {
    if (grepl(omit, split[[i]])) {

      # modifying maching row markup
      split[[i]] <- gsub("class='diffobj-match'", "class='ignore'", split[[i]])
      split[[i]] <- gsub("<div class='diffobj-gutter'><div class='ignore'>&nbsp;", "<div class='diffobj-gutter'><div class='ignore'>x", split[[i]])

      # modifying inserted row markup
      split[[i]] <- gsub("class='insert'", "class='ignore'", split[[i]])
      split[[i]] <- gsub("class='diffobj-word insert'", "class='diffobj-word ignore'", split[[i]])
      split[[i]] <- gsub("<div class='diffobj-gutter'><div class='ignore'>&gt;", "<div class='diffobj-gutter'><div class='ignore'>X", split[[i]])

      #modifying deleted row markup
      split[[i]] <- gsub("class='delete'", "class='ignore'", split[[i]])
      split[[i]] <- gsub("class='diffobj-word delete'", "class='diffobj-word ignore'", split[[i]])
      split[[i]] <- gsub("<div class='diffobj-gutter'><div class='ignore'>&lt;", "<div class='diffobj-gutter'><div class='ignore'>X", split[[i]])

      # highlight the ommitted part
      split[[i]] <- gsub(omit, paste0("<span class='diffobj-word-highlight ignore'>", omit, "</span>"), split[[i]])
    }
  }

  html_string <- paste(split, collapse = "<div class='diffobj-row'>")

  return(diffobj::finalizeHtml(x, html_string))
}
