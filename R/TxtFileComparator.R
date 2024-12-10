#' TextFileComparator.R
#'
#' Fallback comparator for text files without any specific definied comparator.
#' This comparator contains the methods for doing basic comparisons on raw text
#' contents.
#'
#' @include BinaryFileComparator.R
#'
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::vrf_comparator, but if needed, an explicit
#' # comparator can be instantiated directly as well:
#' comparator <- new("TxtFileComparator")
#'
#' @export

setClass("TxtFileComparator",
         contains = "BinaryFileComparator",
         slots = list(file1 = "ANY", file2 = "ANY"))

#' Method for comparing the inner part for the details query. This method can be
#' overwritten by more specialized comparator classes. This method is intended
#' to be called only by the comparator classes in the processing and shouldn't
#' be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison.
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from
#'                   the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#'
#' @keywords internal

setMethod("vrf_summary_inner", "TxtFileComparator", function(comparator, file1, file2, omit, options) {

  file1_contents_list <- vrf_contents(comparator, file1, omit, options)
  file2_contents_list <- vrf_contents(comparator, file2, omit, options)

  file1_contents_omit <- file1_contents_list[[2]]
  file2_contents_omit <- file2_contents_list[[2]]

  difference    <- all.equal(file1_contents_omit, file2_contents_omit)
  result        <- "File content comparison failed!"
  result_images <- ""
  pattern       <- "Lengths \\((\\d+), (\\d+)\\) differ \\(string compare on first"

  if (typeof(difference) == "logical") {
    # all.equal returns logical vector if there are no differences
    result <- "No differences."
  } else if (length(difference) >= 1 && grepl(pattern, difference[1])) {
    # all.equal returns length 1/2 vector with first element comtaining text
    #  matching the pattern
    result <- "Different number of lines in compared content."
  } else if (length(difference) == 1) {
    # all.equal returns length 1 vector if the number of rows are the same but
    # there are differences
    count  <- as.numeric(gsub("[^[:digit:].]", "", difference))
    result <- paste0("File content has changes in ", count, " place(s).")
  }

  if (3 == length(file1_contents_list) && 3 == length(file2_contents_list)) {
    result_images <- "No differences in embedded images."
    file1_contents_images = file1_contents_list[[3]]
    file2_contents_images = file2_contents_list[[3]]

    if (length(file1_contents_images) != length(file2_contents_images)) {
      result_images <- "Different amount of embedded images."
    } else {
      matches <- 0
      total <- length(file1_contents_images)

      for (index in 1:length(file1_contents_images)) {
        if (identical(file1_contents_images[[index]], file2_contents_images[[index]])) {
          matches <- matches + 1
        }
      }

      if (matches != length(file1_contents_images)) {
        result_images <- paste0(total - matches, "/", total, " embedded images have differences.")
      }
    }
  }

  return(paste0(result, " ", result_images))
})

#' Method for comparing the inner part for the details query. This method can
#' be overwritten by more specialized comparator classes. This method is
#' intended to be called only by the comparator classes in the processing and
#' shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from
#'                   the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#'
#' @keywords internal

setMethod("vrf_details_inner", "TxtFileComparator", function(comparator, file1, file2, omit, options) {

  file1_contents_list <- vrf_contents(comparator, file1, omit, options)
  file2_contents_list <- vrf_contents(comparator, file2, omit, options)

  file1_contents_whole <- file1_contents_list[[1]]
  file2_contents_whole <- file2_contents_list[[1]]

  context <- 2
  if ("full" == get_nested(options, "details", "mode")) {
    context <- -1
  }

  my_equalizer_with_omit <- function(x, x.chr) {
    my_finalizer(x, x.chr, omit)
  }

  style <- diffobj::StyleHtmlLightRgb(
    html.output = "diff.w.style",
    finalizer = my_equalizer_with_omit
  )

  diff_print <- diffobj::diffPrint(
    file1_contents_whole,
    file2_contents_whole,
    context = context,
    style = style
  )

  result <- list(
    list(
      type = "text",
      contents = diff_print
    )
  )

  if (3 == length(file1_contents_list) && 3 == length(file2_contents_list)) {
    print("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    file1_contents_images = file1_contents_list[[3]]
    file2_contents_images = file2_contents_list[[3]]

    print(length(file1_contents_images))
    print(length(file2_contents_images))

    if (length(file1_contents_images) == length(file2_contents_images)) {
      print("BBBBBBBBBBBBBBBBBBBBBBBBBBBBBB")
      for (index in 1:length(file1_contents_images)) {
        print("CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC")
        #print(file1_contents_list[3])

        comparator <- new("ImgFileComparator")
        result <- append(result, vrf_details_inner_from_bin(
          comparator,                                                        
          file1_contents_images[[index]],
          file2_contents_images[[index]]
        ))
      }
    }
  }

  return(result)
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

setMethod("vrf_contents_inner", "TxtFileComparator", function(comparator, contents, omit, options) {
  contents_omit <- contents

  if (!is.null(omit) && "" != paste0(omit)) {
    contents_omit <- stringr::str_subset(string = contents,
                                         pattern = paste0(omit),
                                         negate = TRUE)
  }

  return(list(contents, contents_omit))
})

#' Custom finalizer method for diffobj html content finalizing. This method is
#' used to modify the diff html output so that omitted rows have their own
#' special styling and gutters.
#'
#' @param x     comparator instance used for the comparison that is meant to
#'              be created with the factory method vrf_comparator.
#' @param x.chr character text representation of x, typically generated with
#'              the as character
#' @param omit  all lines containing the omit string will be excluded from the
#'              comparison (detaulf = NULL)
#'
#' @keywords internal

my_finalizer <- function(x, x.chr, omit) {

  split <- strsplit(x.chr, "<div class='diffobj-row'>")[[1]]

  if (!is.null(omit) && "" != paste0(omit)) {
    for (i in seq_along(split)) {
      if (grepl(omit, split[[i]])) {
        row <- split[[i]]

        # modifying maching row markup
        row <- gsub("class='diffobj-match'",
                    "class='ignore'",
                    row)

        row <- gsub("<div class='diffobj-gutter'><div class='ignore'>&nbsp;",
                    "<div class='diffobj-gutter'><div class='ignore'>x",
                    row)

        # modifying inserted row markup
        row <- gsub("class='insert'",
                    "class='ignore'",
                    row)

        row <- gsub("class='diffobj-word insert'",
                    "class='diffobj-word ignore'",
                    row)

        row <- gsub("<div class='diffobj-gutter'><div class='ignore'>&gt;",
                    "<div class='diffobj-gutter'><div class='ignore'>X",
                    row)

        # modifying deleted row markup
        row <- gsub("class='delete'",
                    "class='ignore'",
                    row)

        row <- gsub("class='diffobj-word delete'",
                    "class='diffobj-word ignore'",
                    row)

        row <- gsub("<div class='diffobj-gutter'><div class='ignore'>&lt;",
                    "<div class='diffobj-gutter'><div class='ignore'>X",
                    row)

        # highlight the ommitted part
        row <- gsub(omit,
                    paste0("<span class='diffobj-word-highlight ignore'>",
                           omit,
                           "</span>"),
                    row)

        split[[i]] <- row
      }
    }
  }

  html_string <- paste(split, collapse = "<div class='diffobj-row'>")

  return(diffobj::finalizeHtml(x, html_string))
}
