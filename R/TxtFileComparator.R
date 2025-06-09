#' TextFileComparator.R
#'
#' Fallback comparator for text files without any specific definied comparator.
#' This comparator contains the methods for doing basic comparisons on raw text
#' contents.
#'
#' @import stringr
#'
#' @include BinaryFileComparator.R
#'
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::create_comparator that will automatically create
#' # the correct comparator instance based on the file types.
#'
#' file1 <- 'my_file1.txt'
#' file2 <- 'my_file2.txt'
#' comparator <- verifyr2::create_comparator(file1, file2)
#'
#' # If needed, an explicit comparator can be created as well.
#'
#' file1 <- 'my_file1.lst'
#' file2 <- 'my_file2.lst'
#' comparator <- TxtFileComparator$new(file1, file2)
#'
#' @export
#'

# Disable cyclomatic complexity lint for the R6 class definition as lintr
# considers the whole class definition as a single function.
#
# nolint start: cyclocomp_linter
TxtFileComparator <- R6::R6Class(
  "TxtFileComparator",
  inherit = BinaryFileComparator,
  public = list(

    #' @description
    #' Method for comparing the inner part for the details query. This method
    #' can be overwritten by more specialized comparator classes. This method is
    #' intended to be called only by the comparator classes in the processing
    #' and shouldn't be called directly by the user.
    #'
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_summary_inner = function(omit, options) {
      self$vrf_open_debug("Txt::vrf_summary_inner" , options)

      file1_contents_list <- self$file1_contents_list
      file2_contents_list <- self$file2_contents_list

      if (is.null(file1_contents_list)) {
        file1_contents_list <- self$vrf_contents(self$file1, omit, options)
        self$file1_contents_list <- file1_contents_list
      }

      if (is.null(file2_contents_list)) {
        file2_contents_list <- self$vrf_contents(self$file2, omit, options)
        self$file2_contents_list <- file2_contents_list
      }

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

      self$vrf_close_debug()
      return(result)
    },

    #' @description
    #' Method for comparing the inner part for the details query. This method
    #' can be overwritten by more specialized comparator classes. This method is
    #' intended to be called only by the comparator classes in the processing
    #' and shouldn't be called directly by the user.
    #'
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_details_inner = function(omit, options) {
      self$vrf_open_debug("Txt::vrf_details_inner" , options)

      file1_contents_list <- self$file1_contents_list
      file2_contents_list <- self$file2_contents_list

      if (is.null(file1_contents_list)) {
        file1_contents_list <- self$vrf_contents(self$file1, omit, options)
        self$file1_contents_list <- file1_contents_list
      }

      if (is.null(file2_contents_list)) {
        file2_contents_list <- self$vrf_contents(self$file2, omit, options)
        self$file2_contents_list <- file2_contents_list
      }

      file1_contents_whole <- file1_contents_list[[1]]
      file2_contents_whole <- file2_contents_list[[1]]

      context <- 2
      if ("full" == super$vrf_option_value(options, "details.mode")) {
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

      self$vrf_close_debug()
      return(result)
    },

    #' @description
    #' Method for getting the inner part for the file contents query. The method
    #' returns the file contents in two separate vectors inside a list. The
    #' first vector is the file contents and the second one is the file contents
    #' with the rows matching the omit string excluded. This method can be
    #' overwritten by more specialized comparator classes. This method is
    #' intended to be called only by the comparator classes in the processing
    #' and shouldn't be called directly by the user.
    #'
    #' @param contents file contents
    #' @param omit     string pattern to omit from the comparison
    #' @param options  additional comparator parameters
    #'
    vrf_contents_inner = function(contents, omit, options) {
      self$vrf_open_debug("Txt::vrf_contents_inner" , options)

      contents_omit <- contents

      if (!is.null(omit) && "" != paste0(omit)) {
        contents_omit <- stringr::str_subset(
          string = contents,
          pattern = paste0(omit),
          negate = TRUE
        )
      }

      self$vrf_close_debug()
      return(list(contents, contents_omit))
    }
  )
)
# nolint end: cyclocomp_linter

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
        row <- gsub(
          "class='diffobj-match'",
          "class='ignore'",
          row
        )

        row <- gsub(
          "<div class='diffobj-gutter'><div class='ignore'>&nbsp;",
          "<div class='diffobj-gutter'><div class='ignore'>x",
          row
        )

        # modifying inserted row markup
        row <- gsub(
          "class='insert'",
          "class='ignore'",
          row
        )

        row <- gsub(
          "class='diffobj-word insert'",
          "class='diffobj-word ignore'",
          row
        )

        row <- gsub(
          "<div class='diffobj-gutter'><div class='ignore'>&gt;",
          "<div class='diffobj-gutter'><div class='ignore'>X",
          row
        )

        # modifying deleted row markup
        row <- gsub(
          "class='delete'",
          "class='ignore'",
          row
        )

        row <- gsub(
          "class='diffobj-word delete'",
          "class='diffobj-word ignore'",
          row
        )

        row <- gsub(
          "<div class='diffobj-gutter'><div class='ignore'>&lt;",
          "<div class='diffobj-gutter'><div class='ignore'>X",
          row
        )

        # highlight the ommitted part
        row <- gsub(
          omit,
          paste0(
            "<span class='diffobj-word-highlight ignore'>",
            omit,
            "</span>"
          ),
          row
        )

        split[[i]] <- row
      }
    }
  }

  html_string <- paste(split, collapse = "<div class='diffobj-row'>")

  return(diffobj::finalizeHtml(x, html_string))
}
