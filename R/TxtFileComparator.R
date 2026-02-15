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
    #' @param config configuration values
    #' @param omit   string pattern to omit from the comparison
    #'
    vrf_summary_inner = function(config, omit) {
      self$vrf_open_debug("Txt::vrf_summary_inner" , config)

      file1_contents_list <- self$file1_contents_list
      file2_contents_list <- self$file2_contents_list

      if (is.null(file1_contents_list)) {
        file1_contents_list <- self$vrf_contents(self$file1, config, omit)
        self$file1_contents_list <- file1_contents_list
      }

      if (is.null(file2_contents_list)) {
        file2_contents_list <- self$vrf_contents(self$file2, config, omit)
        self$file2_contents_list <- file2_contents_list
      }

      file1_contents_processed <- file1_contents_list[[2]]
      file2_contents_processed <- file2_contents_list[[2]]

      difference    <- all.equal(file1_contents_processed, file2_contents_processed)
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
    #' @param config configuration values
    #' @param omit   string pattern to omit from the comparison
    #'
    vrf_details_inner = function(config, omit) {
      self$vrf_open_debug("Txt::vrf_details_inner" , config)

      file1_contents_list <- self$file1_contents_list
      file2_contents_list <- self$file2_contents_list

      if (is.null(file1_contents_list)) {
        file1_contents_list <- self$vrf_contents(self$file1, config, omit)
        self$file1_contents_list <- file1_contents_list
      }

      if (is.null(file2_contents_list)) {
        file2_contents_list <- self$vrf_contents(self$file2, config, omit)
        self$file2_contents_list <- file2_contents_list
      }

      file1_contents_whole <- file1_contents_list[[1]]
      file2_contents_whole <- file2_contents_list[[1]]

      context <- 2
      if ("full" == super$vrf_option_value(config, "details.mode")) {
        context <- -1
      }

      my_equalizer_with_omit <- function(x, x.chr) {
        my_finalizer(x, x.chr, omit)
      }

      style <- diffobj::StyleHtmlLightRgb(
        html.output = "diff.w.style",
        finalizer = my_equalizer_with_omit
      )

      self$vrf_open_debug("Txt::vrf_details_inner::diffPrint" , config)
      diff_print <- diffobj::diffChr(
        file1_contents_whole,
        file2_contents_whole,
        context = context,
        style = style,
        ignore.white.space = ("yes" == super$vrf_option_value(config, "generic.spaces")),
        mode = "sidebyside",
        word.diff = FALSE
      )
      self$vrf_close_debug()

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
    #' processed for empty spaces and omit terms if applicable. This method can
    #' be overwritten by more specialized comparator classes. This method is
    #' intended to be called only by the comparator classes in the processing
    #' and shouldn't be called directly by the user.
    #'
    #' @param contents file contents
    #' @param config   configuration values
    #' @param omit     string pattern to omit from the comparison
    #'
    vrf_contents_inner = function(contents, config, omit) {
      self$vrf_open_debug("Txt::vrf_contents_inner" , config)

      contents_processed <- contents

      if (!is.null(omit) && "" != paste0(omit)) {
        contents_processed <- stringr::str_subset(
          string = contents,
          pattern = paste0(omit),
          negate = TRUE
        )
      }

      if ("yes" == super$vrf_option_value(config, "generic.spaces")) {
          contents_processed <- stringr::str_replace_all(contents_processed, "[ \t]+", " ")
          contents_processed <- stringr::str_trim(contents_processed, side = "both")
      }

      self$vrf_close_debug()
      return(list(contents, contents_processed))
    },

    #' @description
    #' Inherited method for indicating whether detailed comparison is available
    #' with the current comparator. Returns an empty string if the comparator is
    #' is supported, otherwise a string that will be concatenated with the
    #' summary string.
    #'
    #' @param config configuration values
    #'
    vrf_details_supported = function(config) {
      return("")
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
  width <- nchar(length(split))
  index <- 0

  # add row numbers to compare results manually for diffChr. diffobj-header
  # values need to be processed to identify the actual line numbers in summary
  # results.
  for (i in seq_along(split)) {
    row <- split[[i]]

    # remove the special note regarding whitespace ignoring as it is not
    # relevant with the custom whitespace ignoring supported.
    if (1 == i) {
      old <- paste0(
        "<div class='diffobj-container light rgb'>",
        "<pre class='diffobj-content'>",
        "No visible differences between objects, but there are some ",
        "differencessuppressed by `ignore.white.space`, ",
        "`convert.hz.white.space`, `strip.sgr`,and/or `trim`. Set all ",
        "those arguments to FALSE to highlight the differences."
      )

      if (old == row) {
        split[[i]] <- paste0(
          "<div class='diffobj-container light rgb'>",
          "<pre class='diffobj-content'>",
          "No visible differences between objects."
        )
      }
    }

    if (grepl("<div class='diffobj-header'>@@ .*@@</div>", row)) {
      index <- as.integer(sub(".*@@\\s*([0-9]+),.*@@.*", "\\1", row)) - 1
    }

    if (i > 3) {
      if (grepl("<div class='diffobj-header'>@@ .*@@</div>", row)) {
        index <- as.integer(sub(".*@@\\s*([0-9]+),.*@@.*", "\\1", row)) - 1
      } else {
        index <- index + 1
      }
      row_str <- sprintf("%*s", width + 3, sprintf("[%d] ", index))

      row <- gsub(
        "<div class='diffobj-text'><div class='diffobj-match'>",
        paste0(
          "<div class='diffobj-text'><div class='diffobj-match'>",
          "<span class='diffobj-trim'>",
          row_str,
          "</span>"
        ),
        row
      )

      row <- gsub(
        "<div class='diffobj-text'><div class='delete'>",
        paste0(
          "<div class='diffobj-text'><div class='delete'>",
          "<span class='diffobj-trim'>",
          row_str,
          "</span>"
        ),
        row
      )

      row <- gsub(
        "<div class='diffobj-text'><div class='insert'>",
        paste0(
          "<div class='diffobj-text'><div class='insert'>",
          "<span class='diffobj-trim'>",
          row_str,
          "</span>"
        ),
        row
      )

      split[[i]] <- row
    }
  }

  # custom processing for omit row styles
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

  diffobj::finalizeHtml(x, html_string)
}
