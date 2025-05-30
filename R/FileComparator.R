#' FileComparator.R
#'
#' Comparator 'abstract' class containing the generic comparison methods and
#' handling for high level checks (like file existence). This class should never
#' be instantiated - doing that and calling the comparison methods will lead to
#' error.
#'
#' @importFrom R6 R6Class
#'
#' @field file1               local property for storing file1
#' @field file2               local property for storing file2
#' @field file1_contents_list local property for storing extracted file1 contents
#' @field file2_contents_list local property for storing extracted file2 contents
#' @field summary_comparison  local property for storing summary comparison result
#' @field details_comparison  local property for storing details comparison result
#' @field debugger            local property for storing the debugger instance
#'
#' @export
#'

# Disable cyclomatic complexity lint for the R6 class definition as lintr considers
# the whole class definition as a single function.
#
# nolint start: cyclocomp_linter
FileComparator <- R6Class(
  "FileComparator",
  public <- list(
    file1 = NULL,
    file2 = NULL,
    file1_contents_list = NULL,
    file2_contents_list = NULL,
    summary_comparison  = NULL,
    details_comparison  = NULL,
    debugger = NULL,

    #' @description
    #' Initialize a FileComparator instance
    #'
    #' @param file1 First file to compare.
    #' @param file2 Second file to compare.
    #'
    initialize = function(file1 = NULL, file2 = NULL) {
      self$file1 <- file1
      self$file2 <- file2
      self$details_comparison <- list("summary" = NULL, "full" = NULL)
    },

    #' @description
    #' Method for comparing the file summary information. This method is intended
    #' to be implemented only this class level. For comparator specific rules,
    #' the internal method vrf_summary_inner should be customized on lower
    #' levels instead.
    #'
    #' @param omit    string pattern to omit from the comparison (default = NULL)
    #' @param options additional comparator parameters
    #'
    vrf_summary = function(omit = NULL, options = NULL) {
      self$vrf_open_debug("vrf_summary", options)
      self$vrf_add_debug_files()

      if (!is.null(self$summary_comparison)) {
        self$vrf_add_debug("Returning previously calculated comparison results")
        return(self$summary_comparison)
      }

      if (!file.exists(self$file1) || !file.exists(self$file2)) {
        self$vrf_add_debug("One of both of the files not available, unable perform comparison")
        result <- "File(s) not available; unable to compare."
      } else {
        tryCatch({
          result <- self$vrf_summary_inner(omit, options)
        }, error = function(e) {
          self$vrf_add_debug(paste("Processing failed with exception: ", conditionMessage(e)))
          result <- paste0("Error reading file contents: ", conditionMessage(e))
        })
      }

      self$summary_comparison <- result
      self$vrf_close_debug()

      return(result)
    },

    #' @description
    #' Method for comparing the file details information. This method is intended
    #' to be implemented only this class level. For comparator specific rules,
    #' the internal method vrf_summary_inner should be customized on lower
    #' levels instead.
    #'
    #' @param omit    string pattern to omit from the comparison (default = NULL)
    #' @param options additional comparator parameters
    #'
    vrf_details = function(omit = NULL, options = NULL) {
      mode <- self$vrf_option_value(options, "details.mode")
      if ("NA" == mode) {
        mode <- "summary"
      }

      self$vrf_open_debug(paste("vrf_details, mode: ", mode), options)
      self$vrf_add_debug_files()

      if (!is.null(self$details_comparison[[mode]])) {
        self$vrf_add_debug("Returning previously calculated comparison results")
        return(self$details_comparison[[mode]])
      }

      if (!file.exists(self$file1) || !file.exists(self$file2)) {
        self$vrf_add_debug("One of both of the files not available, unable perform comparison")
        result <- list(
          list(
            type = "text",
            contents = "File(s) not available; unable to compare."
          )
        )
      } else {
        tryCatch({
          result <- self$vrf_details_inner(omit, options)
        }, error = function(e) {
          self$vrf_add_debug(paste("Processing failed with exception: ", conditionMessage(e)))
          result <- list(
            list(
              type = "text",
              contents = paste0("Error reading file contents: ", conditionMessage(e))
            )
          )
        })
      }

      self$details_comparison[[mode]] <- result
      self$vrf_close_debug()

      return(result)
    },

    #' @description
    #' "Abstract" method for comparing the inner part for the summary. This method
    #' has to be overwritten by more specialized comparator classes. This method is
    #' intended to be called only by the comparator classes in the processing and
    #' shouldn't be called directly by the user.
    #'
    #' @param omit    string pattern to omit from the comparison (default = NULL)
    #' @param options additional comparator parameters
    #'
    vrf_summary_inner = function(omit, options) {
      stop("vrf_summary_inner must be implemented in a subclass.")
    },

    #' @description
    #' "Abstract" method for comparing the inner part for the detailsThis method
    #' has to be overwritten by more specialized comparator classes. This method is
    #' intended to be called only by the comparator classes in the processing and
    #' shouldn't be called directly by the user.
    #'
    #' @param omit    string pattern to omit from the comparison (default = NULL)
    #' @param options additional comparator parameters
    #'
    vrf_details_inner = function(omit, options) {
      stop("vrf_details_inner must be implemented in a subclass.")
    },

    #' @description
    #' Method for getting specific value from the options. In the initial version,
    #' returns 'NA' if null options is passed.
    #'
    #' @param options comparator parameters
    #' @param key     key to search from the parameters
    #'
    vrf_option_value = function(options, key) {
      if (is.null(options)) {
        return("NA")
      }
      value <- options$get(key)
      return(value)
    },

    #' @description
    #' Wrapper method for the opening a new debugging instance with Debugger
    #' class if debugging is enabled in options. class. Creates the used
    #' debugger instance if needed.
    #'
    #' @param message message to debug to console
    #' @param options comparator parameters
    #'
    vrf_open_debug = function(message, options = NULL) {
      if ("yes" != self$vrf_option_value(options, "generic.debug")) {
        return()
      }

      if (is.null(self$debugger)) {
        self$debugger <- Debugger$new()
      }

      self$debugger$open_debug(message)
    },

    #' @description
    #' Wrapper method for the adding a new debugging message with Debugger
    #' class.
    #'
    #' @param message message to debug to console
    #'
    vrf_add_debug = function(message) {
      if (is.null(self$debugger)) {
        return()
      }

      self$debugger$add_debug(message)
    },

    #' @description
    #' Special method for adding the compared files into debugger stack.
    #'
    vrf_add_debug_files = function() {
      if (is.null(self$debugger)) {
        return()
      }

      self$debugger$add_debug(paste("File 1:", self$file1))
      self$debugger$add_debug(paste("File 2:", self$file2))
    },

    #' @description
    #' Wrapper method for the stopping (closing) current debugging instance with
    #' Debugger class.
    #'
    vrf_close_debug = function() {
      if (is.null(self$debugger)) {
        return()
      }
      self$debugger$close_debug()
    }
  )
)
# nolint end: cyclocomp_linter
