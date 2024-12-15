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
#'
#' @export
#'
FileComparator <- R6Class(
  "FileComparator",
  public <- list(
    file1 = NULL,
    file2 = NULL,
    file1_contents_list = NULL,
    file2_contents_list = NULL,
    summary_comparison  = NULL,
    details_comparison  = NULL,

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
      if (!is.null(self$summary_comparison)) {
        return(self$summary_comparison)
      }

      if (!file.exists(self$file1) || !file.exists(self$file2)) {
        result <- "File(s) not available; unable to compare."
      }

      tryCatch({
        result <- self$vrf_summary_inner(omit, options)
      }, error = function(e) {
        result <- paste0("Error reading file contents: ", conditionMessage(e))
      })

      self$summary_comparison <- result
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
      mode <- get_nested(options, "details", "mode")
      if ("NA" == mode) {
        mode <- "summary"
      }

      if (!is.null(self$details_comparison[[mode]])) {
        return(self$details_comparison[[mode]])
      }

      if (!file.exists(self$file1) || !file.exists(self$file2)) {
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
          result <- list(
            list(
              type = "text",
              contents = paste0("Error reading file contents: ", conditionMessage(e))
            )
          )
        })
      }

      self$details_comparison[[mode]] <- result
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
    }
  )
)
