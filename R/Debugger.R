
#' Debugger.R
#'
#' Class for managing the library debugging. Tracks the debugged method
#' execution times and prints out the full debugging information only when
#' the main debugging instance is stopped.
#'
#' @examples
#'
#' # Creates a debugger instance.
#'
#' debugger <- Debugger$new()
#'
#' # Opening and closing debugs in multileveled function calls.
#'
#' function1 <- function() {
#'   debugger$open_debug("function1")
#'   function2()
#'   debuger$close_debug()
#' }
#'
#' function2 <- function() {
#'   debugger$open_debug("function2")
#'   Sys.sleep(1)
#'   debugger$close_debug()
#' }
#'
#' # This will produce the following printout to the console after the
#' # function1 finishes
#' #
#' # - 'function1' (execution time 7 ms)
#' #   - 'function2' (execution time 5 ms)
#'
#' @importFrom R6 R6Class
#'
#' @field stack local property for storing debugging labels and start times
#'
#' @export
#'
Debugger <- R6Class("Debugger",
  public = list(
    stack = list(),

    #' @description
    #' Constructor for initializing the Debugger instance.
    #'
    initialize = function() {
      self$stack <- list()
    },

    #' @description
    #' Method for opening new debug instance to the current debugging stack.
    #' Stores also the start time for execution time calculation.
    #'
    #' @param label debugging message
    #'
    open_debug = function(label) {
      entry <- list(
        label      = label,
        start_time = Sys.time(),
        children   = list(),
        duration   = NULL,
        is_message = FALSE
      )
      self$stack[[length(self$stack) + 1]] <- entry
    },

    #' @description
    #' Method for adding a new debug string under the currently open
    #' debug instance.
    #'
    #' @param label debugging message
    #'
    add_debug = function(label) {
      msg <- list(
        label      = label,
        children   = list(),
        duration   = NULL,
        is_message = TRUE
      )

      parent <- self$stack[[length(self$stack)]]
      parent$children[[length(parent$children) + 1]] <- msg
      self$stack[[length(self$stack)]] <- parent
    },

    #' @description
    #' Method for closing a debug instance from the current debugging
    #' stack. If the stopped debug instance is the main level one, the
    #' whole debug data is printed out to console. If the stopped debug
    #' instance is not the main level one, calculates the execution time
    #' of current debug instance and updates the stack data.
    #'
    close_debug = function() {
      # Pop the last entry
      entry <- self$stack[[length(self$stack)]]
      self$stack <- self$stack[-length(self$stack)]

      time_diff      <- difftime(Sys.time(), entry$start_time, units = "secs")
      entry$duration <- round(as.numeric(time_diff) * 1000)

      if (length(self$stack) > 0) {
        parent <- self$stack[[length(self$stack)]]
        parent$children[[length(parent$children) + 1]] <- entry
        self$stack[[length(self$stack)]] <- parent
      } else {
        self$print_debug_tree(entry)
        cat("[DEBUG]'\n")
      }
    },

    #' @description
    #' Recursive method for printing out the current debug stack items and
    #' recursively all the item children. This method is called for the whole
    #' stack once the topmost debug instance is stopped.
    #'
    #' @param entry current debug level being processed for printing
    #' @param depth current processing depth for printing indentation
    #'
    print_debug_tree = function(entry, depth = 0) {
      indent <- paste("[DEBUG]", strrep("  ", depth))
      if (isTRUE(entry$is_message)) {
        cat(indent, "'", entry$label, "'\n", sep = "")
      } else {
        execution_time <- paste0("(execution time ", entry$duration, " ms)")
        cat(indent, "'", entry$label, "' ", execution_time, "\n", sep = "")

        for (child in entry$children) {
          self$print_debug_tree(child, depth + 1)
        }
      }
    }
  )
)
