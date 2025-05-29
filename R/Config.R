`%||%` <- function(a, b) if (!is.null(a)) a else b

merge_config <- function(defaults, overrides) {
  for (name in names(defaults)) {
    if (!is.list(defaults[[name]]) || is.null(overrides[[name]])) {
      overrides[[name]] <- overrides[[name]] %||% defaults[[name]]
    } else {
      overrides[[name]] <- merge_config(defaults[[name]], overrides[[name]])
    }
  }
  return(overrides)
}

get_nested_value <- function(config, key) {
  parts <- strsplit(key, ".", fixed = TRUE)[[1]]
  for (p in parts) {
    config <- config[[p]]
  }
  return(config)
}

set_nested_value <- function(config, key, value) {
  parts <- strsplit(key, ".", fixed = TRUE)[[1]]
  if (length(parts) == 1) {
    config[[parts[1]]] <- value
  } else {
    config[[parts[1]]] <- set_nested_value(config[[parts[1]]], paste(parts[-1], collapse = "."), value)
  }
  return(config)
}

#' Config.R
#'
#' Class for manging the library configuration options. Creates the default
#' configuration without any source file, populates partial or missing config
#' elements, stores the config file to local machine, and provides easy access
#' methods for setting and getting config values.
#'
#' @examples
#'
#' # Creates the configuration instance. Checks automatically if there is
#' # a previously stored configuration json file available for usage.
#'
#' config <- Config$new()
#'
#' # Getting and setting configuration values
#'
#' value <- config$get("rtf.mode")
#' config$set("rtf.mode", "raw")
#'
#' # Saving the current configuration to local machine
#'
#' config$save()
#'
#' @import jsonlite
#' @import rappdirs
#' @importFrom R6 R6Class
#'
#' @field config      local property for storing the current configuration data
#' @field schema      local property for storing the configuration schema
#' @field config_path local property for storing the stored configuration json file path
#'
#' @export
#'
Config <- R6::R6Class(
  "Config",
  public = list(
    config      = NULL,
    schema      = NULL,
    config_path = NULL,

    #' @description
    #' Constructor for initializing the configuration. Checks the local machine
    #' for existing configuration file is load_config = TRUE. Ensures that all
    #' the project configuration values are included.
    #'
    #' @param load_config load configuration from local machine if available
    #'
    initialize = function(load_config = TRUE) {
      self$schema <- self$get_default_schema()
      self$config_path <- file.path(rappdirs::user_config_dir("verifyr2"), "config.json")
      self$config <- self$get_default_config()

      # Load config from file if exists
      if (load_config && file.exists(self$config_path)) {
        file_config <- jsonlite::read_json(self$config_path, simplifyVector = TRUE)
        self$config <- merge_config(self$config, file_config)
      }
    },

    #' @description
    #' Mehod for getting configuration value based on configuration key. Configuration
    #' item children are separated with a dot in the key notation.
    #'
    #' @param key configuration property key for which to get the value
    #'
    get = function(key) {
      get_nested_value(self$config, key)
    },

    #' @description
    #' Mehod for setting configuration value based on configuration key. Configuration
    #' item children are separated with a dot in the key notation.
    #'
    #' @param key   configuration property key for which to get the value
    #' @param value value to set for the specified configuration key
    #'
    set = function(key, value) {
      self$config <- set_nested_value(self$config, key, value)
    },

    #' @description
    #' Method for saving the current configuration data into local machine. The
    #' save location and file name is currently hard coded.
    #'
    save = function() {
      dir.create(dirname(self$config_path), showWarnings = FALSE, recursive = TRUE)
      jsonlite::write_json(self$config, self$config_path, pretty = TRUE, auto_unbox = TRUE)
      message("Configuration saved to: ", normalizePath(self$config_path))
    },

    #' @description
    #' Helper method for getting configuration default values. These default values
    #' will be used in the configuration in case the configuration properties are not
    #' present previously.
    #'
    get_default_config = function() {
      defaults <- list()

      for (group in names(self$schema)) {
        defaults[[group]] <- list()
        for (key in names(self$schema[[group]])) {
          if (key != "description") {
            defaults[[group]][[key]] <- self$schema[[group]][[key]]$default
          }
        }
      }
      return(defaults)
    },

    #' @description
    #' Method for getting the full configuration schema. Apart from the configuration
    #' data, the schema contains property descriptions as well as all possible values
    #' for the configuration properties.
    #'
    get_default_schema = function() {
      list(
        rtf = list(
          description = "RTF comparison (summary and details)",
          mode = list(
            description = "Mode",
            options = c("raw", "content"),
            default = "content"
          )
        ),
        details = list(
          description = "Details comparison",
          mode = list(
            description = "Mode",
            options = c("full", "summary"),
            default = "summary"
          )
        )
      )
    }
  )
)
