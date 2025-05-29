library(R6)
library(jsonlite)

`%||%` <- function(a, b) if (!is.null(a)) a else b

#get_config_schema <- function() {
  #list(
    #rtf = list(
      #mode = list(
        #default = "content",
        #options = c("raw", "content"),
        #description = "RTF output mode"
      #)
    #),
    #details = list(
      #mode = list(
        #default = "summary",
        #options = c("full", "summary"),
        #description = "Detail level"
      #)
    #)
  #)
#}

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
  config
}

set_nested_value <- function(config, key, value) {
  parts <- strsplit(key, ".", fixed = TRUE)[[1]]
  if (length(parts) == 1) {
    config[[parts[1]]] <- value
  } else {
    config[[parts[1]]] <- set_nested_value(config[[parts[1]]], paste(parts[-1], collapse = "."), value)
  }
  config
}

Config <- R6::R6Class("Config",
  public = list(
    config = NULL,
    schema = NULL,
    config_path = NULL,

    initialize = function(load_config = TRUE) {
      self$schema <- self$get_default_schema()
      self$config_path <- file.path(rappdirs::user_config_dir("verifyr2"), "config.json")
      self$config <- self$get_default_config()

      # Load config from file if exists
      if (load_config && file.exists(self$config_path)) {
        print("WOOOOO");
        file_config <- jsonlite::read_json(self$config_path, simplifyVector = TRUE)
        self$config <- merge_config(self$config, file_config)
      }
    },

    get_defaults_from_schema = function() {
      defaults <- list()
      for (group in names(self$schema)) {
        defaults[[group]] <- list()
        for (key in names(self$schema[[group]])) {
          defaults[[group]][[key]] <- self$schema[[group]][[key]]$default
        }
      }
      defaults
    },

    get = function(key) {
      get_nested_value(self$config, key)
    },

    set = function(key, value) {
      self$config <- set_nested_value(self$config, key, value)
    },

    save = function() {
      dir.create(dirname(self$config_path), showWarnings = FALSE, recursive = TRUE)
      jsonlite::write_json(self$config, self$config_path, pretty = TRUE, auto_unbox = TRUE)
      message("Configuration saved to: ", normalizePath(self$config_path))
    },

    get_default_config = function() {
      # Extract default values from schema
      defaults <- list()
      for (group in names(self$schema)) {
        defaults[[group]] <- list()
        for (key in names(self$schema[[group]])) {
          if (key != "description") {
            defaults[[group]][[key]] <- self$schema[[group]][[key]]$default
          }
        }
      }
      defaults
    },

    get_default_schema = function() {
      list(
        rtf = list(
          description = "RTF Output Configuration",
          mode = list(
            description = "RTF Mode",
            options = c("raw", "content"),
            default = "content"
          )
        ),
        details = list(
          description = "Detail Level",
          mode = list(
            description = "Details Mode",
            options = c("full", "summary"),
            default = "summary"
          )
        )
      )
    },

    print = function(...) {
      cat("Current Configuration:\n")
      print(self$config)
    }
  )
)
