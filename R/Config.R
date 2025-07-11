
merge_values <- function(defaults, overrides) {
  result <- list()

  for (name in names(defaults)) {
    def <- defaults[[name]]
    over <- overrides[[name]]

    has_defaults <- "default" %in% names(def)
    has_options  <- "options" %in% names(def)

    if (is.list(def) && has_defaults && has_options) {
      if (!is.null(over) && over %in% def$options) {
        result[[name]] <- over
      } else {
        result[[name]] <- def$default
      }
    } else if (is.list(def)) {
      if (!is.list(over)) over <- list()
      result[[name]] <- merge_values(def, over)
    } else {
      result[[name]] <- if (!is.null(over)) over else def
    }
  }

  result
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
    sub_key <- paste(parts[-1], collapse = ".")
    config[[parts[1]]] <- set_nested_value(config[[parts[1]]], sub_key, value)
  }
  config
}

check_magick_available <- function() {
  requireNamespace("magick", quietly = TRUE)
}

check_pdftools_available <- function() {
  requireNamespace("pdftools", quietly = TRUE)
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
#' # a previously stored configuration json file available for usage. Note
#' # that you don't need to explicitly define the config file path for the
#' # Config instance - by default the config file will be searched from and
#' # written in user-specific configuration directory for the package.
#'
#' path   <- tempfile(fileext = ".json")
#' config <- Config$new(config_path = path)
#'
#' # Getting and setting configuration values
#'
#' value <- config$get("defailts.mode")
#' config$set("details.mode", "full")
#'
#' # Saving the current configuration to local machine (to tmp folder with
#' # the given explicit file path in initialization).
#'
#' config$save()
#'
#' @import jsonlite
#' @import rappdirs
#' @importFrom R6 R6Class
#'
#' @field schema configuration schema
#' @field config current configuration data
#' @field path   configuration json file path
#'
#' @export
#'
Config <- R6::R6Class(
  "Config",
  public = list(
    schema = NULL,
    config = NULL,
    path   = NULL,

    #' @description
    #' Constructor for initializing the configuration. Checks the local machine
    #' for existing configuration file is load_config = TRUE. Ensures that all
    #' the project configuration values are included.
    #'
    #' @param load_config load configuration from local machine if available
    #' @param config_path location of the used/stored configuration json file
    #'
    initialize = function(load_config = TRUE, config_path = NULL) {
      self$schema <- self$get_default_schema()
      self$config <- self$get_default_config()

      if (is.null(config_path)) {
        config_dir <- rappdirs::user_config_dir("verifyr2")
        self$path  <- file.path(config_dir, "config.json")
      } else {
        self$path <- config_path
      }

      if (load_config && file.exists(self$path)) {
        file_config <- jsonlite::read_json(self$path, simplifyVector = TRUE)
        self$config <- merge_values(self$get_default_schema(), file_config)
        self$config <- self$get_default_config() |> merge_values(self$config)
      }
    },

    #' @description
    #' Mehod for getting configuration value based on configuration key.
    #' Configuratio item children are separated with a dot in the key notation.
    #'
    #' @param key configuration property key for which to get the value
    #'
    get = function(key) {
      get_nested_value(self$config, key)
    },

    #' @description
    #' Mehod for setting configuration value based on configuration key.
    #' Configuration item children are separated with a dot in the key notation.
    #'
    #' @param key   configuration property key for which to get the value
    #' @param value value to set for the specified configuration key
    #'
    set = function(key, value) {
      self$config <- set_nested_value(self$config, key, value)
    },

    #' @description
    #' Method for saving the current configuration data into local machine.
    #'
    save = function() {
      dir.create(dirname(self$path), showWarnings = FALSE, recursive = TRUE)
      jsonlite::write_json(self$config, self$path, pretty = TRUE)
    },

    #' @description
    #' Helper method for getting configuration default values. These default
    #' values will be used in the configuration in case the configuration
    #' properties are not present previously.
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
      defaults
    },

    #' @description
    #' Method for getting the full configuration schema. Apart from the
    #' configuration data, the schema contains property descriptions as well as
    #' all possible values for the configuration properties.
    #'
    get_default_schema = function() {
      schema <- list(
        generic = list(
          description = "Generic options",
          debug = list(
            description = "Debugging enabled",
            options = c("yes", "no"),
            default = "no"
          ),
          images = list(
            description = "Process embedded images",
            options = c("yes", "no"),
            default = "yes"
          )
        ),
        rtf = list(
          description = "RTF comparison (summary and details)",
          images = list(
            description = "Process embedded images",
            options = c("yes", "no"),
            default = "yes"
          )
        ),
        pdf = list(
          description = "PDF comparison (summary)",
          details = list(
            description = "Process PDF detailed comparison",
            options = c("yes", "no"),
            default = "yes"
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

      if (!check_magick_available()) {
        schema[["generic"]][["images"]] <- list(
          description = "Process embedded images (missing magick library)",
          options = c("no"),
          default = "no"
        )

        schema[["rtf"]][["images"]] <- list(
          description = "Process embedded images (missing magick library)",
          options = c("no"),
          default = "no"
        )
      }

      if (!check_pdftools_available()) {
        schema[["pdf"]][["details"]] <- list(
          description = "Process PDF details (missing pdftools library)",
          options = c("no"),
          default = "no"
        )
      }

      schema
    }
  )
)
