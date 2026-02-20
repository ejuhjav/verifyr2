
generic_level_desc <- paste0(
  "Option to define whether the comparison is processed and displayed on ",
  "word or row level. Word-level processing requires additional time for ",
  "comparison calculation, so it may be advisable to disable this option ",
  "when working with large files if row-level comparison is sufficient."
)

generic_images_desc <- paste0(
  "Option to define whether embedded images should be processed for all ",
  "supported file types. Processing embedded images requires additional ",
  "time to detect and extract images from documents, so it may be ",
  "advisable to disable this option when working with large files or a ",
  "high volume of files that do not contain images."
)

generic_spaces_desc <- paste0(
  "Option to define whether differences in whitespace should be ignored ",
  "during comparison. When enabled, differences caused solely by additional ",
  "spaces or tab characters are not reported."
)

generic_debug_desc <- paste0(
  "Option to enable debugging so that the application prints diagnostic ",
  "information to the output stream. This option is available to all users ",
  "and can be enabled to include debugging details when reporting issues."
)

rtf_images_desc <- paste0(
  "Option to define whether embedded images should be processed for RTF file ",
  "types. Processing embedded images requires additional time to detect and ",
  "extract images from documents, so it may be advisable to disable this ",
  "option when working with large files or a high volume of files that do ",
  "not contain images."
)

pdf_details_desc <- paste0(
  "Option to define whether detailed PDF comparison should be performed. ",
  "Disabling this option is generally not recommended for users. However, it ",
  "may be automatically disabled by the application if the required ",
  "'pdftools' package is not available."
)

details_mode_desc <- paste0(
  "Option to define the default display mode for the details summary view. ",
  "The available options are 'full,' which displays the complete compared ",
  "file, and 'summary,' which displays only the detected differences."
)

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
    #' Method for getting schema item based on configuration key.
    #' Configuration item children are separated with a dot in the key notation.
    #'
    #' @param key configuration property key for which to get the schema item
    #'
    get_schema_item = function(key) {
      get_nested_value(self$schema, key)
    },

    #' @description
    #' Method for getting configuration value based on configuration key.
    #' Configuration item children are separated with a dot in the key notation.
    #'
    #' @param key configuration property key for which to get the value
    #'
    get = function(key) {
      get_nested_value(self$config, key)
    },

    #' @description
    #' Method for setting configuration value based on configuration key.
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
          if (key != "title") {
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
          title = "Generic options",
          level = list(
            title   = "Comparison level",
            options = c("word", "row"),
            default = "word",
            reload  = TRUE,
            desc    = generic_level_desc
          ),
          spaces = list(
            titlei  = "Ignore empty space differences",
            options = c("yes", "no"),
            default = "no",
            reload  = TRUE,
            desc    = generic_spaces_desc
          ),
          images = list(
            title   = "Process embedded images",
            options = c("yes", "no"),
            default = "yes",
            reload  = TRUE,
            desc    = generic_images_desc
          ),
          debug = list(
            title   = "Debugging enabled",
            options = c("yes", "no"),
            default = "no",
            reload  = FALSE,
            desc    = generic_debug_desc
          )
        ),
        rtf = list(
          title = "RTF comparison (summary and details)",
          images = list(
            title   = "Process embedded images",
            options = c("yes", "no"),
            default = "yes",
            reload  = TRUE,
            desc    = rtf_images_desc
          )
        ),
        pdf = list(
          title = "PDF comparison (summary)",
          details = list(
            title = "Process PDF detailed comparison",
            options = c("yes", "no"),
            default = "yes",
            reload  = TRUE,
            desc    = pdf_details_desc
          )
        ),
        details = list(
          title = "Details comparison",
          mode = list(
            title = "Mode",
            options = c("full", "summary"),
            default = "summary",
            reload  = FALSE,
            desc    = details_mode_desc
          )
        )
      )

      if (!check_magick_available()) {
        schema[["generic"]][["images"]] <- list(
          title   = "Process embedded images (missing magick library)",
          options = c("no"),
          default = "no",
          reload  = TRUE,
          desc    = generic_images_desc
        )

        schema[["rtf"]][["images"]] <- list(
          title   = "Process embedded images (missing magick library)",
          options = c("no"),
          default = "no",
          reload  = TRUE,
          desc    = rtf_images_desc
        )
      }

      if (!check_pdftools_available()) {
        schema[["pdf"]][["details"]] <- list(
          title   = "Process PDF details (missing pdftools library)",
          options = c("no"),
          default = "no",
          reload  = TRUE,
          desc    = pdf_details_desc
        )
      }

      schema
    }
  )
)
