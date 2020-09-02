#' HDXObject abstract class
#'
#' HDXObject class containing all logic for accessing,
#' creating, and updating HDX objects.
HDXObject <- R6::R6Class(
  classname = "HDXObject",
  private = list(
    configuration = NULL
  ),
  public = list(
    #' @field data placeholder for HDXObject field element
    data = list(),
    #' @description
    #' Create a new HDXObject object
    #'
    #' @param initial_data list with required field to create a HDXObject
    #' @param configuration a Configuration object
    #' @return A HDXObject object
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration")) {
        private$configuration <- get_rhdx_config()
      } else {
        private$configuration <- configuration
      }
    },

    #' @description
    #' Update metadata from yaml file
    #'
    #' @importFrom yaml read_yaml
    #' @param path (character) Path to YAML metadata
    update_from_yaml = function(path) {
      self$data <- read_yaml(path)
    },

    #' @description
    #' Update metadata from json file
    #'
    #' @importFrom jsonlite read_json
    #' @param path (character) Path to JSON metadata
    update_from_json = function(path) {
      self$data <- read_json(path)
    },


    #' @description
    #' Get HDXObject required fields
    #'
    #' @return list of required fields for a resource
    get_required_fields = function() {
      ""
    },

    #' @description
    #' Check HDXobject required field
    #'
    #' @return a logical value, TRUE if the the resource is not missing
    #' a required field and throws an error otherwise
    check_required_field = function() {
      FALSE
    },

    #' @description
    #' Get HDXObject field into list
    #'
    #' @return a list with HDXObject field
    as_list = function() {
      self$data
    },

    #' @description
    #' Browse HDX
    browse = function() {
      url <- private$configuration$get_hdx_site_url()
      browseURL(url = url)
    },

    #' @description
    #' Get the current configuration in use
    #'
    #' @return A configuration object, the configuration in use
    get_configuration = function() {
      private$configuration
    },

    #' @description
    #' Print a Dataset object
    print = function() {
      cat(paste0("<HDX Object> ", self$data$id), "\n")
      cat("This is an abstract class!\n")
      invisible(self)
    }
  )
)
