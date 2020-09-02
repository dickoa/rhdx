#' HDX Showcase
#'
#' HDX Showcase
#'
#' @format NULL
#' @usage NULL
Showcase <- R6::R6Class(
  classname = "Showcase",
  inherit = HDXObject,

  private = list(
    configuration = NULL
  ),

  public = list(
    #' @field datasets list of datasets using this showcase
    datasets = NULL,
    #' @field data the field info into list
    data = NULL,

    #' @description
    #' Create a new Showcase object
    #'
    #' @param initial_data list, data with required field to create Showcase
    #' @param configuration Configuration, configuration to use
    #' @return a new Showcase object
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration")) {
        private$configuration <- get_rhdx_config()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data))
        initial_data <- list()
      initial_data <- drop_nulls(initial_data)
      self$data <- initial_data
      key <- names(initial_data)
      if ("dataset" %in% key)
        self$datasets <- lapply(self$data$datasets,
                                function(x)
                                  Dataset$new(initial_data = x,
                                              configuration = configuration))

    },

    #' @description
    #' List datasets using the Showcase
    #' @return a list of dataset
    get_datasets = function() {
      configuration <- private$configuration
      showcase_id <- self$data$id
      res <- configuration$call_action("ckanext_showcase_package_list",
                                       body = list(showcase_id = showcase_id),
                                       verb = "post")
      list_of_ds <- lapply(res, function(x)
        Dataset$new(initial_data = x,
                    configuration = configuration))
      class(list_of_ds) <- "datasets_list"
      list_of_ds
    },


    #' @description
    #' Browse the Showcase page on HDX
    browse = function() {
      url <- private$configuration$get_hdx_site_url()
      browseURL(url = paste0(url, "showcase/", self$data$name))
    },

    #' @description
    #' Get dataset field into list
    #'
    #' @return a list with showcase field info
    as_list = function() {
      self$data
    },

    #' @description
    #' Print a Showcase object
    print = function() {
      cat(paste0("<HDX Showcase> ", self$data$id), "\n")
      cat("  Title: ", self$data$title, "\n", sep = "")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Description: ", self$data$notes, "\n", sep = "")
      cat("  Type: ", self$data$type, "\n", sep = "")
      invisible(self)
    }
  )
)

#' @export
#' @aliases Showcase
#' @importFrom tibble as_tibble
as_tibble.Showcase <- function(x) {
  df <- tibble::tibble(
    showcase_id = x$data$id,
    showcase_name = x$data$name)
  df$showcase <- list(x)
  df
}

#' @export
#' @aliases Showcase
as.list.Showcase <- function(x, ...) {
  x$as_list()
}

#' @noRd
.pull_showcase <- function(identifier = NULL, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  res <- configuration$call_action("ckanext_showcase_show",
                                   body = list(id = identifier),
                                   verb = "post")
  Showcase$new(initial_data = res, configuration = configuration)
}

#' Read Showcase
#'
#' Read HDX Showcase
#'
#' @importFrom memoise memoise
#'
#' @param identifier Character Showcase name or id
#' @param configuration Configuration an HDX configuration object
#'
#' @details Delete resource from dataset
#'
#'
#' @return A showcase
#' @export
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use HDX default server
#'  pull_showcase("fts-requirements-and-funding-data-for-zimbabwe-showcase") # first resource
#' }
pull_showcase <- memoise(.pull_showcase)
