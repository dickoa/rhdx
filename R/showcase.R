#' HDX Showcase
#'
#' HDX Showcase
#'
#' @format NULL
#' @usage NULL
Showcase <- R6::R6Class(
  "Showcase",

  private = list(
    configuration = NULL
  ),

  public = list(
    datasets = NULL,
    data = NULL,
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration")) {
        private$configuration <- configuration_read()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data)) initial_data <- list()
      initial_data <- drop_nulls(initial_data)
      self$data <- initial_data
      key <- names(initial_data)
      if ("dataset" %in% key)
        self$datasets <- lapply(self$data$datasets,
                                function(x) Dataset$new(initial_data = x, configuration = configuration))

    },

    pull = function(identifier = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("ckanext_showcase_show", list(id = identifier))
      Showcase$new(initial_data = res$result, configuration = configuration)
    },

    list_datasets = function() {
      configuration <- private$configuration
      showcase_id <- self$data$id
      res <- configuration$call_remoteclient("ckanext_showcase_list", data = list(showcase_id = showcase_id))
      if (res$status_code != 200L)
        stop("Dataset not added to the showcase")
      res$result
    },

    list_tags = function() {
      configuration <- private$configuration
      showcase_id <- self$data$id
      res <- configuration$call_remoteclient("ckanext_showcase_list", list(showcase_id = showcase_id))
      if (res$status_code != 200L)
        stop("Tag not added to the showcase")
      res$result
    },

    browse = function() {
      url <- private$configuration$get_hdx_site_url()
      browseURL(url = paste0(url, "showcase/", self$data$name))
    },

    as_list = function() {
      self$data
    },

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

#' @aliases Showcase
.pull_showcase <- function(identifier = NULL, configuration = NULL) {
  org <- Showcase$new()
  org$pull(identifier = identifier, configuration = configuration)
}


#' Read Showcase
#'
#' Read HDX Showcase
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
#'  delete_resource(dataset, 1) # first resource
#' }
pull_showcase <- memoise::memoise(.pull_showcase)
