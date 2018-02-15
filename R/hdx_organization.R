#' Create HDX Resource
#'
#' R6 objects are essentially environments, structured in a way that makes them
#' look like an object in a more typical object-oriented language than R. They
#' support public and private members, as well as inheritance across different
#' packages.
#'
#' @export
#' @details
#' **Methods**
#'   \describe{
#'     \item{`create(path, query, disk, stream, ...)`}{
#'       Make a GET request
#'     }
#'     \item{`read(path, query, body, disk, stream, ...)`}{
#'       Make a POST request
#'     }
#'     \item{`delete(path, query, body, disk, stream, ...)`}{
#'       Make a PUT request
#'     }
#'     \item{`setup(path, query, body, disk, stream, ...)`}{
#'       Make a PATCH request
#'     }
#'     \item{`delete(path, query, body, disk, stream, ...)`}{
#'       Make a DELETE request
#'     }
#'     \item{`head(path, query, ...)`}{
#'       Make a HEAD request
#'     }
#'   }
#'
#' @format NULL
#' @usage NULL
#' @details Possible parameters (not all are allowed in each HTTP verb):
#' \itemize{
#'  \item read_from_hdx - query terms, as a named list
#'  \item search_in_hdx  - body as an R list
#'  \item update_from_yaml - one of form, multipart, json, or raw
#'  \item add_update_resource 
#'  \item add_update_resources - one of form, multipart, json, or raw
#' }
#'
#' @examples
#' # ---------------------------------------------------------
#' Configuration$create(hdx_site = "demo")
#' resource <- Resource$read_from_hdx("98aa1742-b5d3-40c3-94c6-01e31ded6e84")
#' resource
#' 
#' @export
#'
Organization <- R6::R6Class(
  "Organization",
  private = list(
    configuration = NULL
  ),
  public = list(
    data = NULL,
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration)) {
        private$configuration <- Configuration$read()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data)) initial_data <- list()
      initial_data <- nc(initial_data)
      self$data <- initial_data
    },
    update_from_yaml = function(hdx_organization_static_yaml) {
      self$data <- yaml::yaml.load_file(hdx_organization_static_yaml)
    },
    update_from_json = function(hdx_organization_static_json) {
      self$data <- jsonlite::fromJSON(hdx_organization_static_json, simplifyVector = TRUE)
    },
    read_from_hdx = function(identifier, configuration = NULL) {
      if (is.null(configuration))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("organization_show", list(id = identifier))
      Organization$new(initial_data = res$result, configuration = configuration)
    },
    search_in_hdx = function(query = "*:*", configuration = NULL, ...) {
      if (is.null(configuration))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("organization_search", list(query = query, ...))
      list_of_org <- lapply(res$result$results, function(x) Organization$new(initial_data = x, configuration = configuration))
      list_of_org
    },
    get_datasets = function(query = "*:*", configuration = NULL, ...) {
      if (is.null(configuration))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("organization_", list(query = query, ...))
      list_of_org <- lapply(res$result$results, function(x) Organization$new(initial_data = x, configuration = configuration))
      list_of_org
    },
    as_list = function() {
      self$data
    },
    print = function(x, ...) {
      cat(paste0("<HDX Organization> ", self$data$id), "\n")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Description: ", self$data$description, "\n", sep = "")
      invisible(self)
    }
  )
)

#' @aliases Resource 
Organization$read_from_hdx <- function(identifier, configuration = NULL, ...) {
  rs <- Organization$new()
  rs$read_from_hdx(identifier, configuration = configuration, ...)
}

#' @aliases Organization
Organization$search_in_hdx <- function(query = "*:*", configuration = NULL, ...) {
  rs <- Organization$new()
  rs$search_in_hdx(query = query, configuration = configuration, ...)
}

#' @export
#' @aliases Organization 
as_tibble.Organization <- function(x, ...) {
  df <- tibble::data_frame(
    resource_id = x$data$id,
    resource_name = x$data$name,
    resource_format = tolower(x$data$format),
    resource_url = x$data$url)
  df$resource <- list(x)
  df
}


#' @export
#' @aliases Organization 
as.data.frame.Organization <- function(x, ...) {
  df <- data.frame(
    resource_id = x$data$id,
    resource_name = x$data$name,
    resource_format = tolower(x$data$format),
    resource_url = x$data$url,
    stringsAsFactors = FALSE)
  df$resource <- list(x)
  df
}

#' @export
#' @aliases Organization 
as.list.Organization <- function(x) {
  x$as_list()
}
