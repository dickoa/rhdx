#' Create and manipulate HDX User
#'
#' HDX user mostly countries
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
#' User$list_all_users()
#' 
#' @export
User <- R6::R6Class(
  "User",
  private = list(
    configuration = NULL
  ),
  public = list(
    data = NULL,
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration")) {
        private$configuration <- Configuration$read()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data)) initial_data <- list()
      initial_data <- nc(initial_data)
      self$data <- initial_data
    },
    update_from_yaml = function(hdx_user_static_yaml) {
      self$data <- yaml::yaml.load_file(hdx_user_static_yaml)
    },
    update_from_json = function(hdx_user_static_json) {
      self$data <- jsonlite::fromJSON(hdx_user_static_json, simplifyVector = TRUE)
    },
    read_from_hdx = function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("user_show", list(id = identifier, include_datasets = include_datasets, ...))
      User$new(initial_data = res$result, configuration = configuration)
    },
    list_all_users = function(order_by = "number_created_packages", configuration = NULL, ...) {
      if (!sort %in% c("name", "number_of_edits", "number_created_packages")) stop("You can just sort by the following parameters `name`, `number_of_edits` or `number_created_packages`")
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("user_list", list(order_by = order_by, ...))
      if (!all_fields)
        unlist(res$result)
      res$result
    },
    as_list = function() {
      self$data
    },
    print = function() {
      cat(paste0("<HDX User> ", self$data$id), "\n")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Display Name: ", self$data$display_name, "\n", sep = "")
      cat("  No Packages: ", self$data$number_created_packages, "\n", sep = "")
      cat("  No Edits: ", self$data$number_of_edits, "\n", sep = "")
      invisible(self)
    }
  )
)

#' @aliases User
User$read_from_hdx <- function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
  org <- User$new()
  org$read_from_hdx(identifier = identifier, include_datasets = include_datasets, configuration = configuration, ...)
}

#' @aliases User
User$list_all_users <- function(sort = "name asc", all_fields = FALSE, configuration = NULL, ...) {
  org <- User$new()
  org$lists_all_user(sort = sort, all_fields = all_fields, configuration = configuration, ...)
}

 
#' @export
#' @aliases User 
#' @importFrom tibble as_tibble
as_tibble.User <- function(x, ...) {
  df <- tibble::data_frame(
    user_id = x$data$id,
    user_name = x$data$name)
  df$user <- list(x)
  df
}

#' @export
#' @aliases User 
as.list.User <- function(x) {
  x$as_list()
}
