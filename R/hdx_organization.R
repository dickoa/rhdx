#' Create and manipulate HDX Organization
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
#' org <- Organization$read_from_hdx("ocha-mali", include_dataset = TRUE)
#' org
#' 
#' @export
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
    read_from_hdx = function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
      if (is.null(configuration))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("organization_show", list(id = identifier, include_datasets = include_datasets, ...))
      Organization$new(initial_data = res$result, configuration = configuration)
    },
    list_all_organizations = function(sort = "name asc", all_fields = FALSE, include_groups = FALSE, configuration = NULL, ...) {
      if (!sort %in% c("name asc", "name", "package_count", "title")) stop("You can just sort by the following parameters `name asc`, `name`, `package_count` or `title`")
      if (is.null(configuration))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("organization_list", list(sort = sort, all_fields = all_fields, include_groups = include_groups, ...))
      if (!all_fields)
        unlist(res$result)
      res$result
    },
    get_datasets = function() {
      if (!"packages" %in% names(self$data)) stop("No datasets available, please run Organization$read_from_hdx with `include_datasets = TRUE` and try again!")
      list_of_ds <- lapply(self$data$packages, function(x) Dataset$new(initial_data = x))
      list_of_ds
    },
    as_list = function() {
      self$data
    },
    print = function(x, ...) {
      cat(paste0("<HDX Organization> ", self$data$id), "\n")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Display name: ", self$data$display_name, "\n", sep = "")
      cat("  No. Datasets: ", self$data$package_count, "\n", sep = "")
      cat("  No. Members: ", length(self$data$users), "\n", sep = "")
      invisible(self)
    }
  )
)

#' @aliases Organization
Organization$read_from_hdx <- function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
  org <- Organization$new()
  org$read_from_hdx(identifier = identifier, include_datasets = include_datasets, configuration = configuration, ...)
}

#' @aliases Organization
Organization$search_in_hdx <- function(query = "*:*", configuration = NULL, ...) {
  org <- Organization$new()
  org$search_in_hdx(query = query, configuration = configuration, ...)
}

#' @aliases Organization
Organization$list_all_organizations <- function(sort = "name asc", all_fields = FALSE, include_groups = FALSE, configuration = NULL, ...) {
  org <- Organization$new()
  org$list_all_organizations(sort = sort, all_fields = all_fields, include_groups = include_groups, configuration = configuration, ...)
}

#' @aliases Organization
Organization$get_datasets <- function(identifier = NULL, configuration = NULL, ...) {
  org <- Organization$new()
  res <- org$read_from_hdx(identifier = identifier, include_datasets = TRUE, configuration = configuration, ...)
  res$get_datasets()
}

 
#' @export
#' @aliases Organization 
as_tibble.Organization <- function(x, ...) {
  df <- tibble::data_frame(
    organization_id = x$data$id,
    organization_name = x$data$name)
  df$organization <- list(x)
  df
}


#' @export
#' @aliases Organization 
as.data.frame.Organization <- function(x, ...) {
  df <- data.frame(
    organization_id = x$data$id,
    organization_name = x$data$name,
    stringsAsFactors = FALSE)
  df$organization <- list(x)
  df
}

#' @export
#' @aliases Organization 
as.list.Organization <- function(x) {
  x$as_list()
}
