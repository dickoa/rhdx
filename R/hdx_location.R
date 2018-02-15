#' Create and manipulate HDX Location
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
#' org <- Location$read_from_hdx("ocha-mali", include_dataset = TRUE)
#' org
#' 
#' @export
Location <- R6::R6Class(
  "Location",
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
    update_from_yaml = function(hdx_location_static_yaml) {
      self$data <- yaml::yaml.load_file(hdx_location_static_yaml)
    },
    update_from_json = function(hdx_location_static_json) {
      self$data <- jsonlite::fromJSON(hdx_location_static_json, simplifyVector = TRUE)
    },
    list = function(sort = "name asc", configuration = NULL, ...) {
      if (!sort %in% c("name asc", "name", "package_count", "title")) stop("You can just sort by the following parameters `name asc`, `name`, `package_count` or `title`")
      if (is.null(configuration))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("location_list", list(sort = sort, ...))
      unlist(res$result)
    },
    get_datasets = function() {
      if (!"packages" %in% names(self$data)) stop("No datasets available, please run again Location$read_from_hdx with `include_dataset = TRUE` and try again!")
      list_of_ds <- lapply(self$data$packages, function(x) Dataset$new(initial_data = x))
      list_of_ds
    },
    as_list = function() {
      self$data
    },
    print = function(x, ...) {
    cat(paste0("<HDX Location> ", self$data$id), "\n")
    cat("  Name: ", self$data$name, "\n", sep = "")
    cat("  Description: ", self$data$description, "\n", sep = "")
    invisible(self)
    }
  )
)

#' @aliases Location
Location$read_from_hdx <- function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
  org <- Location$new()
  org$read_from_hdx(identifier = identifier, include_datasets = include_datasets, configuration = configuration, ...)
}

#' @aliases Location
Location$search_in_hdx <- function(query = "*:*", configuration = NULL, ...) {
  org <- Location$new()
  org$search_in_hdx(query = query, configuration = configuration, ...)
}

#' @aliases Location
Location$get_all_location_names <- function(sort = "name asc", configuration = NULL, ...) {
  org <- Location$new()
  org$get_all_location_names(sort = sort, configuration = configuration, ...)
}

#' @aliases Location
Location$get_datasets <- function(identifier = NULL, configuration = NULL, ...) {
  org <- Location$new()
  res <- org$read_from_hdx(identifier = identifier, include_datasets = TRUE, configuration = configuration, ...)
  res$get_datasets()
}

 
#' @export
#' @aliases Location 
as_tibble.Location <- function(x, ...) {
  df <- tibble::data_frame(
    location_id = x$data$id,
    location_name = x$data$name)
  df$location <- list(x)
  df
}


#' @export
#' @aliases Location 
as.data.frame.Location <- function(x, ...) {
  df <- data.frame(
    location_id = x$data$id,
    location_name = x$data$name,
    stringsAsFactors = FALSE)
  df$location <- list(x)
  df
}

#' @export
#' @aliases Location 
as.list.Location <- function(x) {
  x$as_list()
}
