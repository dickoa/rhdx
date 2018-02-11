#' Create HDX Dataset
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
#' dataset <- Dataset$read_from_hdx("acled-conflict-data-for-africa-realtime-2016")
#' dataset
#' 
Dataset <- R6::R6Class(
  "Dataset",
  private = list(
    configuration = NULL
  ),
  public = list(
    resources = NULL,
    data = list(),
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration)) {
        private$configuration <- Configuration$read()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data)) initial_data <- list()
      initial_data <- nc(initial_data)
      key <- names(initial_data)
      self$init_resources()
      self$data <- initial_data
      ## self$check_required_field()
      if ("resources" %in% key) {
        self$resources <- purrr::map(self$data$resources, ~ Resource$new(initial_data = .x, configuration = configuration))
      }
    },
    init_resources = function() {
      self$data$resources <- list()
    },
    read_from_hdx = function(identifier, configuration = NULL) {
      if (is.null(configuration))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("package_show", list(id = identifier))
      Dataset$new(initial_data = res$result, configuration = configuration)
    },
    get_resources = function() {
      self$resources
    },
    search_in_hdx = function(query = "*:*", rows = 10L, page_size = 1000L, configuration = NULL, ...) {
      if (is.null(configuration))
        configuration <- private$configuration
      cc <- crul::Paginator$new(client = configuration$get_remoteclient(),
                               by = "query_params",
                               limit_param = "rows",
                               offset_param = "start",
                               limit = rows,
                               limit_chunk = page_size)
      suppressMessages(cc$get(path = paste0("/api/3/action/", "package_search"), list(q = query, ...)))
      ds_list <- purrr::flatten(purrr::map(cc$parse(), ~ jsonlite::fromJSON(.x, simplifyVector = FALSE)$result$results))
      ds_list <- purrr::map(ds_list, ~ Dataset$new(initial_data = .x, configuration = configuration))
      ds_list
    },
    update_from_yaml = function(hdx_dataset_static_yaml) {
      self$data <- yaml::yaml.load_file(hdx_dataset_static_yaml)
      if ("resources" %in% names(self$data))
        self$resources <- purrr::map(self$data$resources, ~ Resource$new(initial_data = .x, configuration = configuration))
    },
    update_from_json = function(hdx_dataset_static_json) {
      self$data <- jsonlite::fromJSON(hdx_dataset_static_json, simplifyVector = FALSE)
      if ("resources" %in% names(self$data))
        self$resources <- purrr::map(self$data$resources, ~ Resource$new(initial_data = .x, configuration = configuration))
    },
    get_configuration = function() {
      private$configuration
    },
    get_dataset_date = function() {
      date <- self$data$dataset_date
      if (is.null(date))
        date <- ""
      date
    },
    set_dataset_date = function(date) {
      self$data$dataset_date <- date
    },
    get_update_frequency = function() {
      self$data$data_update_frequency
    },
    set_update_frequency = function(frequency) {
      self$data$data_update_frequency <- frequency
    },
    get_tags = function() {
      self$data$tags
    },
    add_tags = function(tags) {
      self$data$tags <- purrr::map(tags, function(tag) list(names = tag))
    },
    get_locations = function() {
      self$data$groups
    },
    add_locations = function(locations) {
      self$data$groups <- purrr::map(locations, function(location) list(names = location))
    },
    get_maintainer = function() {
      self$data$maintainer
    },
    set_maintainer = function(maintainer) {
      self$data$maintainer <- maintainer
    },
    get_organization = function() {
      self$data$owner_org
    },
    set_organization = function(organization) {
      self$data$owner_org <- organization
    },
    is_requestable = function() {
      self$data$is_requestdata_type
    },
    set_requestable = function(requestable = TRUE) {
      self$data$is_requestable_type <- requestable
      if (requestable)
        self$data$private <- FALSE
    },
    check_required_field = function() {
      n2 <- names(self$data)
      if (!is.null(self$is_requestable()) && self$is_requestable()) {
        n1 <- private$configuration$data$`dataset-requestable`$required_fields
      } else {
        n1 <- private$configuration$data$dataset$required_fields
      }
      if (!all(n1 %in% n2)) stop(sprintf("Field %s is missing in the dataset!", setdiff(n1, n2)))
    },
    count = function(configuration = NULL) {
      if (is.null(configuration))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("package_search", list(q = "*:*", rows = 1L))
      res$result$count
    },
    as_list = function() {
      self$data
    },
    print = function() {
      if (!is.null(self$is_requestable()) && self$is_requestable()) {
        cat(paste0("<HDX Requestable Dataset> ", self$data$id), "\n")
        cat("  Title: ", self$data$title, "\n", sep = "")
        cat("  Name: ", self$data$name, "\n", sep = "")
        cat("  Date: ", self$get_dataset_date(), "\n", sep = "")
        cat("  Tags (up to 5): ", sift_res(self$data$tags), "\n", sep = "")
        cat("  Locations (up to 5): ", sift_res(self$data$groups, "title"), "\n", sep = "")
      } else {
        cat(paste0("<HDX Dataset> ", self$data$id), "\n")
        cat("  Title: ", self$data$title, "\n", sep = "")
        cat("  Name: ", self$data$name, "\n", sep = "")
        cat("  Date: ", self$get_dataset_date(), "\n", sep = "")
        cat("  Tags (up to 5): ", sift_res(self$data$tags), "\n", sep = "")
        cat("  Locations (up to 5): ", sift_res(self$data$groups, "title"), "\n", sep = "")
        cat("  Resources (up to 5): ", sift_res(self$data$resources), "\n", sep = "")
      }
      invisible(self)
    }
  )
)

#' @aliases Dataset 
Dataset$read_from_hdx <- function(identifier, configuration = NULL, ...) {
  ds <- Dataset$new()
  ds$read_from_hdx(identifier, configuration = configuration, ...)
}

#' @aliases Dataset 
Dataset$search_in_hdx <- function(query = "*:*", rows = 10L, page_size = 1000L, configuration = NULL, ...) {
  ds <- Dataset$new()
  ds$search_in_hdx(query = query, rows = rows, page_size = page_size, configuration = configuration, ...)
}
  
#' @aliases Dataset 
Dataset$count <- function(configuration = NULL, ...) {
  ds <- Dataset$new()
  ds$count(configuration = configuration)
}

#' @export
#' @aliases Dataset 
as.list.Dataset <- function(dataset) {
  dataset$as_list()
}

#' @export
#' @aliases Dataset 
as_tibble.Dataset <- function(x, ...) {
  df <- data_frame(dataset_title = tolower(x$data$title),
                  dataset_name = x$data$name,
                  dataset_date = x$get_dataset_date(),
                  requestable = x$is_requestable(),
                  locations_name = purrr::map(x$get_location(), ~ .[["name"]]),
                  organization_name = x$data$organization$name)  
  df$resources_format <- list(tolower(purrr::map_chr(x$get_resources(), function(l) l$get_file_type())))    
  df$tags_name <- list(tolower(purrr::map_chr(x$get_tags(), function(l) l$name)))
  df$resources <- list(x$get_resources())
  df$dataset <- list(x)
  df  
}


