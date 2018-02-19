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
#' \dontrun{
#' # ---------------------------------------------------------
#' Configuration$create(hdx_site = "demo")
#' resource <- Resource$read_from_hdx("98aa1742-b5d3-40c3-94c6-01e31ded6e84")
#' resource
#' }
#' 
#'
Resource <- R6::R6Class(
  "Resource",
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
    update_from_yaml = function(hdx_resource_static_yaml) {
      self$data <- yaml::yaml.load_file(hdx_resource_static_yaml)
    },
    update_from_json = function(hdx_resource_static_json) {
      self$data <- jsonlite::fromJSON(hdx_resource_static_json, simplifyVector = TRUE)
    },
    touch = function() {
      private$configuration$call_remoteclient("resource_patch", list(id = self$data$id))
    },
    download = function(folder = getwd(), filename = NULL, quiet = FALSE, force = FALSE, ...) {
      if (is.null(filename))
        filename <- basename(self$data$url)
      path <- file.path(folder, filename)
      ## cached_files <- file.path(rhdx_cache$cache_path_get(), filename)
      if (file.exists(path) & !force) {
        message("File already present, at: ", path)
      } else {
        download.file(url = self$data$url, destfile = path, mode = "wb", quiet = quiet, ...)
      }
      invisible(path)
    },
    read_session = function(sheet = NULL, layer = NULL, folder = getwd(), json_simplifyVector = FALSE) {
      path <- self$download(folder = folder, quiet = TRUE)
      format <- self$get_file_type()
      switch(
        format,
        csv = {
          check4X("readr")
          readr::read_csv(path, comment = "#")
        },
        excel = read_sheet(path = path, sheet = sheet, format = "xlsx"),
        xlsx = read_sheet(path = path, sheet = sheet, format = format),
        xls = read_sheet(path = path, sheet = sheet, format = format),
        json = {
          check4X("jsonlite")
          jsonlite::fromJSON(path, simplifyVector = json_simplifyVector)
        },
        geojson = read_vector(path, layer),
        `zipped shapefile` = read_vector(path = path, layer = layer),
        `zipped geodatabase` = read_vector(path = path, layer = layer, zipped = FALSE),
        `zipped geopackage` = read_vector(path = path, layer = layer),
        `zipped geotiff` = read_raster(path = path, layer = layer),
        kmz = read_spatial(path = path, layer = layer),
        `zipped kml` = read_spatial(path = path, layer = layer))
    },
    get_dataset = function() {
      package_id <- self$data$package_id
      if (is.null(package_id)) {
        stop("Resource has no package id!", call. = FALSE)
      } else {
        Dataset$read_from_hdx(package_id)        
      }
    },
    get_file_to_upload = function() {
      self$data$file_to_upload
    },
    set_file_to_upload = function(file_to_upload) {
      self$data$file_to_upload <- crul::upload(file_to_upload)
    },
    check_required_field = function(check_dataset_id = FALSE) {
      n2 <- names(self$data)
      n1 <- private$configuration$data$resource$required_fields
      if (check_datasetid) n1 <- setdiff(n1, "package_id")
      if (!all(n1 %in% n2)) stop(sprintf("Field %s is missing in the dataset!", setdiff(n1, n2)), call. = FALSE)
    },
    read_from_hdx = function(identifier, configuration = NULL) {
      if (is.null(configuration))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("resource_show", list(id = identifier))
      Resource$new(initial_data = res$result, configuration = configuration)
    },
    search_in_hdx = function(query = "*:*", configuration = NULL, ...) {
      if (is.null(configuration))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("resource_search", list(query = query, ...))
      list_of_rs <- lapply(res$result$results, function(x) Resource$new(initial_data = x, configuration = configuration))
      list_of_rs
    },
    get_file_type = function() {
      tolower(self$data$format)
    },
    set_file_type = function(file_type) {
      self$data$format <- file_type
    },
    as_list = function() {
      self$data
    },
    print = function() {
      cat(paste0("<HDX Resource> ", self$data$id), "\n")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Description: ", self$data$description, "\n", sep = "")
      cat("  Size: ", self$data$size, "\n", sep = "")
      cat("  Format: ", self$data$format, "\n", sep = "")
      invisible(self)
    }
  )
)

#' @aliases Resource 
Resource$read_from_hdx <- function(identifier = NULL, configuration = NULL, ...) {
  rs <- Resource$new()
  rs$read_from_hdx(identifier = NULL, configuration = configuration, ...)
}

#' @aliases Resource
Resource$search_in_hdx <- function(query = "*:*", configuration = NULL, ...) {
  rs <- Resource$new()
  rs$search_in_hdx(query = query, configuration = configuration, ...)
}

#' @export
#' @aliases Resource 
as_tibble.Resource <- function(x, ...) {
  df <- tibble::data_frame(
    resource_id = x$data$id,
    resource_name = x$data$name,
    resource_format = tolower(x$data$format),
    resource_url = x$data$url)
  df$resource <- list(x)
  df
}


#' @export
#' @aliases Resource 
as.data.frame.Resource <- function(x, ...) {
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
#' @aliases Resource 
as.list.Resource <- function(x) {
  x$as_list()
}


#' @export
#' @aliases Resource 
download_from_hdx <- function(resource, folder = getwd(), filename = NULL, quiet = FALSE, ...) {
  if (!inherits(resource, "Resource")) stop("Not a HDX Resource object!", call. = FALSE)
  resource$download(folder = folder, filename = filename, quiet = quiet, ...)
}

#' @export
#' @aliases Resource 
read_in_R <- function(resource, sheet = NULL, layer = NULL, folder = getwd(), json_simplifyVector = FALSE) {
  if (!inherits(resource, "Resource")) stop("Not a HDX Resource object!", call. = FALSE)
  resource$read_session(sheet = sheet, layer = layer, folder = folder, json_simplifyVector = json_simplifyVector)
}
