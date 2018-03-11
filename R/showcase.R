#' Create and manipulate HDX Showcase
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
#' org <- Showcase$read_from_hdx("ocha-mali", include_dataset = TRUE)
#' org
#' }
#' 
#' @export
Showcase <- R6::R6Class(
  "Showcase",
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
    read_from_hdx = function(identifier = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("ckanext_showcase_show", list(id = identifier))
      Showcase$new(initial_data = res$result, configuration = configuration)
    },
    list_all_showcases = function(configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("ckanext_showcase_list")
      res$result
    },
    create_in_hdx = function() {
      configuration <- private$configuration
      ds <- nc(self$data)
      res <- configuration$call_remoteclient("ckanext_showcase_create",
                                            ds,
                                            verb = "post",
                                            encode = "json")
      if (res1$status_code != 200L)
        stop("Showcase not created check the parameters")
    },
    delete_from_hdx = function(name = NULL) {
      configuration <- private$configuration
      ds <- nc(self$data)
      res <- configuration$call_remoteclient("ckanext_showcase_delete",
                                            list(name = name),
                                            verb = "post",
                                            encode = "json")
      if (res1$status_code != 200L)
        stop("Showcase not created check the parameters")

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
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Display name: ", self$data$display_name, "\n", sep = "")
      cat("  No. Datasets: ", self$data$package_count, "\n", sep = "")
      cat("  No. Members: ", length(self$data$users), "\n", sep = "")
      invisible(self)
    }
  )
)

#' @aliases Showcase
Showcase$read_from_hdx <- function(identifier = NULL, configuration = NULL) {
  org <- Showcase$new()
  org$read_from_hdx(identifier = identifier, configuration = configuration, ...)
}


#' @aliases Showcase
Showcase$list_all_showcases <- function(sort = "name asc", all_fields = FALSE, include_groups = FALSE, configuration = NULL, ...) {
  org <- Showcase$new()
  org$list_all_showcases(sort = sort, all_fields = all_fields, include_groups = include_groups, configuration = configuration, ...)
}

 
#' @export
#' @aliases Showcase 
#' @importFrom tibble as_tibble
as_tibble.Showcase <- function(x, ...) {
  df <- tibble::data_frame(
    showcase_id = x$data$id,
    showcase_name = x$data$name)
  df$showcase <- list(x)
  df
}


#' @export
#' @aliases Showcase 
as.list.Showcase <- function(x) {
  x$as_list()
}

#' @export
#' @aliases Showcase
read_showcase <- function(identifier = NULL, configuration = NULL, ...) {
  org <- Showcase$new()
  org$read_from_hdx(identifier = identifier, configuration = configuration, ...)
}
