#' Create an R6 reference object generator
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
#'  \item create - URL path, appended to the base URL
#'  \item read - query terms, as a named list
#'  \item delete - body as an R list
#'  \item setup - one of form, multipart, json, or raw
#' }
#'
#' @examples
#' # A queue ---------------------------------------------------------
#' Configuration$create(hdx_site = "demo"),
#' conf <- Configuration$read()
#' conf
#' 
Configuration <- R6::R6Class(
  "Configuration",
  private = list(
    remoteclient = NULL,
    static = .rhdx_env
  ),
  public = list(
    hdx_site = NULL,
    hdx_key = NULL,
    hdx_config = NULL,
    project_config = NULL,
    read_only = TRUE,
    default_hdx_key_file = "~/.hdxkey",
    data = list(),
    initialize = function(hdx_site = c("prod", "test", "feature", "demo"), hdx_key = NULL, read_only = TRUE, hdx_key_file = NULL, hdx_config = NULL, hdx_config_yaml = NULL, hdx_config_json = NULL, ...) {
      ## if (is.null(project_config) & is.null(hdx_config_yaml)) stop("More than one HDX configuration")
      if (is.null(hdx_config) & is.null(hdx_config_yaml)) {
        hdx_config <- yaml::yaml.load_file(system.file("config", "hdx_configuration.yml", package = "rhdx"))
      }
      self$data <- hdx_config
      self$hdx_site <- hdx_site
      hdx_site <- paste0("hdx_", match.arg(hdx_site), "_site")
      if (!is.null(hdx_key) && !read_only) {
        self$hdx_key <- hdx_key
      }
      private$remoteclient <- crul::HttpClient$new(url = self$data[[hdx_site]]$url,
                                                  headers = nc(list(`X-CKAN-API-Key` = self$hdx_key, `Content-Type` = "application/json")),
                                                  opts = list(http_version = 2)) ## http 1.1
    },
    get_credentials = function() {
      lapply(self$data[[paste0("hdx_", self$hdx_site, "_site")]][c("username", "password")], function(x) if (is.null(x)) x else rawToChar(base64enc::base64decode(x)))
    },
    set_read_only = function() {
      self$hdx_key = NULL
      private$remoteclient <- crul::HttpClient$new(url = self$data[[paste0("hdx_", self$hdx_site, "_site")]]$url, headers = list(`Content-Type` = "application/json"))
    },
    set_hdx_key = function(hdx_key) {
      self$hdx_key <- hdx_key
    },
    get_hdx_key = function() {
      self$hdx_key
    },
    load_hdx_key = function(path) {
      self$hdx_key <- readLines(path)
    },
    set_hdx_site = function(hdx_site = c("prod", "test", "feature", "demo")) {
      self$hdx_site <-  hdx_site
      hdx_site <- paste0("hdx_", match.arg(hdx_site), "_site")
      private$remoteclient <- crul::HttpClient$new(url = self$data[[hdx_site]]$url, headers = list(`X-CKAN-API-Key` = self$hdx_key, `Content-Type` = "application/json"))
    },
    get_hdx_site = function() {
      self$hdx_site
    },
    get_hdx_site_url = function() {
      self$data[[paste0("hdx_", self$hdx_site, "_site")]]$url
    },
    get_remoteclient = function() {
      private$remoteclient
    },
    call_remoteclient = function(action, data = NULL, ...) {
      res <- private$remoteclient$get(path = paste0("/api/3/action/", action), query = data, ...)
      list(status_code = res$status_code, result = jsonlite::fromJSON(res$parse(encoding = "UTF-8"), simplifyVector = FALSE)$result)
    },
    read = function() {
      if (is.null(private$static$configuration)) {
        stop("There is no HDX configuration! Use Configuration$create(...)")
      } else {
        private$static$configuration
      }
    },
    setup = function(configuration = NULL, ...) {
      if (is.null(configuration)) {
        private$static$configuration <- Configuration$new(...)
      } else {
        private$static$configuration <- configuration        
      }
    },
    create = function(configuration = NULL, ...) {
      if (!is.null(private$static$configuration)) {
        stop("Configuration already created! You can use Configuration$setup or rhdx_setup to modify the configuration")
      } else {
        self$setup(configuration, ...)
      }
    },
    delete = function() {
      private$static$configuration <- NULL
    },
    print = function() {
      cat("<HDX Configuration> ", sep = "\n")
      cat(paste0("  HDX site: ", self$hdx_site), sep = "\n")
      cat(paste0("  HDX site url: ", self$data[[paste0("hdx_", self$hdx_site, "_site")]]$url), sep = "\n")
      cat(paste0("  HDX API key: ", self$hdx_key), sep = "\n")
      invisible(self)
    }
  )
)


#' @aliases Configuration
Configuration$create <- function(configuration = NULL, ...) {
  conf <- Configuration$new()
  conf$create(configuration = configuration, ...)
}
  
#' @aliases Configuration
Configuration$setup <- function(configuration = NULL, ...) {
  conf <- Configuration$new()
  conf$setup(configuration = configuration, ...)
}

#' @aliases Configuration
Configuration$delete <- function() {
  conf <- Configuration$new()
  conf$delete()
}
  
#' @aliases Configuration
Configuration$read <- function() {
  conf <- Configuration$new()
  conf$read()
}

#' @export
#' @aliases Configuration
rhdx_setup <- function(hdx_site = c("prod", "test", "feature", "demo"), hdx_key = NULL, configuration = NULL, ...) {
  conf <- Configuration$new()
  conf$setup(configuration = configuration, hdx_site = hdx_site, hdx_key = hdx_key, ...)
}
