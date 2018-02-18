#' Configuration for HDX
#'
#' HDX Configuration allow to connect to a HDX server and setup project where you interact with Humanitarian Data Exchange
#' platfom.
#' 
#' @export
#' @details
#' **Methods**
#'   \describe{
#'     \item{`create(hdx_site, hdx_key, read_only, hdx_key_file, hdx_config, hdx_config_yaml, hdx_config_json, project_config, project_config_yaml, project_config_json, configuration)`}{
#'       Create a HDX Configuration 
#'     }
#'     \item{`setup(hdx_site, hdx_key, read_only, hdx_key_file, hdx_config, hdx_config_yaml, hdx_config_json, project_config, project_config_yaml, project_config_json, configuration)`}{
#'       Setup a HDX Configuration object
#'     }
#'     \item{`read()`}{
#'       Read a Configuration object
#'     }
#'     \item{`delete()`}{
#'       Delete configuration
#'     }
#'   }
#'
#' @format NULL
#' @usage NULL
#' @details Possible parameters (not all are allowed in each HTTP verb):
#' \itemize{
#'  \item create - URL path, appended to the base URL
#'  \item setup - one of form, multipart, json, or raw
#'  \item read - query terms, as a named list
#'  \item delete - body as an R list
#' }
#'
#' @examples
#' Configuration$setup(hdx_site = "demo"),
#' conf <- Configuration$read()
#' conf
#' 
Configuration <- R6::R6Class(
  "Configuration",
  private = list(
    remoteclient = NULL,
    shared = .rhdx_env
  ),
  public = list(
    hdx_site = NULL,
    hdx_key = NULL,
    hdx_config = NULL,
    project_config = NULL,
    read_only = TRUE,
    default_hdx_key_file = "~/.hdxkey",
    data = list(),
    initialize = function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE, hdx_key_file = NULL, hdx_config = NULL, project_config = NULL) {
      if (!hdx_site %in% c("prod", "test", "feature", "demo")) stop("hdx_site can be either `prod`, `test`, `feature` or `demo`")
      if (!is.null(hdx_config)) {
        if (is.character(hdx_config)) {
          if (!file.exists(hdx_config)) stop("HDX config file not found!")
          file_ext <- tools::file_ext(hdx_config)
          if (!file_ext %in% c("yml", "json")) stop("Only YAML and JSON configuration file are supported for the moment!")
          hdx_config <- switch(file_ext,
                              yml = yaml::yaml.load_file(hdx_config),
                              json = jsonlite::fromJSON(hdx_config, simplifyVector = FALSE))
        }        
      } else {
        hdx_config <- yaml::yaml.load_file(system.file("config", "hdx_configuration.yml", package = "rhdx"))
      } 
      if (!is.null(hdx_key) & !is.null(hdx_key_file)) stop ("You have two sources (key and key file) for your HDX API key, choose one!")
      if (is.null(hdx_key) & is.null(hdx_key_file))
        hdx_key <- readLines(self$default_hdx_key_file)
      if (!is.null(hdx_key_file))
        hdx_key <- readLines(hdx_key_file) 
      self$data <- hdx_config
      self$hdx_site <- hdx_site
      hdx_site <- paste0("hdx_", hdx_site, "_site")
      if (!read_only) 
        self$hdx_key <- hdx_key
      private$remoteclient <- crul::HttpClient$new(url = self$data[[hdx_site]]$url,
                                                  headers = nc(list(`X-CKAN-API-Key` = self$hdx_key, `Content-Type` = "application/json")),
                                                  opts = list(http_version = 2)) ## http 1.1
    },
    get_credentials = function() {
      lapply(self$data[[paste0("hdx_", self$hdx_site, "_site")]][c("username", "password")], function(x) if (is.null(x)) x else rawToChar(base64enc::base64decode(x)))
    },
    set_read_only = function() {
      self$hdx_key <- NULL
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
    set_hdx_site = function(hdx_site = "prod") {
      if (!hdx_site %in% c("prod", "test", "feature", "demo")) stop("hdx_site can be either `prod`, `test`, `feature` or `demo`")
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
      if (is.null(private$shared$configuration)) {
        stop("There is no HDX configuration! Use Configuration$create(...)")
      } else {
        private$shared$configuration
      }
    },
    setup = function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE, hdx_key_file = NULL, hdx_config = NULL, project_config = NULL, configuration = NULL) {
      if (!hdx_site %in% c("prod", "test", "feature", "demo")) stop("hdx_site can be either `prod`, `test`, `feature` or `demo`")
      if (!is.null(configuration)) {
        if (!inherits(configuration, "Configuration")) stop("Not a 'Configuration' object!")
        private$shared$configuration <- configuration        
      } else {
        private$shared$configuration <- Configuration$new(hdx_site = hdx_site, hdx_key = hdx_key, read_only = read_only, hdx_key_file = hdx_key_file, hdx_config = hdx_config, hdx_config_yaml =  hdx_config_yaml, hdx_config_json = hdx_config_json)
      }
    },
    create = function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE, hdx_key_file = NULL, hdx_config = NULL, project_config = NULL, configuration = NULL) {
      if (!hdx_site %in% c("prod", "test", "feature", "demo")) stop("hdx_site can be either `prod`, `test`, `feature` or `demo`")
      if (!is.null(private$shared$configuration)) {
        stop("Configuration already created! You can use Configuration$setup or rhdx_setup to modify the configuration")
      } else {
        self$setup(hdx_site = hdx_site, hdx_key = hdx_key, read_only = read_only, hdx_key_file = hdx_key_file, hdx_config = hdx_config, hdx_config_yaml =  hdx_config_yaml, hdx_config_json = hdx_config_json, configuration = configuration)
      }
    },
    delete = function() {
      private$shared$configuration <- NULL
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
Configuration$create <- function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE, hdx_key_file = NULL, hdx_config = NULL, project_config = NULL, configuration = NULL) {
  conf <- Configuration$new()
  conf$create(hdx_site = hdx_site, hdx_key = hdx_key, read_only = read_only, hdx_key_file = hdx_key_file, hdx_config = hdx_config, project_config = project_config, configuration = configuration)
}

#' @aliases Configuration
Configuration$setup <- function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE, hdx_key_file = NULL, hdx_config = NULL, project_config = NULL, configuration = NULL) {
  conf <- Configuration$new()
  conf$setup(hdx_site = hdx_site, hdx_key = hdx_key, read_only = read_only, hdx_key_file = hdx_key_file, hdx_config = hdx_config, project_config = project_config, configuration = configuration)
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
rhdx_setup <- function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE, hdx_key_file = NULL, hdx_config = NULL, project_config = NULL, configuration = NULL) {
  conf <- Configuration$new()
  conf$setup(hdx_site = hdx_site, hdx_key = hdx_key, read_only = read_only, hdx_key_file = hdx_key_file, hdx_config = hdx_config, project_config = project_config, configuration = configuration)
}
