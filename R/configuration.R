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
#' \dontrun{
#' set_rhdx_config(hdx_site = "demo")
#' get_rhd_config()
#' }
#' 
Configuration <- R6::R6Class(
  "Configuration",
  private = list(
    shared = .rhdx_env
  ),
  public = list(
    data = list(),
    initialize = function(hdx_site = "prod", hdx_key = NULL, hdx_config = NULL, read_only = TRUE) {
      if (!hdx_site %in% c("prod", "test", "feature", "demo")) stop("hdx_site can be either `prod`, `test`, `feature` or `demo`", call. = FALSE)
      if (is.null(hdx_config)) {
        hdx_config <- yaml::read_yaml(system.file("config", "hdx_configuration.yml", package = "rhdx"))
      } else {
        if (is.character(hdx_config)) {
          if (!file.exists(hdx_config)) stop("HDX config file not found!", call. = FALSE)
          file_ext <- tools::file_ext(hdx_config)
          if (!file_ext %in% c("yml", "json"))
            stop("Only YAML and JSON configuration file are supported for the moment!", call. = FALSE)
          hdx_config <- switch(file_ext,
                              yml = yaml::read_yaml(hdx_config),
                              json = jsonlite::fromJSON(hdx_config, simplifyVector = FALSE))
        }
      }
      self$data$hdx_config <- hdx_config
      self$data$hdx_site <- hdx_site
      hdx_site <- paste0("hdx_", hdx_site, "_site")
      self$data$hdx_key <- hdx_key
      self$data$read_only <- read_only
      headers <- list(`Content-Type` = "application/json")
      if (!read_only)
        headers <- nc(list(`X-CKAN-API-Key` = hdx_key, `Content-Type` = "application/json"))
      self$data$remoteclient <- crul::HttpClient$new(url = self$data$hdx_config[[hdx_site]]$url,
                                                    headers = headers,
                                                    opts = list(http_version = 2, useragent = get_user_agent())) ## http 1.1
      private$shared$configuration <- self
    },
    get_credentials = function() {
      hdx_site <- paste0("hdx_", self$data$hdx_site, "_site")
      lapply(self$data$hdx_config[[hdx_site]][c("username", "password")], function(x) if (is.null(x)) x else rawToChar(base64enc::base64decode(x)))
    },
    set_read_only = function(read_only = TRUE) {
      hdx_site <- paste0("hdx_", self$data$hdx_site, "_site")
      headers <- list(`Content-Type` = "application/json")
      if (!read_only)
        headers <- nc(list(`X-CKAN-API-Key` = self$data$hdx_key, `Content-Type` = "application/json"))
      self$data$remoteclient <- crul::HttpClient$new(url = self$data$hdx_config[[hdx_site]]$url,
                                                    headers = headers,
                                                    opts = list(http_version = 2, useragent = get_user_agent())) ## http 1.1
    },
    set_hdx_key = function(hdx_key) {
      self$data$hdx_key <- hdx_key
    },
    get_hdx_key = function() {
      self$data$hdx_key
    },
    load_hdx_key = function(path) {
      self$data$hdx_key <- readLines(path)
    },
    set_hdx_site = function(hdx_site = "prod") {
      if (!hdx_site %in% c("prod", "test", "feature", "demo")) stop("hdx_site can be either `prod`, `test`, `feature` or `demo`", call. = FALSE)
      self$data$hdx_site <-  hdx_site
      hdx_site <- paste0("hdx_", hdx_site, "_site")
      self$data$remoteclient <- crul::HttpClient$new(url = self$data$hdx_config[[hdx_site]]$url,
                                                    headers = nc(list(`X-CKAN-API-Key` = self$data$hdx_key, `Content-Type` = "application/json")),
                                                    opts = list(http_version = 2, useragent = get_user_agent()))
    },
    get_hdx_site = function() {
      self$data$hdx_site
    },
    get_hdx_site_url = function() {
      hdx_site <- paste0("hdx_", self$data$hdx_site, "_site")
      self$data$hdx_config[[hdx_site]]$url
    },
    remoteclient = function() {
      self$data$remoteclient
    },
    call_remoteclient = function(action, data = NULL, verb = "get", encode = "json", ...) {
      if (!verb %in% c("post", "get", "put", "patch"))
        stop("Only `get`, `post`, `put` and `patch` are supported!")
      res <- switch(verb,
                   get = {
                     res <- self$data$remoteclient$get(path = paste0("/api/3/action/", action), query = data, ...)
                     list(status_code = res$status_code, result = jsonlite::fromJSON(res$parse(encoding = "UTF-8"), simplifyVector = FALSE)$result)
                   },
                   post = {
                     res <- self$data$remoteclient$post(path = paste0("/api/3/action/", action), body = data, encode = encode, ...)
                     list(status_code = res$status_code, result = jsonlite::fromJSON(res$parse(encoding = "UTF-8"), simplifyVector = FALSE)$result)
                   },
                   put = {
                     res <- self$data$remoteclient$put(path = paste0("/api/3/action/", action), body = data, encode = encode, ...)
                     list(status_code = res$status_code, result = jsonlite::fromJSON(res$parse(encoding = "UTF-8"), simplifyVector = FALSE)$result)
                   },
                   patch = {
                     res <- self$data$remoteclient$patch(path = paste0("/api/3/action/", action), body = data, encode = encode, ...)
                     list(status_code = res$status_code, result = jsonlite::fromJSON(res$parse(encoding = "UTF-8"), simplifyVector = FALSE)$result)
                   })
      res
    },
    read = function() {
      self
    },
    setup = function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE, hdx_config = NULL, configuration = NULL) {
      if (!hdx_site %in% c("prod", "test", "feature", "demo")) stop("hdx_site can be either `prod`, `test`, `feature` or `demo`", call. = FALSE)
      if (!is.null(configuration)) {
        if (!inherits(configuration, "Configuration")) stop("Not a 'Configuration' object!", call. = FALSE)
        private$shared$configuration <- configuration        
      } else {
        private$shared$configuration <- Configuration$new(hdx_site = hdx_site, hdx_key = hdx_key, read_only = read_only, hdx_config = hdx_config)
      }
    },
    delete = function() {
      private$shared$configuration <- NULL
    },
    get_global_config = function() {
      self$data$hdx_config
    },
    general_statistics = function() {
      res <- self$data$remoteclient$get(path = "/api/3/action/hdx_general_statistics")
      jsonlite::fromJSON(res$parse(encoding = "UTF-8"), simplifyVector = TRUE)$result  
    },
    as_list = function() {
      self$data
    },
    print = function() {
      cat("<HDX Configuration> ", sep = "\n")
      cat(paste0("  HDX site: ", self$get_hdx_site()), sep = "\n")
      cat(paste0("  HDX site url: ", self$get_hdx_site_url()), sep = "\n")
      cat(paste0("  HDX API key: ", self$get_hdx_key()), sep = "\n")
      invisible(self)
    }
  )
)


#' @aliases Configuration
Configuration$setup <- function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE, hdx_config = NULL, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    .rhdx_env$configuration <- configuration
  .rhdx_env$configuration <- Configuration$new(hdx_site = hdx_site, hdx_key = hdx_key, read_only = read_only, hdx_config = hdx_config)
}

#' @aliases Configuration
Configuration$delete <- function() {
  configuration <- .rhdx_env$configuration
  if (is.null(configuration) | !inherits(configuration, "Configuration"))
    configuration <- Configuration$new()
  conf$delete()
}
  
#' @aliases Configuration
Configuration$read <- function() {
  configuration <- .rhdx_env$configuration
  if (is.null(configuration) | !inherits(configuration, "Configuration"))
    stop("There is no HDX configuration! Use `set_rhdx_config`", call. = FALSE)
  configuration$read()
}

#' @export
#' @aliases Configuration
set_rhdx_config <- function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE, hdx_config = NULL, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    .rhdx_env$configuration <- configuration
  .rhdx_env$configuration <- Configuration$new(hdx_site = hdx_site, hdx_key = hdx_key, read_only = read_only, hdx_config = hdx_config)
}


#' @export
#' @aliases Configuration
get_rhdx_config <- function() {
  configuration <- .rhdx_env$configuration
  if (is.null(configuration) | !inherits(configuration, "Configuration"))
    stop("There is no HDX configuration! Use `set_rhdx_config`", call. = FALSE)
  configuration$read()
}
