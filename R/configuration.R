#' HDX Configuration
#'
#' HDX Configuration allow to connect to an HDX server and setup project where you can interact with the HDX platform
#'
#' @details
#' **Methods**
#'   \describe{
#'     \item{`create(hdx_site, hdx_key, read_only, hdx_key_file, hdx_config,
#'            hdx_config_yaml, hdx_config_json, project_config, project_config_yaml, project_config_json, configuration)`}{
#'       Create a HDX Configuration
#'     }
#'     \item{`get_credentials()`}{
#'       Setup a HDX Configuration object
#'     }
#'     \item{`set_read_only(read_only)`}{
#'       Read a Configuration object
#'     }
#'
#'     \item{`set_hdx_key(hdx_key)`}{
#'       Provides an API key
#'     }
#'
#'     \item{`load_hdx_key(path)`}{
#'       Provides an API key from path
#'     }
#'
#'     \item{`get_hdx_key()`}{
#'       Get the HDX server site in use.
#'     }
#'
#'     \item{`set_hdx_key(hdx_site)`}{
#'       Set the HDX server site to use.
#'     }
#'
#'    \item{`get_hdx_key()`}{
#'      Get the API key
#'    }
#'
#'    \item{`get_hdx_key_url()`}{
#'      Get the HDX server URL
#'    }
#'
#'   \item{`remoteclient()`}{
#'      Remote API client
#'    }
#'
#'   \item{`call_remoteclient()`}{
#'      Call remote API client
#'    }
#'
#'   \item{`setup()`}{
#'      Setup configuration i.e HDX server, API and read only status.
#'    }
#'
#'   \item{`delete()`}{
#'      Delete actual configuration.
#'    }
#'
#'   \item{`general_statistics()`}{
#'      Some statistics about HDX
#'    }
#'
#' }
#'
#' @format NULL
#' @usage NULL
#'
#' @importFrom yaml read_yaml
#' @importFrom jsonlite read_json
#' @importFrom crul HttpClient
#'
#' @examples
#' \dontrun{
#' set_rhdx_config(hdx_site = "prod")
#' get_rhd_config()
#' }
Configuration <- R6::R6Class(
  "Configuration",
  private = list(shared = .rhdx_env),
  public = list(
    data = list(),

    initialize = function(hdx_site = "prod", hdx_key = NULL,
                          hdx_config = NULL, hdx_config_file = NULL,
                          read_only = TRUE, user_agent = NULL) {
      check_config_params(hdx_site = hdx_site, hdx_key = hdx_key,
                          hdx_config_file = hdx_config_file,
                          read_only = read_only, user_agent = user_agent)

      if (!is.null(hdx_config_file) & !is.null(hdx_config))
        stop("You need to have just one config parameter: `hdx_config_file` or `hdx_config`",
             call. = FALSE)

      if (is.null(hdx_config_file) & is.null(hdx_config))
        hdx_config <- yaml::read_yaml(system.file("config",
                                                  "hdx_base_configuration.yml",
                                                  package = "rhdx"))

      if (!is.null(hdx_config_file) & is.null(hdx_config)) {
        file_ext <- tools::file_ext(hdx_config)
        if (!file_ext %in% c("yml", "json"))
          stop("Only YAML and JSON configuration file are supported for the moment!",
               call. = FALSE)
        hdx_config <- switch(file_ext,
                             yml = yaml::read_yaml(hdx_config_file),
                             json = jsonlite::read_json(hdx_config_file,
                                                        simplifyVector = FALSE))
      }

      self$data$hdx_config <- hdx_config
      self$data$hdx_site <- hdx_site
      hdx_site <- paste0("hdx_", hdx_site, "_site")
      self$data$hdx_key <- hdx_key
      self$data$read_only <- read_only
      headers <- NULL

      if (isFALSE(read_only))
        headers <- list(`X-CKAN-API-Key` = hdx_key)

      if (is.null(user_agent))
        user_agent <- get_user_agent()

      self$data$remoteclient <- crul::HttpClient$new(url = self$data$hdx_config[[hdx_site]]$url,
                                                     headers = headers,
                                                     opts = list(http_version = 2L,
                                                                 useragent = user_agent))
      private$shared$configuration <- self
    },

    get_credentials = function() {
      hdx_site <- paste0("hdx_", self$data$hdx_site, "_site")
      lapply(self$data$hdx_config[[hdx_site]][c("username", "password")],
             function(x) {
               if (is.null(x)) {
                 x
               } else {
                 rawToChar(base64enc::base64decode(x))
               }
             })
    },

    set_read_only = function(read_only = TRUE) {
      hdx_site <- paste0("hdx_", self$data$hdx_site, "_site")
      headers <- NULL

      if (isFALSE(read_only))
        headers <- list(`X-CKAN-API-Key` = self$data$hdx_key)

      self$data$remoteclient <- crul::HttpClient$new(url = self$data$hdx_config[[hdx_site]]$url,
                                                     headers = headers,
                                                     opts = list(http_version = 2,
                                                                 useragent = get_user_agent()))
    },

    set_hdx_key = function(hdx_key) {
      if (!is_valid_uuid(key))
        stop("key not valid!", call. = FALSE)
      self$data$hdx_key <- hdx_key
    },

    get_hdx_key = function() {
      self$data$hdx_key
    },

    load_hdx_key = function(path) {
      key <- readLines(path)
      if (!is_valid_uuid(key))
        stop("key not valid!", call. = FALSE)
      self$data$hdx_key <- key
    },

    set_hdx_site = function(hdx_site = "prod") {
      if (!hdx_site %in% c("prod", "test", "feature", "demo"))
        stop("hdx_site can be either `prod`, `test`, `feature` or `demo`",
             call. = FALSE)
      self$data$hdx_site <-  hdx_site
      hdx_site <- paste0("hdx_", hdx_site, "_site")
      headers <- drop_nulls(list(`X-CKAN-API-Key` = self$data$hdx_key))
      self$data$remoteclient <- crul::HttpClient$new(url = self$data$hdx_config[[hdx_site]]$url,
                                                     headers = headers,
                                                     opts = list(http_version = 2,
                                                                 useragent = get_user_agent()))
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
      cli <- self$data$remoteclient
      res <- switch(verb,
                    get = {
                      res <- cli$get(path = paste0("/api/3/action/", action),
                                     query = data, ...)
                    },
                    post = {
                      res <- cli$post(path = paste0("/api/3/action/", action),
                                      body = data, encode = encode, ...
                      )
                    },
                    put = {
                      res <- cli$put(path = paste0("/api/3/action/", action),
                                     body = data, encode = encode, ...)
                    },
                    patch = {
                      res <- cli$patch(path = paste0("/api/3/action/", action),
                                       body = data, encode = encode, ...)
                    })

      status_code <- res$status_code
      result <- jsonlite::fromJSON(res$parse(encoding = "UTF-8"), simplifyVector = FALSE)$result
      error <- jsonlite::fromJSON(res$parse(encoding = "UTF-8"), simplifyVector = FALSE)$error
      drop_nulls(list(status_code = status_code, error = error, result = result))
    },

    read = function() {
      self
    },

    setup = function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE,
                     hdx_config = NULL, configuration = NULL) {
      if (!hdx_site %in% c("prod", "test", "feature", "demo"))
        stop("hdx_site can be either `prod`, `test`, `feature` or `demo`", call. = FALSE)
      if (!is.null(configuration)) {
        if (!inherits(configuration, "Configuration"))
          stop("Not a 'Configuration' object!", call. = FALSE)
        private$shared$configuration <- configuration
      } else {
        private$shared$configuration <- Configuration$new(hdx_site = hdx_site, hdx_key = hdx_key,
                                                          read_only = read_only, hdx_config = hdx_config)
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
      jsonlite::fromJSON(res$parse(encoding = "UTF-8"),
                         simplifyVector = TRUE)$result
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


#' Read configuration
#'
#' Sets the configuration settings for using rhdx.
#'
configuration_read <- function() {
  configuration <- .rhdx_env$configuration
  assert_configuration(configuration)
  configuration$read()
}


#' Set rhdx config
#'
#' Sets the configuration settings for using rhdx.
#'
#' @param hdx_site Character to specify which HDX server you want to use. Default to "prod".
#' @param hdx_key Character for the CKAN API key, it is required to push data into HDX
#' @param hdx_config List of HDX configuration
#' @param hdx_config_file Character, path of the HDX config file in JSON and YAML format
#' @param read_only Logical if `FALSE` and hdx_key provided is correct you can push metdata and data to HDX
#' @param configuration Configuration object.
#' @param ... Extra parameters
#'
#' @rdname set_rhdx_config
#'
#' @details Setting up a configuration will help you access and push data into HDX
#'
#'
#' @return Invisibly returns the rhdx config object
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#' set_rhdx_config(hdx_site = "prod")
#'
#' # You can check your configuration using \code{get_rhdx_config}
#' config <- get_rhdx_config()
#' config
#' }
set_rhdx_config <- function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE, hdx_config = NULL, hdx_config_file = NULL, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    .rhdx_env$configuration <- configuration
  .rhdx_env$configuration <- Configuration$new(hdx_site = hdx_site, hdx_key = hdx_key, read_only = read_only, hdx_config = hdx_config, hdx_config_file = hdx_config_file)
}


#' @rdname set_rhdx_config
#' @export
get_rhdx_config <- function() {
  configuration <- .rhdx_env$configuration
  assert_configuration(configuration)
  configuration$read()
}


#' Delete rhdx config
#'
#' Delete the configuration settings for using rhdx.
#'
#' @rdname delete_rhdx_config
#'
#' @details Delete HDX config
#'
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#' set_rhdx_config(hdx_site = "prod")
#' get_rhdx_config()
#'
#' delete_rhdx_config()
#' get_rhdx_config()
#' }
delete_rhdx_config <- function() {
  configuration <- .rhdx_env$configuration
  assert_configuration(configuration)
  configuration$delete()
}


#' Get general stats about HDX
#'
#' Get some stats about HDX
#'
#' @return A list
#' @export
hdx_general_statistics <- function() {
  configuration <- .rhdx_env$configuration
  assert_configuration(configuration)
  configuration$general_statistics()
}