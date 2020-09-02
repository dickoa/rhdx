#' HDX Configuration
#'
#' HDX Configuration allow to connect to an HDX server
#' and setup project where you can interact with the HDX platform
#'
#' @format NULL
#' @usage NULL
#'
#' @importFrom tools file_ext
#' @importFrom yaml read_yaml
#' @importFrom jsonlite fromJSON
#' @importFrom crul HttpClient
#'
#' @examples
#' \dontrun{
#' set_rhdx_config(hdx_site = "prod")
#' get_rhd_config()
#' }
Configuration <- R6::R6Class(
  classname = "Configuration",
  private = list(shared = .rhdx_env),
  public = list(
    #' @field data all info in list.
    data = list(),

    #' @description
    #' Create a new Configuration object.
    #'
    #' @importFrom tools file_ext
    #' @importFrom yaml read_yaml
    #' @importFrom jsonlite read_json
    #' @importFrom crul HttpClient
    #'
    #' @param hdx_site character the server instance to use
    #' @param hdx_key character, the HDX API key
    #' @param hdx_config configuration in a list
    #' @param hdx_config_file a character value config file.
    #' default is the config supplied in the package
    #' @param read_only a logical value indicating if you want to just read
    #' or be also able to write on the HDX server. You will need a API key to write.
    #' @param user_agent a character value, User agent
    #' @return A new Configuration object.
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
        hdx_config <- read_yaml(system.file("config",
                                            "hdx_base_configuration.yml",
                                            package = "rhdx"))

      if (!is.null(hdx_config_file) & is.null(hdx_config)) {
        file_ext <- file_ext(hdx_config)
        if (!file_ext %in% c("yml", "json"))
          stop("Only YAML and JSON configuration file are supported for the moment!",
               call. = FALSE)
        hdx_config <- switch(file_ext,
                             yml = read_yaml(hdx_config_file),
                             json = read_json(hdx_config_file,
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

      self$data$remoteclient <- HttpClient$new(url = self$data$hdx_config[[hdx_site]]$url,
                                               headers = headers,
                                               opts = list(http_version = 2L,
                                                           useragent = user_agent))
    },

    #' @description
    #' Configuration credentials when using a HDX API key
    #' @importFrom base64enc base64decode
    #' @return the username and password associated to the HDX API key
    get_credentials = function() {
      hdx_site <- paste0("hdx_", self$data$hdx_site, "_site")
      lapply(self$data$hdx_config[[hdx_site]][c("username", "password")],
             function(x) {
               if (is.null(x)) {
                 x
               } else {
                 rawToChar(base64decode(x))
               }
             })
    },

    #' @description
    #' Create or revoke read only status
    #'
    #' @param read_only a logical value indicating if you want to just read or be also able to write on the HDX server. You will need a API key to write.
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

    #' @description
    #' Specify a HDX API key
    #'
    #' @param hdx_key a character with key
    set_hdx_key = function(hdx_key) {
      if (!is_valid_uuid(key))
        stop("key not valid!", call. = FALSE)
      self$data$hdx_key <- hdx_key
    },

    #' @description
    #' Specify a HDX API key
    #'
    #' @return a character, the HDX API key
    get_hdx_key = function() {
      self$data$hdx_key
    },

    #' @description
    #' Specify a HDX server to use
    #'
    #' @importFrom crul HttpClient
    #' @param hdx_site a character, the server type to use,
    #' `prod`, `test`, `feature` or `demo`
    #' @return a character, the HDX API key
    set_hdx_site = function(hdx_site = "prod") {
      if (!hdx_site %in% c("prod", "demo", "stage", "feature", "dev"))
        stop("hdx_site can be either `prod`, `demo`, `stage`, `feature`, or `dev`",
             call. = FALSE)
      self$data$hdx_site <-  hdx_site
      hdx_site <- paste0("hdx_", hdx_site, "_site")
      headers <- drop_nulls(list(`X-CKAN-API-Key` = self$data$hdx_key))
      self$data$remoteclient <- HttpClient$new(url = self$data$hdx_config[[hdx_site]]$url,
                                               headers = headers,
                                               opts = list(http_version = 2,
                                                           useragent = get_user_agent()))
    },

    #' @description
    #' Get the HDX server in use
    #' @return the server type
    get_hdx_site = function() {
      self$data$hdx_site
    },

    #' @description
    #' Get the HDX server URL in use
    #' @return the server URL
    get_hdx_site_url = function() {
      hdx_site <- paste0("hdx_", self$data$hdx_site, "_site")
      self$data$hdx_config[[hdx_site]]$url
    },

    #' @description
    #' Get the remoteclient currently used
    #' @return a crul::HttpClient
    remoteclient = function() {
      self$data$remoteclient
    },

    #' @description
    #' Call the client to the HDX API
    #'
    #' @param action a character
    #' @param ... parameters for each verb used
    #' @param verb a character the verb used, `post`, `get`, `put` or `patch`
    #' @return list a with status code and results
    call_action = function(action, ..., verb = "get") {
      if (!verb %in% c("post", "get", "put", "patch"))
        stop("Only `get`, `post`, `put` and `patch` are supported!")
      cli <- self$data$remoteclient
      action_path <- paste0("/api/3/action/", action)
      res <- cli$verb(verb, path = action_path, ...)
      parse_response(res)
    },

    #' @description
    #' read and show Configuration object
    #' @return Configuration object
    read = function() {
      self
    },

    #' @description
    #' Setup Configuration object
    #'
    #' @param hdx_site a character value, the server
    #' @param hdx_config a list
    #' @param configuration a character
    #' @param hdx_key a character value, the API key
    #' @param read_only a logical value read only
    setup = function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE,
                     hdx_config = NULL, configuration = NULL) {
      if (!hdx_site %in% c("prod", "demo", "stage", "feature", "dev"))
        stop("hdx_site can be either `prod`, `demo`, `stage`, `feature`, or `dev`",
             call. = FALSE)
      if (!is.null(configuration)) {
        if (!inherits(configuration, "Configuration"))
          stop("Not a 'Configuration' object!", call. = FALSE)
        private$shared$configuration <- configuration
      } else {
        private$shared$configuration <- Configuration$new(hdx_site = hdx_site,
                                                          hdx_key = hdx_key,
                                                          read_only = read_only,
                                                          hdx_config = hdx_config)
      }
    },

    #' @description
    #' Delete a Configuration object
    delete = function() {
      private$shared$configuration <- NULL
    },

    #' Access the global Configuration
    #' @return list with HDX configuration information
    get_global_config = function() {
      self$data$hdx_config
    },

    #' @description
    #'
    #' Get general  statistics about the server
    #'
    #' @importFrom jsonlite read_json
    #'
    #' @return list with statistics about the server
    general_statistics = function() {
      res <- self$data$remoteclient$get(path = "/api/3/action/hdx_general_statistics")
      read_json(res$parse(encoding = "UTF-8"),
                simplifyVector = TRUE)$result
    },

    #' @description
    #' Convert configuration to list
    #' @return configuration in list format
    as_list = function() {
      self$data
    },

    #' @description
    #' Print Configuration object
    print = function() {
      cat("<HDX Configuration> ", sep = "\n")
      cat(paste0("  HDX site: ", self$get_hdx_site()), sep = "\n")
      cat(paste0("  HDX site url: ", self$get_hdx_site_url()), sep = "\n")
      cat(paste0("  HDX API key: ", self$get_hdx_key()), sep = "\n")
      invisible(self)
    }
  )
)

#' Create an HDX configuration object
#'
#' Create and HDX configuration object
#' @param hdx_site Character to specify which HDX server you want to use. Default to "prod".
#' @param hdx_key Character for the CKAN API key, it is required to push data into HDX
#' @param hdx_config List of HDX configuration
#' @param hdx_config_file Character, path of the HDX config file in JSON and YAML format
#' @param read_only Logical if `FALSE` and hdx_key provided is correct
#' you can push metdata and data to HDX
#' @return An HDX Configuration object
#' @export
create_rhdx_config <- function(hdx_site = "prod", hdx_key = NULL,
                               read_only = TRUE, hdx_config = NULL, hdx_config_file = NULL) {
  Configuration$new(hdx_site = hdx_site, hdx_key = hdx_key,
                    read_only = read_only, hdx_config = hdx_config,
                    hdx_config_file = hdx_config_file)
}

#' Set rhdx config
#'
#' Sets the configuration settings for using rhdx.
#'
#' @param hdx_site Character to specify which HDX server you want to use. Default to "prod".
#' @param hdx_key Character for the CKAN API key, it is required to push data into HDX
#' @param hdx_config List of HDX configuration
#' @param hdx_config_file Character, path of the HDX config file in JSON and YAML format
#' @param read_only Logical if `FALSE` and hdx_key provided is correct
#' you can push metdata and data to HDX
#' @param configuration Configuration object.
#'
#' @rdname set_rhdx_config
#'
#' @details Setting up a configuration will help you access from an HDX server
#'
#'
#' @return Invisibly returns the rhdx config object
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#' set_rhdx_config(hdx_site = "demo")
#'
#' # You can check your configuration using \code{get_rhdx_config}
#' config <- get_rhdx_config()
#' config
#' }
set_rhdx_config <- function(hdx_site = "prod", hdx_key = NULL, read_only = TRUE, hdx_config = NULL, hdx_config_file = NULL, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "Configuration")) {
    .rhdx_env$configuration <- configuration
  } else {
    .rhdx_env$configuration <- Configuration$new(hdx_site = hdx_site,
                                                 hdx_key = hdx_key,
                                                 read_only = read_only,
                                                 hdx_config = hdx_config,
                                                 hdx_config_file = hdx_config_file)
  }
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
  configuration <- get_rhdx_config()
  configuration$delete()
}


#' Get general stats about HDX
#'
#' Get some stats about HDX
#'
#' @return A list
#' @export
hdx_general_statistics <- function() {
  configuration <- get_rhdx_config()
  configuration$general_statistics()
}
